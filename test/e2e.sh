#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#coreutils nixpkgs#tmux nixpkgs#jq nixpkgs#diffutils --command bash
# shellcheck shell=bash
# End-to-end runs of `hocket tui --fixture`, driven over the agent socket
# (plan §6 Phase 2.4). Never touches the real account.
#
#   test/e2e.sh [SCENARIO...]        all scenarios when none is named
#   HOCKET=/path/to/hocket test/e2e.sh   skip the nix build
#
# Each scenario starts one hocket in its own pane of a private tmux server,
# compares the dumped store (server side) and the flags + status line (local
# side) with test/e2e/expected-NAME{,.local}.json, and requires a clean exit.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
repo=$(dirname "$here")
hocket=${HOCKET:-$(nix build --no-link --print-out-paths "$repo")/bin/hocket}
# Short on purpose: a socket path must stay under 108 bytes.
tmp=$(mktemp -d /tmp/hocket-e2e.XXXXXX)

# A private server without the user's config (remain-on-exit, hooks, ...);
# remain-on-exit is set here so a crashed hocket's exit status can be read.
tm() { tmux -L "hocket-e2e-$$" -f /dev/null "$@"; }
server_socket=''
cleanup() {
  tm kill-server 2>/dev/null || true
  # kill-server leaves the server's socket file behind.
  [ -z "$server_socket" ] || rm -f "$server_socket"
  rm -r "$tmp"
}
trap cleanup EXIT
tm new-session -d -s keepalive 'sleep 100000'
# Read once, while the server surely runs: cleanup may find it gone.
server_socket=$(tm display-message -p -t keepalive '#{socket_path}')
tm set-option -g remain-on-exit on

name='' sock='' dump='' desktop=''
failed=()

fail() {
  echo "FAIL [$name]: $*" >&2
  echo "--- screen ---" >&2
  tm capture-pane -p -t "$name" >&2 || true
  return 1
}

agent() { "$hocket" agent --socket-path "$sock" "$@"; }

# wait_for SECONDS WHAT CMD...: retry CMD until it succeeds.
wait_for() {
  local secs=$1 what=$2 deadline=$((SECONDS + $1))
  shift 2
  until "$@" >/dev/null 2>&1; do
    ((SECONDS < deadline)) || {
      fail "timed out after ${secs}s waiting for $what"
      return 1
    }
    sleep 0.2
  done
}

state_is() { agent get_state | jq -e ".result | $1"; }
action_is() { agent get_item "$1" | jq -e ".result.pending.action == \"$2\""; }

# Writes from one client are injected in order, so once a later write shows
# in get_state, every earlier one has been handled. The marker must not touch
# what the expected files compare: a status marker could overwrite a batch's
# final status, or be overwritten by FetchCompleted before it is seen. So
# show_future_reminders goes false -> true -> false, each step awaited: the
# setting ends unchanged, and showing more items never moves the selection.
# A scenario that turns it on asserts with wait_for until it is off again.
settle() {
  state_is '.show_future_reminders == false' >/dev/null || {
    fail "settle needs show_future_reminders off; the scenario left it on"
    return 1
  }
  agent set_show_future_reminders --show >/dev/null &&
    wait_for 10 "the marker" state_is '.show_future_reminders == true' &&
    agent set_show_future_reminders --hide >/dev/null &&
    wait_for 10 "the marker's reset" state_is '.show_future_reminders == false' &&
    wait_for 20 "the async slot to be free" state_is '.async_op == null'
}

start() { # NAME SEED [--desktop] [ARGS...]
  name=$1
  local seed=$2 dir=$tmp/$1
  shift 2
  # --desktop: log what the browser and clipboard would have done, and let
  # key() send the keys that do it.
  desktop=''
  if [ "${1-}" = --desktop ]; then
    shift
    desktop=$dir/desktop.jsonl
    set -- --fixture-desktop-log "$desktop" "$@"
  fi
  # The socket gets a directory of its own: hocket removes the socket's
  # directory on exit (plan B7).
  mkdir -p "$dir/home" "$dir/config" "$dir/state" "$dir/runtime" "$dir/cwd" "$dir/sock"
  chmod 700 "$dir/runtime"
  sock=$dir/sock/control.sock dump=$dir/dump.json
  # Nothing of the user's: an empty environment, home, XDG dirs and cwd (for
  # the legacy ./config.dhall), so a --fixture that reads config again fails
  # instead of reaching the real account. TERM and TERMINFO_DIRS stay for
  # vty, which finds no terminfo on NixOS without them.
  tm new-session -d -s "$name" -x 200 -y 50 -c "$dir/cwd" \
    "env -i TERM=\"\$TERM\" TERMINFO_DIRS=$(printf '%q' "${TERMINFO_DIRS-}") LANG=C.UTF-8 LOCALE_ARCHIVE=$(printf '%q' "${LOCALE_ARCHIVE-}") $(printf '%q ' \
      HOME="$dir/home" XDG_CONFIG_HOME="$dir/config" XDG_STATE_HOME="$dir/state" XDG_RUNTIME_DIR="$dir/runtime" \
      "$hocket" tui --fixture "$here/e2e/$seed" --fixture-dump "$dump" --agent-socket-path "$sock" "$@")"
  wait_for 10 "the agent socket" test -S "$sock" && settle
}

# Enter, Space, e and y reach the browser or clipboard; fixture mode only
# records them, and a scenario sends them only when it checks that record.
key() {
  case "$1" in
  Enter | Space | ' ' | e | y | C-m | C-j)
    [ -n "$desktop" ] || {
      fail "refusing to send '$1' without --desktop"
      return 1
    }
    ;;
  esac
  tm send-keys -t "$name" "$1"
}

# filter_lock TEXT: type TEXT into the filter editor and lock it with Enter,
# which in the editor only keeps the query.
filter_lock() {
  key / && tm send-keys -t "$name" -l "$1" && tm send-keys -t "$name" Enter &&
    wait_for 5 "the filter \"$1\"" state_is ".filter_query == \"$1\""
}

# Always quit with Q: a killed hocket skips its exit path (and the dump).
stop_tui() {
  pane_dead && return 0
  # Escape first: should the TUI be in filter mode, Q would be typed into it.
  key Escape && key Q
  wait_for 10 "hocket to exit on Q" pane_dead
}

pane_dead() { [ "$(tm display-message -p -t "$name" '#{pane_dead}')" = 1 ]; }

# Snapshot the local side, quit, require a clean exit, compare both sides.
# The scenario has settled after its last action.
finish() {
  local dir=$tmp/$name local_json
  local_json=$(jq -n -S \
    --argjson flagged "$(agent list_items --all --flagged-only | jq '.result | map({id, action: .pending.action}) | sort_by(.id)')" \
    --argjson status "$(agent get_state | jq '.result.status')" \
    '{flagged: $flagged, status: $status}')
  stop_tui || return 1
  local status
  status=$(tm display-message -p -t "$name" '#{pane_dead_status}')
  [ "$status" = 0 ] || {
    fail "hocket exited with status $status"
    return 1
  }
  [ -f "$dump" ] || {
    fail "no dump at $dump"
    return 1
  }
  local ok=0
  jq -S '.items | map({id: (._id | tostring), collection: .collection["$id"], has_reminder: (.reminder != null)}) | sort_by(.id)' "$dump" >"$dir/server.json"
  echo "$local_json" >"$dir/local.json"
  diff -u <(jq -S . "$here/e2e/expected-$name.json") "$dir/server.json" >&2 || ok=1
  diff -u <(jq -S . "$here/e2e/expected-$name.local.json") "$dir/local.json" >&2 || ok=1
  if [ -n "$desktop" ]; then
    diff -u "$here/e2e/expected-$name.desktop.jsonl" <(cat "$desktop" 2>/dev/null) >&2 || ok=1
  fi
  ((ok == 0)) || fail "dump, local state or desktop log differs from the expected files (diffs above)"
}

flag() { agent set_flag "$1" --action "$2" >/dev/null; }

scenario_all_three() {
  start all-three seed-basic.json || return 1
  flag 1001 archive && flag 1002 reminder && flag 1003 remove_reminder || return 1
  agent execute >/dev/null && settle || return 1
  finish
}

# D6 global error: the archive step aborts, the reminder step is skipped.
scenario_archive_503() {
  start archive-503 seed-basic.json --fixture-fail archive=503 || return 1
  flag 1001 archive && flag 1002 reminder || return 1
  agent execute >/dev/null && settle || return 1
  finish
}

# D6 item-scoped error (B5): reminders go out in id order, so call 2 is 1002.
scenario_reminder_404() {
  start reminder-404 seed-basic.json --fixture-fail set-reminders:2=404 || return 1
  flag 1001 reminder && flag 1002 reminder && flag 1004 reminder || return 1
  agent execute >/dev/null && settle || return 1
  finish
}

# B1: an empty fetch must free the slot, at startup and on every refresh.
scenario_empty_fetch() {
  start empty-fetch seed-empty.json || return 1
  agent refresh >/dev/null && settle || return 1
  agent refresh >/dev/null && settle || return 1
  finish
}

# B4 through the real key: `a` on a reminder-flagged item archives it.
scenario_key_a() {
  start key-a seed-basic.json || return 1
  flag 1002 reminder && agent select_item 1002 >/dev/null || return 1
  wait_for 5 "1002 to be selected" state_is '.selected == "1002"' || return 1
  key a
  wait_for 5 "1002 to be flagged for archive" action_is 1002 archive || return 1
  agent execute >/dev/null && settle || return 1
  finish
}

# D3 through the keys: the editor reports what it holds, and keeps its
# cursor while the query agrees; Q in the editor is text, not quit.
scenario_filter_typing() {
  start filter-typing seed-basic.json || return 1
  key / && tm send-keys -t "$name" -l abc && key Left && key Left && key X || return 1
  wait_for 5 'the filter "aXbc"' state_is '.filter_query == "aXbc"' || return 1
  tm send-keys -t "$name" -l Q
  wait_for 5 'the Q after the X' state_is '.filter_query == "aXQbc"' || return 1
  ! pane_dead || {
    fail "Q in the filter editor quit hocket"
    return 1
  }
  key Escape
  wait_for 5 "Esc to clear the filter" state_is '.filter_query == ""' || return 1
  finish
}

# D2 through the keys: the shell passes the list's height, so a page jumps,
# and Down in the filter editor moves the selection.
scenario_navigation() {
  start navigation seed-basic.json || return 1
  local ids
  mapfile -t ids < <(agent list_items | jq -r '.result[].id')
  ((${#ids[@]} >= 3)) || {
    fail "want 3 visible items, got ${#ids[@]}"
    return 1
  }
  local last=${ids[-1]}
  # g first: the marker may have shown an item above the selection, which
  # stays on its item.
  nav g "${ids[0]}" && nav j "${ids[1]}" && nav j "${ids[2]}" && nav G "$last" && nav g "${ids[0]}" &&
    nav NPage "$last" && nav g "${ids[0]}" && key / && nav Down "${ids[1]}" || return 1
  key Escape
  finish
}

# The agent's flags: set_flag is idempotent, none clears, the bulk writes
# reach every item, and execute archives only what is flagged then.
scenario_agent_flags() {
  start agent-flags seed-basic.json || return 1
  flag 1001 archive && flag 1001 archive || return 1
  wait_for 5 "one archive flag after two set_flag" state_is '.counts.archive_flagged == 1' || return 1
  flag 1002 reminder && wait_for 5 "1002 flagged for a reminder" action_is 1002 reminder || return 1
  flag 1002 none && wait_for 5 "1002 unflagged" action_is 1002 none || return 1
  agent flag_all_archive >/dev/null && wait_for 5 "all four flagged" state_is '.counts.archive_flagged == 4' || return 1
  agent clear_all_flags >/dev/null && wait_for 5 "no flags" state_is '.counts.archive_flagged == 0 and .counts.reminder_flagged == 0 and .counts.remove_reminder_flagged == 0' || return 1
  flag 1004 archive && agent execute >/dev/null && settle || return 1
  finish
}

# The agent's view writes: filter, video filter, future reminders, select,
# status. visible counts the items the list shows.
scenario_agent_view() {
  start agent-view seed-mixed.json || return 1
  agent set_filter 1004 >/dev/null && wait_for 5 "the filter to leave 1004" state_is '.counts.visible == 1' || return 1
  agent set_filter "" >/dev/null && wait_for 5 "all but the reminder" state_is '.counts.visible == 5' || return 1
  agent set_video_filter only_videos >/dev/null && wait_for 5 "only the two videos" state_is '.counts.visible == 2' || return 1
  agent set_video_filter hide_videos >/dev/null && wait_for 5 "the videos hidden" state_is '.counts.visible == 3' || return 1
  agent set_video_filter none >/dev/null && wait_for 5 "no video filter" state_is '.counts.visible == 5' || return 1
  # From here on settle would refuse: show_future_reminders stays on.
  agent set_show_future_reminders --show >/dev/null && wait_for 5 "the reminder shown" state_is '.counts.visible == 6' || return 1
  agent select_item 1003 >/dev/null && wait_for 5 "1003 selected" state_is '.selected == "1003"' || return 1
  agent set_status hello >/dev/null && wait_for 5 "the status" state_is '.status == "agent: hello"' || return 1
  finish
}

# rejected CMD...: the agent call must fail.
rejected() {
  if agent "$@" >/dev/null 2>&1; then
    fail "agent $* was accepted"
    return 1
  fi
}

# view: what a rejected write must leave alone (the version counts every
# event, agent connections included, so it is not compared).
view() { jq -n -S --argjson s "$(agent get_state | jq '.result | del(.version)')" --argjson i "$(agent list_items --all | jq .result)" '{s: $s, i: $i}'; }

# Rejected writes change nothing; wait_version wakes on a later write.
scenario_agent_errors() {
  start agent-errors seed-basic.json || return 1
  local before after v
  before=$(view)
  rejected set_flag 9999 --action archive || return 1
  rejected select_item 1003 || return 1 # hidden: a future reminder
  rejected set_flag 1001 --action remove_reminder || return 1
  rejected set_flag 1003 --action reminder || return 1
  after=$(view)
  [ "$before" = "$after" ] || {
    fail "a rejected write changed the state"
    diff -u <(echo "$before") <(echo "$after") >&2
    return 1
  }
  v=$(agent get_state | jq '.result.version')
  agent set_status checked >/dev/null || return 1
  agent wait_version --after "$v" --timeout-ms 5000 >/dev/null || {
    fail "wait_version --after $v did not return after a write"
    return 1
  }
  wait_for 5 "the status" state_is '.status == "agent: checked"' || return 1
  finish
}

# on ID KEY: select ID, then press KEY on it.
on() {
  agent select_item "$1" >/dev/null && wait_for 5 "$1 to be selected" state_is ".selected == \"$1\"" && key "$2"
}

# The flag keys: s and u, J/K between flagged items, U, X with nothing
# flagged, a + X, and r as an incremental refresh over a full store.
scenario_keys_flags() {
  start keys-flags seed-basic.json || return 1
  on 1001 s && wait_for 5 "s to flag 1001" action_is 1001 reminder || return 1
  on 1001 u && wait_for 5 "u to unflag 1001" action_is 1001 none || return 1
  flag 1002 archive && flag 1004 reminder || return 1
  nav g 1001 && nav J 1002 && nav J 1004 && nav K 1002 || return 1
  key U && wait_for 5 "U to clear every flag" state_is '.counts.archive_flagged == 0 and .counts.reminder_flagged == 0' || return 1
  key X && settle || return 1
  state_is '.async_op == null and .status == null' >/dev/null || {
    fail "X with nothing flagged started something"
    return 1
  }
  on 1004 a && wait_for 5 "a to flag 1004" action_is 1004 archive || return 1
  key X && settle || return 1
  local before
  before=$(agent get_state | jq '.result.last_updated')
  key r && settle || return 1
  wait_for 5 "the refresh to see the archived 1004" state_is ".last_updated > $before and .counts.total == 3" || return 1
  finish
}

# The view keys, then navigation against what the list shows: with five
# items in a 50-row pane every page move reaches an end.
scenario_keys_view() {
  start keys-view seed-mixed.json || return 1
  # Every transition of both keys, V from no filter first, its common use.
  key V && wait_for 5 "V from none: videos hidden" state_is '.counts.visible == 3' || return 1
  key v && wait_for 5 "v from hidden: only videos" state_is '.counts.visible == 2' || return 1
  key v && wait_for 5 "v from only: no filter" state_is '.counts.visible == 5' || return 1
  key V && wait_for 5 "V from none: videos hidden" state_is '.counts.visible == 3' || return 1
  key V && wait_for 5 "V from hidden: no filter" state_is '.counts.visible == 5' || return 1
  key v && wait_for 5 "v from none: only videos" state_is '.counts.visible == 2' || return 1
  key V && wait_for 5 "V from only: videos hidden" state_is '.counts.visible == 3' || return 1
  key V && wait_for 5 "V from hidden: no filter" state_is '.counts.visible == 5' || return 1
  key S && wait_for 5 "S: the reminder shown" state_is '.counts.visible == 6' || return 1
  key S && wait_for 5 "S: the reminder hidden" state_is '.counts.visible == 5' || return 1
  local ids
  mapfile -t ids < <(agent list_items | jq -r '.result[].id')
  [ "${ids[*]}" = "1001 1002 1004 1005 1006" ] || {
    fail "unexpected list: ${ids[*]}"
    return 1
  }
  nav g 1001 && nav End 1006 && nav k 1005 && nav Up 1004 && nav Home 1001 &&
    nav C-f 1006 && nav C-b 1001 && nav C-d 1006 && nav C-u 1001 && nav End 1006 && nav PPage 1001 || return 1
  finish
}

# The filter editor: C-w deletes a word, Esc leaves and clears, and a
# locked query filters; finish then quits with Q.
scenario_filter_editor() {
  start filter-editor seed-basic.json || return 1
  key / && tm send-keys -t "$name" -l "e2e item" || return 1
  wait_for 5 'the query "e2e item"' state_is '.filter_query == "e2e item"' || return 1
  key C-w && wait_for 5 'C-w to leave "e2e "' state_is '.filter_query == "e2e "' || return 1
  key C-w && wait_for 5 "C-w to clear the query" state_is '.filter_query == ""' || return 1
  key Escape && filter_lock 1002 || return 1
  wait_for 5 "only 1002 visible" state_is '.counts.visible == 1' || return 1
  finish
}

# A failed startup fetch frees the slot and says so; r then fetches.
scenario_fetch_fail() {
  start fetch-fail seed-basic.json --fixture-fail fetch:1=503 || return 1
  state_is '.counts.total == 0 and .async_op == null and (.status | startswith("failed"))' >/dev/null || {
    fail "the failed startup fetch left $(agent get_state | jq -c '.result | {total: .counts.total, async_op, status}')"
    return 1
  }
  key r && settle || return 1
  wait_for 5 "r to fetch the four items" state_is '.counts.total == 4 and .status == null' || return 1
  finish
}

# logged N: the desktop record has N calls. The count is a function, not a
# $(...) argument, so wait_for re-reads the file on every try.
desktop_calls_are() { [ "$(wc -l <"$desktop" 2>/dev/null || echo 0)" -eq "$1" ]; }
logged() { wait_for 5 "$1 desktop calls" desktop_calls_are "$1"; }

# What the browser and clipboard would have done, in order: Enter, Space,
# e, y, and the agent's open_item and open_and_flag.
scenario_desktop() {
  start desktop seed-basic.json --desktop || return 1
  on 1001 Enter && logged 1 && wait_for 5 "Enter to flag 1001" action_is 1001 archive || return 1
  on 1002 Space && logged 2 || return 1
  on 1002 e && logged 3 || return 1
  on 1002 y && logged 4 && wait_for 5 "the copy in the status" state_is '.status == "Copied: https://example.com/1002"' || return 1
  agent open_item 1004 >/dev/null && logged 5 || return 1
  agent open_and_flag 1004 >/dev/null && logged 6 && wait_for 5 "1004 flagged" action_is 1004 archive || return 1
  finish
}

# nav KEY ID: press KEY, then ID must be selected.
nav() {
  key "$1"
  wait_for 5 "$1 to select $2" state_is ".selected == \"$2\""
}

scenarios=(all-three archive-503 reminder-404 empty-fetch key-a filter-typing navigation agent-flags agent-view agent-errors keys-flags keys-view filter-editor fetch-fail desktop)
(($# == 0)) || scenarios=("$@")
for s in "${scenarios[@]}"; do
  if "scenario_${s//-/_}"; then
    echo "ok   $s"
  else
    echo "FAIL $s"
    failed+=("$s")
    stop_tui || echo "  [$s] did not quit on Q; the exit trap's kill-server ends it" >&2
  fi
done
((${#failed[@]} == 0)) || {
  echo "failed: ${failed[*]}" >&2
  exit 1
}
