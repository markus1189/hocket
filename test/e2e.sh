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

# nav KEY ID: press KEY, then ID must be selected.
nav() {
  key "$1"
  wait_for 5 "$1 to select $2" state_is ".selected == \"$2\""
}

scenarios=(all-three archive-503 reminder-404 empty-fetch key-a filter-typing navigation)
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
