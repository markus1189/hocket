---
name: hocket-rpc
description: Observe and drive a running hocket TUI over its agent control socket. Use when asked to look at, filter, flag, archive, or set reminders on the user's Raindrop bookmarks in hocket; to read what hocket is currently showing; to stage a batch and execute it; or to watch hocket for changes. Triggers include "what's in hocket", "flag these for archive", "set a reminder on that bookmark", "filter hocket for X", "execute the batch", "hocket control socket", "hocket agent".
allowed-tools: Bash(hocket agent:*)
---

# hocket agent control socket

Hocket is a TUI bookmark manager for Raindrop.io. When it runs with
`--agent-socket`, it exposes a Unix-domain socket so you can observe and
drive **the same live session the user is looking at**.

Three facts that shape everything below:

- **Reads never block the UI.** They are answered from a snapshot mirror
  rebuilt after every render, so the mirror is at worst one frame stale.
- **Writes are keystrokes.** A write is transcribed into the exact event the
  matching keybinding would fire. You cannot reach a state a human at the
  keyboard could not, and you cannot touch the Raindrop token, the shell, or
  the filesystem through this socket.
- **One response per request, no push.** There is no subscription. Long-poll
  with `wait_version`.

## Preflight

The user must have started the TUI with the socket enabled:

```bash
hocket tui --agent-socket           # default path
hocket tui --agent-socket-path PATH # explicit path
```

Default path: `$XDG_RUNTIME_DIR/hocket/control.sock`, falling back to
`/tmp/hocket-<uid>/control.sock`. The socket file is `0600` — same user
only.

Liveness check, and your first command in any session:

```bash
hocket agent get_state
```

Every invocation prints **one line of JSON** on stdout (pipe it to `jq`) and
exits:

| Exit | Meaning |
|---|---|
| 0 | `"ok": true` — request accepted |
| 1 | `"ok": false` — rejected; read `.error` |
| 2 | Socket unreachable; the TUI is not running with `--agent-socket` |

Add `--socket-path PATH` before the method for a non-default socket.

## Methods

Reads — served from the mirror, never block the UI:

| Command | Wire method | Notes |
|---|---|---|
| `get_state` | `get_state` | Counts, selection, filter, status, `version` |
| `list_items [--all] [--flagged-only]` | `list_items` | Visible items only unless `--all`; `--flagged-only` narrows to staged items |
| `get_item ID` | `get_item` | Works for hidden items too (`"visible": false`) |
| `wait_version --after N [--timeout-ms MS]` | `wait_version` | Blocks until `version > N`; default 10000ms, clamped to `[0, 60000]` |

Writes — injected as UI events:

| Command | Wire method | Equivalent key |
|---|---|---|
| `set_flag ID --action archive\|reminder\|remove_reminder\|none` | `set_flag` | `a` / `s` |
| `clear_all_flags` | `clear_all_flags` | `u` |
| `flag_all_archive` | `flag_all_archive` | bulk flag |
| `execute` | `execute` | `X` |
| `refresh` | `refresh` | `r` |
| `set_filter QUERY` | `set_filter` | live filter |
| `set_video_filter none\|only_videos\|hide_videos` | `set_video_filter` | |
| `set_show_future_reminders --show\|--hide` | `set_show_future_reminders` | |
| `select_item ID` | `select_item` | moves the user's cursor |
| `open_item ID` | `open_item` | opens a browser |
| `set_status TEXT` | `set_status` | writes `agent: TEXT` to the status bar |

An item carries: `id`, `title`, `link`, `tags`, `note`, `excerpt`,
`created`, `reminder`, `favorite`, `pending`, `visible`. `pending` is
`{"action":"none"|"archive"|"remove_reminder"}` or
`{"action":"reminder","at":"<ISO-8601>"}`.

## Recipes

**Watch for changes.** `version` is a render counter, so this is the only
correct watch loop:

```bash
V=$(hocket agent get_state | jq .result.version)
hocket agent wait_version --after "$V" --timeout-ms 30000 | jq .result
# exit 1 with "timeout waiting for state change" just means nothing happened
```

**Stage a batch, then execute.** Nothing touches Raindrop until `execute`:

```bash
hocket agent set_filter "haskell"
hocket agent list_items | jq -r '.result[].id' | while read -r id; do
  hocket agent set_flag "$id" --action archive
done
hocket agent get_state | jq .result.counts   # confirm before committing
hocket agent execute
```

**Confirm a write actually landed.** `"ok": true` means *queued*, not done:

```bash
V=$(hocket agent set_flag 123 --action archive | jq .result.version)
hocket agent wait_version --after "$V"
hocket agent get_item 123 | jq .result.pending
```

**Tell the human what you are doing.** The status bar is shared with the
TUI, so use it while you work:

```bash
hocket agent set_status "reviewing 40 unread bookmarks"
```

## Gotchas

- **Acknowledged is not applied.** Injection success means the event was
  queued on a channel of capacity 10. Confirm with `wait_version`.
- **`version` is a pulse, not a digest.** It increments on any render,
  including ones that changed nothing you care about. Never treat it as a
  content hash; re-read state after it moves.
- **`event channel full, retry`** is ordinary back-pressure, not an error in
  your request. Retry it.
- **`reminder` on an item that already has one is rejected** — use
  `remove_reminder`. The reverse (`remove_reminder` on an item with no
  reminder) is rejected too. Reminders are always scheduled for the next day
  at 07:00 local, exactly like the `s` key.
- **`select_item` needs a visible item**; `open_item` and `get_item` do not.
- **Status is single-lane.** `set_status` overwrites whatever the TUI was
  showing, and the TUI overwrites you.
- **`execute` is not reversible.** It archives and sets reminders against
  the user's real Raindrop account. Stage, show the counts, and get
  confirmation before running it.
- **There is no `add_bookmark` over the socket.** Use the separate CLI:
  `hocket add URL --tag foo`.

## Without the binary

The protocol is newline-delimited JSON; any socket client works.

```bash
SOCK=${XDG_RUNTIME_DIR:-/tmp}/hocket/control.sock
echo '{"id":1,"method":"get_state"}' | socat - UNIX-CONNECT:$SOCK
echo '{"id":2,"method":"set_flag","params":{"id":"123","action":"archive"}}' \
  | socat - UNIX-CONNECT:$SOCK
```

With OpenBSD netcat pass `-N`, or it hangs after the reply:
`echo '...' | nc -N -U $SOCK`.

## The spec

This file is a working summary. `docs/RPC.md` in the hocket repo is the
normative specification — transport, framing, stale-socket reclamation,
validation rules, the concurrency model and the security boundary. Read it
before implementing a client or when this summary and the server disagree.
