# Hocket Agent Control-Socket RPC

This document specifies the message protocol spoken over the agent control
socket introduced in the **agent-native** line of work. It is the reference
for anyone implementing a client — an AI agent harness, a shell script, an
editor plugin — or auditing the server implementation.

For the motivation, target experience and the roadmap (MCP bridge,
`add_bookmark`), see [`agent-native-design.md`](agent-native-design.md).
For the operations guide, see the *Agent Control Socket* section of the
`README.md`.

- [1. Overview](#1-overview)
- [2. Transport](#2-transport)
  - [2.1 Socket location](#21-socket-location)
  - [2.2 Framing](#22-framing)
  - [2.3 Permission and ownership](#23-permission-and-ownership)
  - [2.4 Reclaiming a stale socket](#24-reclaiming-a-stale-socket)
- [3. Wire format](#3-wire-format)
  - [3.1 Request](#31-request)
  - [3.2 Response](#32-response)
  - [3.3 Errors](#33-errors)
- [4. State model and versioning](#4-state-model-and-versioning)
  - [4.1 The snapshot mirror](#41-the-snapshot-mirror)
  - [4.2 Monotonic versions](#42-monotonic-versions)
  - [4.3 What crosses the socket (and what never does)](#43-what-crosses-the-socket-and-what-never-does)
- [5. Read methods](#5-read-methods)
  - [5.1 `get_state`](#51-get_state)
  - [5.2 `list_items`](#52-list_items)
  - [5.3 `get_item`](#53-get_item)
  - [5.4 `wait_version`](#54-wait_version)
- [6. Write methods](#6-write-methods)
  - [6.1 The write path](#61-the-write-path)
  - [6.2 `set_flag`](#62-set_flag)
  - [6.3 `clear_all_flags`](#63-clear_all_flags)
  - [6.4 `flag_all_archive`](#64-flag_all_archive)
  - [6.5 `execute`](#65-execute)
  - [6.6 `refresh`](#66-refresh)
  - [6.7 `set_filter`](#67-set_filter)
  - [6.8 `set_video_filter`](#68-set_video_filter)
  - [6.9 `set_show_future_reminders`](#69-set_show_future_reminders)
  - [6.10 `select_item`](#610-select_item)
  - [6.11 `open_item`](#611-open_item)
  - [6.12 `set_status`](#612-set_status)
- [7. Concurrency model](#7-concurrency-model)
- [8. Security](#8-security)
- [9. Design constraints](#9-design-constraints)
- [10. Example session](#10-example-session)
- [11. Implementation map](#11-implementation-map)

---

## 1. Overview

The TUI runs with Brick and keeps keyboard-driven invariants. The agent
control socket is an escape hatch for external processes to **observe** and
**drive** the *same* live session the user sees — not a separate view of
the Raindrop API.

Two properties make this safe and simple:

- **Reads never block the UI.** They are answered from a handful of
  transactional variables (a state mirror, the event channel). The Brick
  event loop is never touched on a read path. `wait_version` is a blocking
  STM `check` that parks the agent's own thread, not the TUI.
- **Writes are just keystrokes.** A validated write is transcribed into the
  exact `HocketEvent` the corresponding keybinding would have produced and
  injected into the same `BChan` the keyboard feeds. The agent cannot
  produce a state that a human at the keyboard could not, so every TUI
  invariant holds for the agent automatically.

Because the protocol is newline-delimited JSON, the server can be driven
straight from a shell (`socat`, `nc -U`) with no client library.

---

## 2. Transport

### 2.1 Socket location

The default path is resolved at startup:

1. If `$XDG_RUNTIME_DIR` is set and non-empty:
   `$XDG_RUNTIME_DIR/hocket/control.sock`
2. Otherwise `/tmp/hocket-<uid>/control.sock` using the effective user id.

The directory is created if missing. `--agent-socket-path PATH` overrides
the path entirely; `--agent-socket` selects the default. The socket is bound
with a listen backlog of **5**.

### 2.2 Framing

Each request is a single line of JSON terminated by `\n`. Each response is a
single line of JSON terminated by `\n`. The server sets the connection
handle to **line buffering**, so one read = one request and one write = one
response. The server **never** emits unsolicited messages: the connection is
strictly request/response, one in flight at a time per connection.

A client that sends concatenated multiple lines per write is handled (each
line is a separate request). A blank/whitespace-only line is skipped.

### 2.3 Permission and ownership

- The containing directory is created `0700`.
- The socket file itself is created `0600`.
- The socket is owned by whichever user launched the TUI.

Only the owning user (and root) may connect. This is the first line of
defense: an agent running as a different user cannot reach the socket.

### 2.4 Reclaiming a stale socket

A crash or hard kill leaves a socket *file* behind with no live listener.
Before binding, the server **probes** the path with a `connect`:

- **Connect succeeds** → a live listener owns the path. We leave it alone so
  the subsequent `bind` raises the kernel's canonical "address already in
  use". We never silently hijack a live competitor.
- **Connect refuses** (`ECONNREFUSED` / any `IOException`) → the owning
  process is gone. We unlink only if a file actually exists, then bind.

On graceful TUI exit the socket file and its (now-empty) directory are
removed, best-effort and never allowed to change the exit outcome.

---

## 3. Wire format

### 3.1 Request

```json
{ "id": 1, "method": "get_state" }
{ "id": 2, "method": "set_flag", "params": { "id": "123", "action": "archive" } }
```

- `id` — *optional*, echoed verbatim in the response. Any JSON value
  (number, string, null, object). Use it to correlate responses. `null` is
  accepted and preserved.
- `method` — *required*, a non-empty string (see §5, §6).
- `params` — *optional*. When absent, treated as an empty object `{}`.
  When present it **must** be a JSON object; otherwise the request is
  rejected.

The method name and params are decoded **separately** (`decodeCmd`), so a
malformed method can still be answered with the request's own `id`.

### 3.2 Response

```json
{ "id": 1, "ok": true, "result": { ... } }
{ "id": 2, "ok": false, "error": "...wikinomial..." }
```

- `id` — echoes the request's `id`.
- `ok` — boolean.
- `result` — present iff `ok` is `true`; the method-specific payload.
- `error` — present iff `ok` is `false`; a human-readable message.

`ok` is the authoritative discriminator. Do not infer success from the
presence of `result`.

### 3.3 Errors

`error` values are free-form text, not codes. Failure categories:

| Category | Cause |
|---|---|
| `bad request: <json>` | Line is not valid JSON, or `params` is not an object |
| `unknown method: <m>` | Unrecognised `method` |
| method-specific | e.g. `item is not visible under the current filters: <id>` |
| `event channel full, retry` | Injected write could not be queued (see §6.1) |
| `timeout waiting for state change` | `wait_version` timed out |

Unknown methods and malformed bodies still get a normal `{"ok": false}`
response with the request `id`; they do **not** close the connection.

---

## 4. State model and versioning

### 4.1 The snapshot mirror

The server holds a typed mirror of the TUI state (`AgentSnapshot`), stored
in a `TVar`, rebuilt after **every** render (`syncForRender` + snapshot
projection). It contains:

- The plain bookmark data currently in memory: id, title, link, tags, note,
  excerpt, created time, reminder, favorite flag, pending action.
- Whether each item is **visible** under the current filter pipeline (vs.
  present in the contents map but filtered out).
- The current selection, active filter query, video filter mode,
  `show_future_reminders`, status text, last-update time, and the in-flight
  async operation (`fetch` / `execute_batch`).
- A monotonic **version** (§4.2).

Rebuilding after every render means the mirror is at worst one render behind
the actual screen. It is cheap to do because the snapshot shares the
persistent data structures; the full item JSON is only forced by a server
thread that actually serves it.

### 4.2 Monotonic versions

Every rebuild bumps the version: `takeSnapshot (asVersion old + 1) s`. The
version is the agent-side equivalent of a TUI screen repaint.

- It counts successful renders, so it increases even when nothing that
  matters to the agent changed (e.g. a keypress that only moves the cursor
  past the same item still bumps it).
- It is **not** an absolute state hash and never resets within a session
  (starts at `0` from `emptySnapshot`).
- `wait_version` and the `version` field are the only places the version is
  consulted; clients should treat it as a purely *monotonic change signal*,
  never as a content digest.

The correct client pattern for "watch for changes":

1. `get_state` → note `version`.
2. `wait_version` with `after` = that version (long-poll).
3. On success, `get_state` again (or use the `stateView` already returned)
   to pull the new state.

### 4.3 What crosses the socket (and what never does)

A `SnapshotItem` carries exactly: `id`, `title`, `link`, `tags`, `note`,
`excerpt`, `created`, `reminder`, `favorite`, `pending`, `visible`.

**Credentials never cross the socket.** The Raindrop API token is
structurally absent from `AgentSnapshot` — it is not part of the type, so it
*cannot* be serialised even by accident. The socket is a view of in-memory
bookmark state, not a general access layer to the Raindrop backend.

---

## 5. Read methods

Reads are answered purely from the mirror; they never block the UI and never
inject anything.

### 5.1 `get_state`

```
params: {}
```

Returns the complete `stateView`: header summary plus (in the snapshot form)
items etc. Method-specific result fields:

| Field | Type | Meaning |
|---|---|---|
| `version` | int | Monotonic render counter (§4.2) |
| `counts.total` | int | All items currently in memory |
| `counts.visible` | int | Items passing the filter pipeline |
| `counts.archive_flagged` | int | Items pending `archive` |
| `counts.reminder_flagged` | int | Items pending a reminder |
| `counts.remove_reminder_flagged` | int | Items pending reminder removal |
| `selected` | id \| null | Currently selected item |
| `filter_query` | string | Active filter query |
| `video_filter` | `none` \| `only_videos` \| `hide_videos` | Active video filter |
| `show_future_reminders` | bool | Whether future reminders are shown |
| `status` | string \| null | Current status line |
| `last_updated` | number \| null | POSIX time of last sync |
| `async_op` | `fetch` \| `execute_batch` \| null | In-flight async op |

Item fields are as specified in §4.3 with `pending` as flat action objects
(§6.2).

### 5.2 `list_items`

```
params: { "visible_only": true, "flagged_only": false }   # both optional
```

Returns a JSON array of `SnapshotItem` objects. Filtering:

- `visible_only` defaults to `true` — only items visible under the current
  filters are returned.
- `flagged_only` defaults to `false` — when `true`, only items with a
  non-`none` pending action are returned.

The two combine by **AND**. An item is included iff

```
(not visible_only  OR visible)
AND (not flagged_only OR pending ≠ none)
```

Items appear in the same order as in the snapshot (visible items in display
order first, then hidden items).

### 5.3 `get_item`

```
params: { "id": "123" }
```

Returns a single `SnapshotItem`. The id must reference an item in the
contents map; a **hidden** item (not currently visible) is still returned,
with `visible: false`. Returns `item is not visible under the current
filters`-style errors only where applicable; an unknown id yields
`unknown item id: <id>`.

### 5.4 `wait_version`

```
params: { "after": 12, "timeout_ms": 10000 }   # after required, timeout optional
```

Long-polls for a state change. Blocks the *agent's own server thread* on
`STM.check (version > after)` and returns the fresh `stateView` when it
passes. `timeout_ms` defaults to `10000`, is clamped to `[0, 60000]`, and is
a hint: on timeout the response is `{"ok": false, "error":
"timeout waiting for state change"}`.

Because the snapshot only advances on render, `wait_version` can in
principle wait until the next repaint. This is the building block for an
agent that needs to wait until its injected change actually lands.

---

## 6. Write methods

### 6.1 The write path

A write goes through four stages before anything affects the UI:

```
decode → validate → transcribe → inject
```

1. **decode** — method/params become a typed `WriteCmd`.
2. **validate** (`validateWrite`) — a pure check against the current mirror,
   rejecting writes that cannot make sense. Checks are on slow-changing
   facts (item identity, whether a reminder already exists, whether an item
   is visible), never on cursor-level state. Validation failures return an
   error and **nothing** is injected.
3. **transcribe** (`toEvent`) — the validated command becomes the
   `HocketEvent` the keyboard would have produced. For `set_flag`
   `reminder`, the reminder clock (`nextDayAt7AM`) is sampled *here*, in the
   server thread, exactly matching the `s` key.
4. **inject** — the event is written non-blocking into the TUI `BChan`
   (capacity **10**). Injection returns a boolean:
   - `true` → the event is queued; response is
     `{"ok": true, "result": {"injected": true, "version": <v>}}` where
     `<v>` is the mirror version *at injection time*.
   - `false` → the channel is full; response is `event channel full, retry`.

**Acknowledging injection is not acking effect.** `ok: true` means *the
event was queued*, not that the UI has processed or rendered it. To confirm
the effect landed, use `wait_version` after the current version and re-read.

Writes are **idempotent by construction** — they are the same events the
keyboard fires, so replaying a write is no different from the user pressing
the same key twice.

The returned `version` is the snapshot version at injection time; the *next*
successful render will be strictly greater, so `wait_version` with `after`
= that version returns once the effect is reflected.

### 6.2 `set_flag`

```
params: { "id": "123", "action": "archive" }
```

`action` ∈ `archive` | `reminder` | `remove_reminder` | `none`. Sets the
pending action on one item, mirroring the flag/reminder keys. Validation:

- `reminder` on an item that **already has** a reminder → rejected
  (`item already has a reminder (flag remove_reminder instead): <id>`).
- `remove_reminder` on an item **without** a reminder → rejected (`item has
  no reminder to remove: <id>`).
- `archive` / `none` are always accepted for a known id.
- Unknown id → `unknown item id: <id>`.

Serialised `pending` field of an item:

```json
{ "action": "none" }
{ "action": "archive" }
{ "action": "reminder", "at": "<ISO-8601>" }
{ "action": "remove_reminder" }
```

### 6.3 `clear_all_flags`

```
params: {}
```

Clears every staged pending action — the agent's `u` (clear all).

### 6.4 `flag_all_archive`

```
params: {}
```

Flags every item for archive — the agent's bulk `A`-style op. Note this
stages flags; nothing executes until `execute`.

### 6.5 `execute`

```
params: {}
```

Executes the currently staged batch, exactly like the `X` key. After this,
`async_op` becomes `execute_batch` until completion.

### 6.6 `refresh`

```
params: {}
```

Triggers a resync — the `r` key. `async_op` becomes `fetch` until
completion. The agent should follow with `wait_version` to learn when the
updated data lands.

### 6.7 `set_filter`

```
params: { "query": "news site" }
```

Replaces the live filter query. `visible` flags and the `visible` count will
change on the next render.

### 6.8 `set_video_filter`

```
params: { "mode": "none" | "only_videos" | "hide_videos" }
```

Sets the video filter. Unknown modes are rejected at decode time.

### 6.9 `set_show_future_reminders`

```
params: { "show": true }
```

Toggles whether future reminders are displayed.

### 6.10 `select_item`

```
params: { "id": "123" }
```

Moves the UI selection to the item — the agent can drive what the human's
cursor points at. Validation requires the item to be **visible** under the
current filters:
`item is not visible under the current filters: <id>`.

### 6.11 `open_item`

```
params: { "id": "123" }
```

Opens the item in the browser. Any item in the contents map is accepted
(need not be visible).

### 6.12 `set_status`

```
params: { "text": "hello" }
```

Sets the status line to **`agent: <text>`** — the same line the TUI uses.
Only one status line exists, so an agent overwrites whatever is currently
shown (and vice-versa); there is no separate agent lane.

---

## 7. Concurrency model

```
                ┌─────────────────────────────── TUI (Brick) ──┐
  keyboard ──► BChan(10) ──► event loop ──► render ──► TVar snapshot
                ▲  ▲                                             │
                │  └── non-blocking inject (aeInject)            │
                │                       read                     ▼
       agent server thread ───────────────────────► TVar snapshot
        │         accept loop                          (STM/readTVarIO)
        │         └─ fork per client ── wait_version (check) / reads / writes
        ▼
 publish client count ──► BChan ──► SetAgentClients ──► header [agent]
```

- The **agent server thread** owns a `runAgentServer` accept loop; it never
  returns normally. If it dies, a status line (`agent socket failed: …`) is
  surfaced instead of crashing the app. In-flight failures inside a client
  connection are isolated.
- **One thread per client**, spawned with `forkFinally`. The client count is
  mirrored into the TUI header as `[agent]` while `≥ 1` client is connected
  (`SetAgentClients`).
- **`wait_version` blocks only the client's thread** via `STM.check`; the
  event loop and other clients are unaffected.
- **Single shared event channel.** Keyboard and agent writes feed the same
  `BChan` of capacity 10. If the loop is busy, a write can be dropped
  (`aeInject == False` → `event channel full, retry`). This is the one place
  the protocol can race the user; the write path accounts for it.
- The snapshot `TVar` is written under `atomically` after every render and
  read `readTVarIO` / within `STM` by server threads. The only sequential
  point is the version bump, which happens inside the render thread, so
  versions are never duplicated.

---

## 8. Security

1. **Filesystem lockdown.** Socket file `0600`, containing directory `0700`,
   owned by the launching user. A socket path is required to connect
   (§2.3).
2. **No credentials on the wire.** The Raindrop token is structurally absent
   from `AgentSnapshot`; it cannot be serialised (§4.3). The link field is
   exposed (as it is on the screen), but no session/API secrets are.
3. **Writes are keystrokes.** The agent is bounded to the same operations a
   human can perform; it cannot reach the filesystem, shell, or Raindrop
   write API directly through this socket.
4. **Stateful validation.** Flag and selection writes are rejected when
   they are impossible against the current mirror (§6.2, §6.10), and
   malformed/unknown input is answered with an error rather than executed.
5. **No silent socket hijack.** A live competitor is never unlinked (§2.4).

The trust boundary is "whoever can read/write the owning user's `~/…`
runtime dirs" — i.e. effectively the same user. The socket is **not**
encrypted and provides **no authentication beyond Unix permissions**; it is
designed for same-user, same-host tools.

---

## 9. Design constraints

These are intentional, not bugs:

- **One response per request, no push.** Clients must poll or long-poll;
  there is no subscription/notification channel. `wait_version` is the
  polling primitive.
- **`version` is a pulse, not a digest.** It beats on any render and cannot
  be used to detect "the specific change I wanted".
- **Acknowledged ≠ done.** Inject success means queued. Confirm via
  `wait_version`.
- **Status is single-lane.** `set_status` overwrites the shared status
  line.
- **No `add_bookmark` (yet).** It is an MCP-side/open item on the roadmap;
  the socket today cannot add bookmarks.
- **Writes are best-effort under load.** The `BChan` capacity of 10 means
  back-pressure surfaces as `event channel full` rather than blocking the
  agent or the TUI.

---

## 10. Example session

Read the header, long-poll for the next change, then react:

```bash
SOCK=${XDG_RUNTIME_DIR:-/tmp}/hocket/control.sock

# 1. initial state
echo '{"id":1,"method":"get_state"}' | socat - UNIX-CONNECT:$SOCK

# 2. block until the state moves past version 42, up to 30s
echo '{"id":2,"method":"wait_version","params":{"after":42,"timeout_ms":30000}}' \
  | socat - UNIX-CONNECT:$SOCK

# 3. flag every visible news-site item for archive (id "101")
echo '{"id":3,"method":"set_flag","params":{"id":"101","action":"archive"}}' \
  | socat - UNIX-CONNECT:$SOCK

# 4. wait for it to render (injected version, then +1)
echo '{"id":4,"method":"wait_version","params":{"after":43}}' \
  | socat - UNIX-CONNECT:$SOCK

# 5. execute the staged batch
echo '{"id":5,"method":"execute"}' | socat - UNIX-CONNECT:$SOCK
```

(A real client keeps one connection open and sends each request as its own
line, reusing the connection across requests rather than reconnecting like
the one-shot `socat` lines above.)

---

## 11. Implementation map

| Concern | Module | Notes |
|---|---|---|
| Protocol decode / validate / serve | `src/Network/Bookmark/Agent/Protocol.hs` | `decodeCmd`, `validateWrite`, `serveRead`, `stateView`; total & pure |
| Snapshot mirror + JSON | `src/Network/Bookmark/Agent/Snapshot.hs` | `AgentSnapshot`, `takeSnapshot`, `ToJSON` |
| Socket server (IO shell) | `main/AgentServer.hs` | `runAgentServer`, `resolveAgentSocketPath`, `reclaimStaleSocket`, `handleLine` / `dispatch` / `toEvent` |
| Event types + injection | `main/Events.hs` | agent write events; `setAgentClientsEvt` |
| Wiring, rendering mirror, cleanup | `main/hocket.hs` | `startAgentServer`, `app` snapshot rebuild, `cleanupAgentSocket` |
| Tests | `test/test.hs` | protocol round-trips and IO-shell behaviour |