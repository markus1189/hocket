# Agent-Native Hocket: Design Exploration

Status: **phases 1–2 implemented** (state mirror, control socket, read
methods, validated writes; `add_bookmark` and the MCP bridge are still
open). The protocol as built is specified in [RPC.md](RPC.md).

Written before the functional-core refactor. The design held; names and
places below are today's where they describe code. Since the refactor the
pure `library core` (domain, the one `update :: Msg -> Model -> (Model,
[Effect])`, the agent protocol and snapshot) knows no brick, socket or
thread, and the main library around it holds the ports, adapters and the
shell: the TUI, the headless loop, the agent server and client
([AGENTS.md, Architecture](../AGENTS.md#architecture-overview)).

## Goal

Make the hocket TUI *agent-native*: while the user works in the TUI, an AI
agent (Claude Code or any other agent harness) can

- **see** the same live state the user sees — items, selection, pending
  flags, active filters, sync status — and
- **act** on it with full read/write access: flag items, set filters, move
  the selection, execute the batch, refresh, add bookmarks.

The target experience: the user has `hocket tui` open in one terminal and a
Claude Code session in another. They say *"archive everything from
news sites older than a month, but keep the favorites"* — and watch the
`A` flags appear live in their TUI, review them, and either let the agent
execute or press `X` themselves.

## Chosen direction (from interview)

1. **Control socket / IPC as the foundation** — the TUI process listens on
   a Unix domain socket speaking a small JSON protocol.
2. **MCP server layered on top** — a `hocket mcp` subcommand bridges the
   socket to the Model Context Protocol so agents get typed tools without
   custom glue.
3. **Full read/write** — the agent may also execute operations (archive,
   set/remove reminders, add bookmarks), not just stage flags.
4. Design doc first (this document), implementation after review.

### Why socket *plus* MCP rather than MCP alone

MCP's common transport is stdio — but hocket's stdin/stdout *is* the TUI
(vty owns the terminal). An MCP server therefore has to be a separate
process no matter what. Splitting the problem gives each layer a clean job:

- The **socket** is the single integration point with the running TUI.
  It is agent-agnostic: `socat`, shell scripts, editor plugins, tests, and
  the MCP bridge all use the same protocol.
- The **MCP bridge** (`hocket mcp`) is a thin stateless translator:
  stdio JSON-RPC on one side, the control socket on the other. If MCP
  evolves (streamable HTTP, new auth), only the bridge changes.

## Architecture

```
┌─────────────┐   keystrokes   ┌──────────────────────────────────┐
│    user     │ ─────────────► │  hocket tui (one process)        │
└─────────────┘                │                                  │
                               │  Brick event loop                │
┌─────────────┐                │   ├─ vtyEventHandler             │
│ Claude Code │                │   └─ internalEventHandler        │
└──────┬──────┘                │        ▲            │            │
       │ stdio (MCP)           │        │ writeBChan │ after each │
┌──────▼──────┐                │        │            ▼ event      │
│ hocket mcp  │   UDS, JSON    │  ┌─────┴──────┐  ┌────────────┐  │
│  (bridge)   │ ◄────────────► │  │ agent      │  │ TVar       │  │
└─────────────┘                │  │ server     │◄─┤ snapshot   │  │
                               │  │ (thread)   │  │ (mirror)   │  │
                               │  └────────────┘  └────────────┘  │
                               └──────────────────────────────────┘
```

Two mechanisms, matching the two halves of "agent-native":

### Seeing: a snapshot mirror (`TVar AgentSnapshot`)

The model (`Model`, `Hocket.App.Model`) lives in the TUI's event loop and
is invisible from other threads. Rather than routing reads through the event loop (request/response
events are awkward in Brick — events don't return values), the TUI
maintains a mirror:

- A new serializable type `AgentSnapshot` containing: all items (id,
  title, link, tags, note, excerpt, created, last update, reminder,
  favorite, **pending action**), the *visible* item order and selection
  (i.e. after `settle`'s filter pipeline), filter states
  (`mFilterQuery`, `mVideoFilter`, `mShowFutureReminders`), `mStatus`,
  `mLastUpdated`, the async-op slot (`mAsyncOp`), and a monotonically
  increasing `stateVersion`.
- Every handled event ends with `settle` and `publish`
  (`Hocket.Shell.Tui.App`; the headless loop does the same), which writes
  the snapshot to a `TVar`.
  Because Haskell data is immutable and shared, this is a pointer swap —
  serialization cost is only paid when an agent actually asks.
- The agent server thread answers all read requests from the `TVar`
  directly; the Brick loop is never blocked by reads.

`stateVersion` (bumped on every handled event) tells agents that
something changed. It does not say that *their* write landed: a wait can
wake for an earlier event or another agent's connection, so an agent
re-reads until the state shows the effect, until a deadline (RPC.md §7.1).

### Acting: inject events through the existing `BChan`

Agent writes become the `Intent` their key produces (`toIntent`), injected
as a `Msg` into the same `BChan` that keystrokes and async completions
already use (`injectMsg`, `Hocket.Shell.Interpret`).
This is the crucial design property: **the agent goes through the same
funnel as the keyboard**, so every existing invariant holds for free —
the single-slot async lock (`withAsyncSlot` / `tryAcquireAsyncOp`), flag
toggle semantics, filter re-rendering, drop-on-busy for concurrent
fetch/execute.

Much of the surface already existed as id-addressed events, at the time of
this design (main/Events.hs, since replaced by `Msg` and `Intent`; today's
mapping is `toIntent` in `Hocket.Agent.Intent`):

| Agent command            | Existing event                                  |
|--------------------------|-------------------------------------------------|
| flag/unflag archive      | `ShiftItem bid` (toggle)                        |
| flag/unflag reminder     | `ShiftItemReminder bid` (smart toggle)          |
| clear all flags          | `ClearAllFlags`                                 |
| flag all for archive     | `SetAllFlagsToArchive`                          |
| execute batch            | `ExecuteBatch`                                  |
| refresh                  | `FetchItems`                                    |
| toggle video filters     | `ToggleVideoFilter` / `ToggleInvertedVideoFilter` |
| toggle future reminders  | `ToggleReminders`                               |
| set status message       | `SetStatus`                                     |
| open item in browser     | `BrowseItem`                                    |

A few new `UiCommand` constructors are needed where toggling is the wrong
verb for an agent (toggles are racy if the agent's view is stale) or no
event exists yet:

- `SetPendingAction !BookmarkItemId !PendingAction` — idempotent "make it
  so" flagging. The agent says *flag for archive*; if it's already flagged
  nothing changes. This is the workhorse; toggle events stay for the keyboard.
- `SetFilterQuery !Text` — set/replace the fuzzy filter wholesale (the
  keyboard edits it in place via the filter editor; an agent shouldn't).
- `SetVideoFilter !VideoFilterMode` / `SetShowFutureReminders !Bool` —
  idempotent variants of the toggles.
- `SelectItem !BookmarkItemId` — move the TUI selection so the user can
  literally watch the agent walk the list ("look at this one").
- `AddBookmarkAsync !Text !(Maybe Text) ![Text]` (new `AsyncCommand`) —
  reuse the `hocket add` code path from inside the TUI, followed by a
  refresh.

Mutations are fire-and-forget at the protocol level (ack = "event
injected"); agents confirm effects by re-reading until they show (see
above). One
caveat to handle: `writeBChan` blocks when the channel (size 10) is full —
the agent server must write with a timeout and report back-pressure as an
error instead of hanging a request.

## The control socket protocol

- **Transport**: Unix domain socket at `$XDG_RUNTIME_DIR/hocket/control.sock`
  (fallback `/tmp/hocket-$UID/control.sock`), directory `0700`, socket
  `0600`, unlinked on start and on clean exit. Multiple concurrent clients
  allowed.
- **Framing**: newline-delimited JSON (JSON Lines). Requests carry an `id`;
  responses echo it. Deliberately *not* full JSON-RPC — but shaped so the
  MCP bridge's translation is mechanical.
- **Opt-in**: off by default. Enabled via config
  (`_agentSocket = Some True` or a path override in `config.dhall`) or a
  `--agent-socket` flag on `hocket tui`.

### Methods (initial set)

Read (served from the snapshot, never touching the event loop):

| method        | params                          | returns |
|---------------|---------------------------------|---------|
| `get_state`   | —                               | header info: counts, filters, status, selection, `stateVersion`, `asyncOp` |
| `list_items`  | `visible_only?`, `flagged_only?`| full item records incl. pending actions, in display order |
| `get_item`    | `id`                            | one full item record |
| `wait_version`| `after`, `timeout_ms`           | blocks until `stateVersion > after` (long-poll; the cheap substitute for subscriptions in phase 1) |

Write (injected as events):

| method            | params                         | maps to |
|-------------------|--------------------------------|---------|
| `set_flag`        | `id`, `action` (`archive` \| `reminder` \| `remove_reminder` \| `none`) | `SetPendingAction` |
| `clear_all_flags` | —                              | `ClearAllFlags` |
| `flag_all_archive`| —                              | `SetAllFlagsToArchive` |
| `execute`         | —                              | `ExecuteBatch` |
| `refresh`         | —                              | `FetchItems` |
| `set_filter`      | `query`                        | `SetFilterQuery` |
| `set_video_filter`| `mode`                         | `SetVideoFilter` |
| `select_item`     | `id`                           | `SelectItem` |
| `open_item`       | `id`                           | `BrowseItem` |
| `add_bookmark`    | `url`, `collection?`, `tags?`  | `AddBookmarkAsync` |
| `set_status`      | `text`                         | `SetStatus` — lets the agent talk to the user *inside* the TUI status bar |

`reminder` uses the same next-day-7am policy as the `s` key
(`nextDayAt7AM`); an optional `at` param can come later without breaking
the shape.

## The MCP bridge: `hocket mcp`

A third subcommand next to `tui` and `add`. It speaks MCP over stdio and
forwards to the socket; it holds no state of its own.

- **Tools** map 1:1 to socket methods, named `hocket_get_state`,
  `hocket_list_items`, `hocket_set_flag`, `hocket_execute`, … with JSON
  Schemas for inputs so agents get typed, discoverable tools. Descriptions
  encode the workflow hints ("prefer staging flags and telling the user to
  review before calling hocket_execute" — even with full write access,
  that's the polite default the agent can choose to follow).
- **Registration** is one command for the user:
  `claude mcp add hocket -- hocket mcp`.
- **Implementation**: hand-rolled. The stdio MCP server surface needed here
  is small (`initialize`, `tools/list`, `tools/call`) and aeson covers it;
  Haskell MCP SDK packages exist but are young, and taking a dependency for
  three request types isn't worth it.
- **Error UX**: if the socket is absent, the bridge returns a tool error
  saying "hocket tui isn't running (or agent socket is disabled)" — the
  agent can relay that to the user verbatim.

### Where it goes: one more adapter, no core change

Checked against the code after the refactor: the bridge is one module in
the shell, `Hocket.Shell.Mcp`, plus a `mcp` subcommand in main/hocket.hs.
It speaks `AgentCmd` through the client that `hocket agent` already uses,
and needs nothing from the core that the core does not export today:

- `tools/list` answers a table in the module, one row per socket method:
  the tool name (`hocket_` + method), a description and the JSON Schema of
  its params, as RPC.md §5–6 specifies them.
- `tools/call hocket_<m> args` is `decodeCmd m (Just args)`
  (`Hocket.Agent.Protocol`: the same decoder the server runs, so a bad
  argument fails as it would on the socket), then `callAgent path cmd`
  (`Hocket.Shell.Agent.Client`) on `resolveAgentSocketPath`
  (`Hocket.Shell.Agent.Server`), and the reply's `result` or `error` as the
  tool's content or error.
- Optionally the write tools confirm their effect the way RPC.md §7.1
  says, re-reading until a deadline, so that agents get "landed" instead
  of "queued"; that is client code too.
- One test keeps the table honest: every command `genAgentCmd` makes, put
  through `encodeCmd`, names a method the table has. That `decodeCmd`
  reads those params back is tested already (`Core.Properties`).

## What the user sees in the TUI

Agent-native should be visible and non-spooky:

- **Connection indicator**: header gains a marker (e.g. `[agent]`) while at
  least one socket client is connected.
- **Agent-set status**: `set_status` messages render in the existing status
  bar, prefixed (e.g. `agent: staged 12 items for archive`).
- Flags set by the agent look exactly like user flags — they *are* user
  flags — so the existing review flow (`J`/`K` to jump between flagged,
  `u` to unflag, `X` to execute) doubles as the human-approval flow.

## Alternatives considered (and why not)

- **Chat pane inside the TUI**: embedding an LLM conversation in Brick
  means API keys, streaming, and agent-harness features (tools, memory)
  re-implemented in Haskell — and it locks users into one agent. Letting a
  real harness connect from outside is strictly more capable. Could still
  be built later *on top of* the socket.
- **Headless CLI + JSON only** (`hocket list --json` etc.): simplest, but
  there is no live-TUI awareness — the agent operates on a parallel copy of
  reality, and staged flags (which only exist in TUI memory) are invisible
  to it. Kept as a possible phase-5 fallback *inside the MCP bridge* for
  when the TUI isn't running.
- **Terminal scraping (tmux capture-pane)**: works with zero code changes
  but is brittle, read-only in practice, and loses all structure
  (truncated URLs, no ids).
- **State-file dump + polling**: no way to act, laggy, and file lifecycle
  is messier than a socket.

## Security notes

- The socket grants **full control of the user's Raindrop account minus
  credentials**: the Raindrop token never crosses the socket, and no
  method exposes it (the model does not hold the credentials at all;
  only the Raindrop adapter and the config loader see them).
- Socket permissions (`0600`, dir `0700`) limit access to the same user —
  the standard local-IPC trust model (same as tmux, gpg-agent, ssh-agent).
- Off by default; enabling it is an explicit config/flag choice.
- `execute` and `add_bookmark` have real-world effects. With full
  read/write chosen, the guardrail is convention (tool descriptions steer
  agents to stage-then-confirm) plus the user's own agent-side permission
  system — Claude Code will ask before calling mutating MCP tools unless
  the user allowlists them. Hocket doesn't need to duplicate that.

## Implementation plan (incremental, each phase shippable)

Phase 1 — mirror + read-only socket
- New lib modules: `Hocket.Agent.Snapshot` (types + aeson instances,
  built from `Model`), `Hocket.Shell.Agent.Server` (socket accept loop,
  JSON-lines framing, read methods incl. `wait_version`).
- Wire-up in `main/hocket.hs`: create `TVar`, write it in `appHandleEvent`,
  start server thread when enabled, `[agent]` header indicator.
- New deps: `network`, `unix`, `stm` (all boring, all in nixpkgs/stackage).
- Config: optional `_agentSocket` field — must stay backward-compatible
  with existing `config.dhall` files (`Optional` in the Dhall schema).
- Tests: snapshot JSON golden tests (tasty-golden already in the test
  deps); protocol handler tests against a fabricated snapshot, no vty
  needed.

Phase 2 — mutations
- New events: `SetPendingAction`, `SetFilterQuery`, `SetVideoFilter`,
  `SetShowFutureReminders`, `SelectItem`, `AddBookmarkAsync`; handlers in
  `uiCommandEventHandler` / `asyncCommandEventHandler` (mostly one-liners
  onto existing `State.hs` functions; `SetPendingAction` needs a small new
  pure function next to `togglePendingAction`).
- Socket write methods, with the non-blocking `writeBChan` guard.

Phase 3 — MCP bridge
- `hocket mcp` subcommand: stdio loop, tool schemas, socket client.
- README: keybinding-style docs for the agent surface + `claude mcp add`
  one-liner.

Phase 4 — polish
- Push notifications over the socket (`{"event": "state_changed", ...}`)
  to replace long-polling for agents that keep the connection open.
- `set_flag` bulk variant (list of ids) to cut round-trips on large sweeps.
- Reminder `at` parameter.

Phase 5 (optional, decide later)
- Headless mode in the bridge: when no TUI is running, serve
  `list/add/archive` directly against the Raindrop API so the agent still
  functions (flags/selection semantics don't exist there; tools would say
  so).

## Open questions for review

1. **Snapshot scope**: include *all* items (thousands) in `list_items` by
   default, or default `visible_only=true`? Proposal: default to visible
   items; full dump behind the explicit flag.
2. **Multiple agents**: allow N concurrent clients (current design) or
   enforce a single client? N is simpler to implement (no ownership) and
   flags are idempotent, but two agents could fight; proposal: allow N,
   revisit if it bites.
3. **Reminder policy**: is next-day-7am acceptable for agent-set reminders
   in phase 2, with custom times deferred to phase 4?
4. **Socket opt-in default**: keep off-by-default (proposed), or on when a
   `$HOCKET_AGENT` env var is set, so `claude mcp` setups don't need a
   config edit?
