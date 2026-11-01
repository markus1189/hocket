# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Development Commands

### Using Nix (preferred)
- `nix build` - Build the project
- `nix run -- tui` - Run the TUI application directly
- `nix develop` - Enter development shell with all dependencies
- `hlint` - Run linter, do this before committing
- Use `ormolu -i` to format haskell files in-place, do this before committing

### Using Cabal
- `cabal build` - Build the project
- `cabal test` - Run the test suite
- `cabal build --ghc-options="-Wall -Werror"` - Development build with warnings
- After HLS/hie-bios sessions in the checkout, `cabal test` once failed to link (undefined references from the test objects into `lib:core`) while a clean `nix build` was green; `rm -r dist-newstyle` fixed it

### Running the Application
- `hocket tui` - Start the terminal user interface (requires config.dhall)
- `hocket tui --fixture FILE` - Run against a fake Raindrop; never use the real account to try changes out. It never starts a browser or clipboard tool; `--fixture-desktop-log OUT` records those calls as JSON lines

### End-to-end tests
- `test/e2e.sh [SCENARIO...]` - Fixture sessions driven over the agent socket; run before merging any change to workers, handlers or the agent socket. `HOCKET=path` skips the nix build

### Benchmarks
- `bench/compare.sh BASE_REV` - HEAD's benchmarks against BASE_REV's, interleaved on pinned cores; fails on a >10% slowdown or a vanished name. Run before merging changes to `settle`, `takeSnapshot` or `validateWrite`, on a quiet machine; never rename a benchmark

## Architecture Overview

Hocket is a terminal-based bookmark manager for Raindrop.io built with Haskell and the Brick TUI library.

### Key Components
- **main/hocket.hs** - Entry point: CLI parsing, config loading and wiring
- **src/Hocket/Shell/Tui/{App,View}.hs** - The brick TUI: event handling and the shell's `AppState`, and rendering
- **src/core/Hocket/App/{Msg,Effect,Update}.hs** - The TUI core: keys, agent writes and worker results arrive as `Msg`; the pure `update` answers the new state and `Effect`s
- **src/Hocket/Shell/Interpret.hs** - Runs `Effect`s: fetch and batch workers, browser, clipboard
- **src/Hocket/Shell/Tui/Keymap.hs** - Normal-mode keys as `Intent`s (`keyIntents`)
- **src/Hocket/Adapter/Raindrop.hs** - HTTP client implementation for Raindrop.io REST API
- **src/core/Hocket/Domain/Bookmark.hs** - Core data types, JSON serialization, and bookmark model
- **src/core/Hocket/App/Model.hs** - Application state management and UI state synchronization
- **src/core/Hocket/Domain/Filter.hs** - Reusable UI helper functions
- **src/Hocket/Shell/Agent/Server.hs** - Unix-domain socket server (IO shell) for the agent control socket
- **src/Hocket/Shell/Agent/Client.hs** - Client for the agent control socket, exposed as the `hocket agent` subcommand
- **src/core/Hocket/Agent/Protocol.hs** - Agent control-socket protocol: command decode/encode, write validation, read serving
- **src/core/Hocket/Agent/Snapshot.hs** - Serializable read-only mirror of TUI state for the agent socket
- **docs/RPC.md** - Full spec of the agent control-socket protocol
- **skills/hocket-rpc/SKILL.md** - Agent-facing operating guide for the control socket (install with `cp -r skills/hocket-rpc ~/.claude/skills/`)

### Architecture Patterns
- **Event-driven async operations** - UI remains responsive during network calls using BChan and async
- **Single-pane interface with action flags** - Items display pending action indicators instead of being moved to separate panes
- **Lens-based state management** - Uses lens library for clean record access and updates
- **Smart synchronization** - Only fetches items modified since last update to minimize API calls
- **Two libraries** - `library core` (`src/core/`: domain and application, pure) has no brick, servant, network or async in its dependencies, so importing them there fails the build; the main library (`src/`) holds ports, adapters and the shell
- **Agent control socket** - Optional Unix-domain socket (JSON over newline-delimited lines) for external tools/agents to observe and drive the TUI; reads served from a snapshot mirror, writes injected as regular UI events. Spec: `docs/RPC.md`. Enabled via `--agent-socket` / `--agent-socket-path`.

### Configuration
- Requires `config.dhall` file with Raindrop.io API token and archive collection ID
- Uses Dhall functional configuration language for type-safe config

### Dependencies
- **brick** - Terminal user interface framework
- **servant-client** - HTTP client; the Raindrop API is a servant type in `Hocket.Adapter.Raindrop.Api`
- **lens** - Functional record access and manipulation
- **dhall** - Configuration language
- **aeson** - JSON parsing and serialization
- **async** - Concurrent operations

## GitHub Actions

- Automated flake updates and build verification via workflows in `.github/workflows/`

## Code Style Guidelines

- Use `lens` package for record access - avoid underscore-prefixed accessors
- HTTP goes through `servant-client`: endpoints in `Hocket.Adapter.Raindrop.Api`, retries in `Hocket.Adapter.Raindrop`, and the app reaches Raindrop only via `RaindropPort` (`Hocket.Port`)
- Use `tasty` for testing framework
- Follow Brick TUI patterns for UI components
- Run `cabal test` after implementing changes
- Use `rg` (ripgrep) for searching the codebase
- Update README when you change keybindings
- GitHub workflows use `.yml` extension (not `.yaml`)
