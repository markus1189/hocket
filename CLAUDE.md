# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Development Commands

### Using Nix (preferred)
- `nix build` - Build the project
- `nix run -- tui` - Run the TUI application directly
- `nix develop` - Enter development shell with all dependencies
- `hlint` - Run linter
- Use `ormolu -i` to format haskell files in-place, do this before committing

### Using Cabal
- `cabal build` - Build the project
- `cabal test` - Run the test suite
- `cabal build --ghc-options="-Wall -Werror"` - Development build with warnings

### Running the Application
- `hocket tui` - Start the terminal user interface (requires config.dhall)

## Architecture Overview

Hocket is a terminal-based bookmark manager for Raindrop.io built with Haskell and the Brick TUI library.

### Key Components
- **main/hocket.hs** - Main application entry point, UI event handling, and TUI rendering using Brick
- **main/Events.hs** - Event system definitions for async operations and UI commands
- **src/Network/Raindrop.hs** - HTTP client implementation for Raindrop.io REST API
- **src/Network/Bookmark/Types.hs** - Core data types, JSON serialization, and bookmark model
- **src/Network/Bookmark/Ui/State.hs** - Application state management and UI state synchronization
- **src/Network/Bookmark/Ui/Widgets.hs** - Reusable UI helper functions

### Architecture Patterns
- **Event-driven async operations** - UI remains responsive during network calls using BChan and async
- **Single-pane interface with action flags** - Items display pending action indicators instead of being moved to separate panes
- **Lens-based state management** - Uses lens library for clean record access and updates
- **Smart synchronization** - Only fetches items modified since last update to minimize API calls

### Configuration
- Requires `config.dhall` file with Raindrop.io API token and archive collection ID
- Uses Dhall functional configuration language for type-safe config

### Dependencies
- **brick** - Terminal user interface framework
- **wreq** - HTTP client for API communication
- **lens** - Functional record access and manipulation
- **dhall** - Configuration language
- **aeson** - JSON parsing and serialization
- **async** - Concurrent operations

## Code Style Guidelines

- Use `lens` package for record access - avoid underscore-prefixed accessors
- Use `wreq` package for HTTP operations
- Use `tasty` for testing framework
- Follow Brick TUI patterns for UI components
- Run `cabal test` after implementing changes
- Use `rg` (ripgrep) for searching the codebase
