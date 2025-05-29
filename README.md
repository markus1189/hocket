# Hocket #

Hocket - The Haskell [Raindrop.io](https://raindrop.io/) client

[![Build Status](https://travis-ci.org/markus1189/hocket.png?branch=master)](https://travis-ci.org/markus1189/hocket)

## About ##

Hocket is a minimalistic [Raindrop.io](https://raindrop.io/) client that works on the command line using [brick](https://github.com/jtdaugherty/brick), written in Haskell.

It's designed to match a focused workflow with Raindrop.io, providing a clean terminal interface for managing your bookmarks.

### Features ###

- **View unread bookmark items** - Browse your unsorted bookmarks in a clean list
- **Archive items** - Move items to a special archive collection (mark as read)
- **Keyboard-driven interface** - Navigate efficiently without touching the mouse
- **Real-time updates** - Fetch latest bookmarks from Raindrop.io
- **Dual-pane view** - Separate views for unread items and items pending archive

### Screenshots ###

![Hocket Interface](pics/hocket.png)

The interface shows two main sections:
- **Unread items** (top) - Your new bookmarks from Raindrop.io
- **Items to archive** (bottom) - Items you've marked for archiving

### Keyboard Controls ###

- `u` - Update/fetch latest items from Raindrop.io
- `Space` - Browse selected item in your default browser
- `Enter` - Browse item and mark for archiving
- `d` - Mark item for archiving (without browsing)
- `A` - Archive all pending items
- `Tab` - Switch between unread and pending lists
- `q` - Quit application
- `↑/↓` or `j/k` - Navigate items in current list

## Installation ##

Hocket can be installed using `nix`:

```bash
# Build from source
nix build

# Run directly
nix run
```

## Authentication ##

Hocket requires a `config.dhall` file in the current directory to authenticate with Raindrop.io:

```dhall
{
  _raindropToken = "your-raindrop-test-token-here"
}
```

### How to get a Raindrop.io Test Token ###

1. Go to your Raindrop.io settings: https://app.raindrop.io/settings/integrations
2. Click on **"+ Create new app"**
3. Give it a name (e.g., "Hocket")
4. Click **"Create"**
5. You'll see your app listed. Click on it
6. Copy the **"Test token"** - this is what you'll use for `_raindropToken` in your `config.dhall`

**Note:** Keep your token secure and don't commit it to version control!

## Usage ##

1. Create your `config.dhall` file with your Raindrop.io token
2. Run `hocket` in the same directory
3. Press `u` to fetch your latest bookmarks
4. Use keyboard controls to navigate and manage your bookmarks

## Technical Details ##

- **Language:** Haskell
- **UI Framework:** [brick](https://github.com/jtdaugherty/brick) (terminal UI)
- **API:** Raindrop.io REST API v1
- **Configuration:** [Dhall](https://dhall-lang.org/) configuration language

## Development Status ##

Hocket is functional but minimalistic, designed to fit a specific workflow. It focuses on the core use case of processing unread bookmarks efficiently.

**Current limitations:**
- Only works with the "Unsorted" collection (ID: -1)
- No support for tags, favorites, or other Raindrop.io features
- Archive moves items to a hardcoded collection

## Contributing ##

Contributions are welcome! The codebase is designed to be simple and focused, but improvements and bug fixes are appreciated.

## License ##

BSD3 - see LICENSE file for details.
