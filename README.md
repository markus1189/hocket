# Hocket

A minimalistic terminal user interface for [Raindrop.io](https://raindrop.io/) written in Haskell.


## Overview

Hocket provides a keyboard-driven terminal interface for efficiently managing your Raindrop.io bookmarks. It's designed for users who prefer command-line tools and want to quickly process their bookmark collection without leaving the terminal.

![Hocket Interface](pics/hocket.png)

## Features

### Core Functionality
- **Single-pane interface with action flags** - View all items with visual indicators for pending actions
- **Real-time synchronization** - Fetch latest bookmarks from Raindrop.io
- **Batch operations** - Archive multiple items and manage reminders at once
- **Smart reminder management** - Set and remove reminders with intelligent toggling
- **Smart updates** - Only fetch items modified since last sync
- **Favorite indicators** - Visual markers (★) for favorite bookmarks
- **Rich item display** - Shows dates, titles, URLs, notes, excerpts, and reminder dates

### Command Line Interface
- **Add bookmarks from CLI** - Add bookmarks directly from terminal
- **Automatic metadata extraction** - Raindrop.io parses title and description automatically
- **Collection targeting** - Specify which collection to add bookmarks to
- **Tag support** - Add multiple tags when creating bookmarks
- **Robust error handling** - Automatic retries with exponential backoff

### Workflow Support
- **Browser integration** - Open bookmarks in your default browser
- **Archive management** - Move items to a designated archive collection
- **Reminder scheduling** - Set reminders for next day at 7:00 AM in your local timezone
- **Keyboard navigation** - Efficient Vi-style navigation
- **Status tracking** - Visual feedback for all operations

## Installation

### Using Nix (Recommended)

```bash
# Build the project
nix build

# Run directly
nix run -- tui

# Install to your profile
nix profile install
```

### Using Cabal

```bash
# Clone the repository
git clone https://github.com/markus1189/hocket.git
cd hocket

# Build and install
cabal build
cabal install
```

## Configuration

### XDG Base Directory Support

Hocket follows the XDG Base Directory Specification for configuration files:

**Current config location**: `~/.config/hocket/config.dhall`

Create your configuration file:

```dhall
{
  _raindropToken = "your-raindrop-test-token-here",
  _archiveCollectionId = 12345
}
: ./schema.dhall
```

### Legacy Support

For backward compatibility, Hocket will still use `./config.dhall` if it exists in your working directory. However, you'll see a warning message encouraging migration to the XDG location.

### Migration from Legacy Config

If you have an existing `config.dhall` in your working directory:

1. Create the XDG config directory: `mkdir -p ~/.config/hocket`
2. Move your config: `mv ./config.dhall ~/.config/hocket/config.dhall`
3. Move the schema: `mv ./schema.dhall ~/.config/hocket/schema.dhall`

The application will automatically create the schema file if it doesn't exist.

### Getting Your Raindrop.io Token

1. Visit [Raindrop.io Settings → Integrations](https://app.raindrop.io/settings/integrations)
2. Click **"+ Create new app"**
3. Name your app (e.g., "Hocket")
4. Click **"Create"**
5. Click on your new app
6. Copy the **"Test token"**

### Finding Your Archive Collection ID

1. Go to [Raindrop.io](https://app.raindrop.io)
2. Create or navigate to your desired archive collection
3. Note the collection ID from the URL (e.g., `app.raindrop.io/my/12345`)

**Security Note:** Keep your token secure and never commit it to version control.

## Usage

### Commands

#### Terminal User Interface
```bash
# Run the interactive TUI (config is loaded from ~/.config/hocket/config.dhall)
hocket tui
```

#### Add Bookmarks from Command Line
```bash
# Add a bookmark to the unsorted collection
hocket add https://example.com

# Add a bookmark to a specific collection
hocket add https://example.com --collection 12345

# Add a bookmark with tags
hocket add https://example.com --tag programming --tag tutorial

# Add a bookmark with collection and tags
hocket add https://example.com --collection 12345 --tag rust --tag cli
```

**Add Command Features:**
- **Automatic metadata extraction** - Title, description, and other metadata are fetched automatically
- **Default collection** - Uses unsorted collection (-1) if not specified
- **Multiple tags** - Add multiple `--tag` flags for multiple tags
- **Retry logic** - Automatically retries failed requests with exponential backoff
- **Error handling** - Clear error messages for configuration and API issues

### Keyboard Controls

#### Navigation
- `↑/↓` or `j/k` - Move up/down in the list
- `J/K` - Jump to next/previous flagged item (skip unflagged items)
- `q` - Quit application

#### Item Actions
- `Space` - Open selected item in browser
- `Enter` - Open item in browser AND mark for archiving
- `e` - Open selected item's edit page in Raindrop.io
- `a` - Flag selected item for archiving
- `s` - Smart reminder toggle: flag for reminder setting (unscheduled items) or removal (scheduled items)
- `u` - Remove flags from selected item (archive, reminder, or removal flags)

#### Bulk Operations
- `r` - Refresh/fetch latest items from Raindrop.io
- `U` - Clear all flags from all items
- `X` - Execute all flagged operations (archive items, set reminders, remove reminders)
- `S` - Toggle showing/hiding items with future reminders
- `v` - Toggle video filter (show only YouTube URLs and items tagged with "video")

## Video Filter

The video filter (`v` key) allows you to quickly focus on video content in your bookmarks. When enabled, only bookmarks that meet the following criteria are displayed:

- **YouTube URLs**: Bookmarks containing "youtube" in the URL (case-insensitive)
- **Video tags**: Bookmarks tagged with "video" (case-insensitive)

### Video Filter Features
- **Toggle functionality**: Press `v` to turn the filter on/off
- **Visual indicator**: Header shows "(VIDEO)" when filter is active
- **Combined with other filters**: Works alongside the reminder visibility toggle (`S`)
- **All operations supported**: Archive, reminder, and other operations work normally on filtered results
- **Non-persistent**: Filter resets when application restarts

### Interface Layout

```
┌─ Hocket (VIDEO): (15|2|1|1) (3) ──────────────────────────────────┐
│   2025-01-15: ★ Important Article Title         reddit.com/r/... │
│   2025-01-14:   Regular Bookmark                github.com/...   │
│ A 2025-01-13:   Item flagged for archive        example.com/...  │
│ R 2025-01-12:   Item flagged for reminder       example.com/...  │
│ r 2025-01-11:   Item flagged to remove reminder news.ycombinator...│
│   2025-01-10:   Item with existing reminder     stackoverflow.com/│
├──────────────────────────────────────────────────────────────────┤
└─ REMINDER 2025-01-12 EXCERPT: This is a sample excerpt ──────────┘
│                                                    Last: 14:32:18 │
│ Status: setting reminders                                         │
└────────────────────────────────────────────────────────────────────┘
```

#### Visual Elements
- **A** - Flag indicating item is pending archive action
- **R** - Flag indicating item is pending reminder setting (blue color)
- **r** - Flag indicating item is pending reminder removal (red color)
- **★** - Indicates favorite bookmarks
- **Header counts** - `(normal|archive|remind_set|remind_remove) (existing_reminders)`
- **Date** - When the bookmark was created (shows reminder date when present)
- **Title** - Bookmark title or URL if no title available
- **Domain** - Truncated URL showing the domain and path
- **Bottom section** - Shows notes, reminder dates, and excerpts for the selected item
- **Status bar** - Last update time and current operation status

## Reminder Management

### Smart Reminder Toggle

The `s` key provides intelligent reminder management based on the item's current state:

- **Unscheduled items**: Press `s` to flag for reminder setting (shows blue "R ")
- **Items with existing reminders**: Press `s` to flag for reminder removal (shows red "r ")

### Reminder Scheduling

- **Default time**: All reminders are set for the next day at 7:00 AM in your local timezone
- **Automatic conversion**: Times are converted to UTC for API storage
- **Visual feedback**: Items with reminders show the reminder date instead of creation date

### Workflow Example

```bash
# Set reminders for important items
1. Navigate to an unscheduled item
2. Press 's' - item shows blue "R " flag
3. Repeat for other items
4. Press 'X' - executes all flagged operations

# Remove unwanted reminders
1. Navigate to an item with existing reminder
2. Press 's' - item shows red "r " flag  
3. Press 'X' - removes the reminder

# Mixed operations
1. Flag some items for archive with 'a'
2. Flag some items for reminders with 's'
3. Flag some existing reminders for removal with 's'
4. Press 'X' - executes all operations at once
```

### Display Features

- **Color coding**: Blue for setting reminders, red for removing them
- **Status tracking**: Real-time count of flagged operations in header
- **Jump navigation**: `J/K` keys jump between all flagged items (archive and reminder)
- **Toggle visibility**: `S` key shows/hides items with future reminders

## Technical Details

### Architecture
- **Language**: Haskell (GHC 9.6+)
- **UI Framework**: [Brick](https://github.com/jtdaugherty/brick) (terminal UI library)
- **HTTP Client**: [Wreq](http://www.serpentine.com/wreq/) for API communication
- **Configuration**: [Dhall](https://dhall-lang.org/) functional configuration language
- **Concurrency**: Async operations for non-blocking UI

### API Integration
- **Raindrop.io REST API v1** - Full integration with bookmark and reminder management
- **Individual reminder operations** - Each reminder uses separate API calls as required by Raindrop.io
- **Smart pagination** - Efficiently handles large bookmark collections  
- **Rate limiting** - Respects API limits with exponential backoff retry logic
- **Error handling** - Graceful degradation on network issues
- **Automatic metadata parsing** - Uses Raindrop's `pleaseParse` feature for rich bookmark data

### Data Management
- **Local state** - In-memory bookmark cache with smart updates
- **Conflict resolution** - Handles concurrent modifications
- **Performance** - Optimized for collections with thousands of items

## Development

### Building from Source

```bash
# Install dependencies
cabal update

# Build the project
cabal build

# Run tests
cabal test

# Development build with warnings
cabal build --ghc-options="-Wall -Werror"
```

### Project Structure

```
hocket/
├── main/                   # Main application
│   ├── hocket.hs          # Entry point and UI logic
│   └── Events.hs          # Event system definitions
├── src/Network/
│   ├── Raindrop.hs        # API client implementation
│   └── Bookmark/
│       ├── Types.hs       # Data types and JSON instances
│       └── Ui/
│           ├── State.hs   # Application state management
│           └── Widgets.hs # UI helper functions
└── test/                  # Test suite
```

### Contributing

Contributions are welcome! Please:

1. Follow the existing code style (see `CONVENTIONS.md`)
2. Add tests for new functionality
3. Update documentation as needed
4. Ensure `cabal test` passes

#### Code Style Guidelines
- Use `lens` for record access (avoid `_` prefixed accessors)
- Prefer `wreq` for HTTP operations
- Use `tasty` for testing
- Follow Brick patterns for UI components

## Limitations

Current limitations that may be addressed in future versions:

- **Single collection focus** - Primarily works with unsorted bookmarks
- **Browser dependency** - Requires external browser for opening links
- **Terminal only** - No GUI version available
- **Limited search** - No built-in search functionality

## Troubleshooting

### Common Issues

**"Authentication failed"**
- Verify your Raindrop.io token in `~/.config/hocket/config.dhall`
- Ensure the token has not expired
- Check your internet connection

**"Collection not found"**
- Verify the `archiveCollectionId` in your config
- Ensure the collection exists and is accessible

**"Items not appearing"**
- Press `r` to refresh/fetch latest items
- Check that items exist in your Raindrop.io unsorted collection
- Verify your account has bookmarks

**"Reminders not working"**
- Ensure you have proper permissions to modify bookmarks
- Check that the reminder time (next day 7:00 AM) is in the future
- Verify your system timezone is configured correctly
- Press `r` to refresh and see updated reminder states

**"Reminder flags showing incorrectly"**
- Press `S` to toggle reminder visibility and refresh the display
- Use `r` to refresh from Raindrop.io to sync latest reminder states

### Getting Help

- Check existing [GitHub Issues](https://github.com/markus1189/hocket/issues)
- Review the `CONVENTIONS.md` file for development guidelines
- Consult the [Brick documentation](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst) for UI-related questions

## License

BSD3 - See [LICENSE](LICENSE) file for details.

## Acknowledgments

- [Raindrop.io](https://raindrop.io/) for providing an excellent bookmark service with a great API
- [Brick](https://github.com/jtdaugherty/brick) for making terminal UI development in Haskell approachable
- The Haskell community for excellent libraries and tooling