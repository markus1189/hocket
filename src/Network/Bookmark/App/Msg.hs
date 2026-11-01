-- | What the TUI core reacts to (plan §4.2). Every flag, batch and view
-- action of the keyboard and the agent arrives as an 'Intent' (G2);
-- navigation and the filter editor follow in Phase 4. Phase 3 moves the
-- handlers over one group at a time; 'Msg' grows with them.
module Network.Bookmark.App.Msg
  ( Now (..),
    Target (..),
    Intent (..),
    Msg (..),
  )
where

import Data.Text (Text)
import Data.Time (TimeZone, UTCTime)
import Network.Bookmark.Agent.Protocol (FlagAction)
import Network.Bookmark.Types (BookmarkItemId)
import Network.Bookmark.Ui.State (VideoFilterMode)

-- | Sampled by the shell only, once per key press or agent request.
data Now = Now {nowUtc :: !UTCTime, nowZone :: !TimeZone}
  deriving (Show, Eq)

-- | Keys act on the focused item, the agent names one.
data Target = Selected | ById !BookmarkItemId
  deriving (Show, Eq)

data Intent
  = -- | Key @a@: archive, whatever the flag was.
    SetArchive !Target
  | -- | The flag half of Enter: toggle archive.
    ToggleArchive !Target
  | -- | Key @s@.
    ToggleReminder !Target !Now
  | -- | Key @u@.
    Unflag !Target
  | -- | Agent @set_flag@: idempotent, unlike the keys.
    SetFlag !BookmarkItemId !FlagAction !Now
  | ClearAllFlags
  | FlagAllArchive
  | Execute
  | Refresh
  | -- | Space, the browse half of Enter, agent @open_item@.
    Open !Target
  | OpenAndFlag !BookmarkItemId
  | EditInRaindrop !Target
  | CopyUrl !Target
  | Select !BookmarkItemId
  | SetFilter !Text
  | SetVideoFilter !VideoFilterMode
  | ToggleVideoOnly
  | ToggleHideVideos
  | ToggleFutureReminders
  | SetShowFutureReminders !Bool
  | SetStatus !Text
  | Quit
  deriving (Show, Eq)

data Msg
  = Intent !Intent
  | -- | A browser or clipboard call failed; the text goes to the status line.
    SystemFailed !Text
  | -- | The cleaned URL that reached the clipboard.
    Copied !Text
  deriving (Show, Eq)
