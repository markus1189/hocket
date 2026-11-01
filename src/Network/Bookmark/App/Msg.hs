-- | What the TUI core reacts to (plan §4.2). Every flag, batch and view
-- action of the keyboard and the agent arrives as an 'Intent' (G2);
-- navigation and the filter editor follow in Phase 4. Phase 3 moves the
-- handlers over one group at a time; 'Msg' grows with them.
module Network.Bookmark.App.Msg
  ( Now (..),
    Target (..),
    Intent (..),
    Msg (..),
    StepResult (..),
  )
where

import Data.Text (Text)
import Data.Time (TimeZone, UTCTime)
import Network.Bookmark.Agent.Protocol (FlagAction)
import Network.Bookmark.Batch (StepOutcome)
import Network.Bookmark.Types (BookmarkItemBatch, BookmarkItemId)
import Network.Bookmark.Ui.State (BatchStep, VideoFilterMode)

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
  | -- | Clients connected to the agent socket right now.
    AgentClients !Int
  | -- | The agent socket server stopped; the reason stays in the header.
    AgentServerDied !Text
  | -- | The one terminal message of a fetch: whether it was incremental, and
    -- every page. Only this or 'FetchFailed' frees the fetch slot.
    FetchCompleted !Bool ![BookmarkItemBatch]
  | FetchFailed !(Maybe Text)
  | -- | A batch step begins; its name goes to the status line.
    BatchStepStarted !BatchStep
  | StepDone !StepResult
  | -- | The batch's one terminal message, with its summary ('Nothing' when
    -- every step was clean). The only thing that frees the batch slot.
    BatchFinished !(Maybe Text)
  deriving (Show, Eq)

-- | What a batch step got done, per step: set-reminders carries the times
-- that were sent (B3).
data StepResult
  = Archived !(StepOutcome BookmarkItemId Text)
  | RemindersSetAt !(StepOutcome (BookmarkItemId, UTCTime) Text)
  | RemindersRemoved !(StepOutcome BookmarkItemId Text)
  deriving (Show, Eq)
