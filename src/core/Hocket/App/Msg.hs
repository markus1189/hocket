{-# LANGUAGE LambdaCase #-}

-- | What the TUI core reacts to (plan §4.2). Every flag, batch and view
-- action of the keyboard and the agent arrives as an 'Intent' (G2);
-- navigation and the filter editor follow in Phase 4. Phase 3 moves the
-- handlers over one group at a time; 'Msg' grows with them.
module Hocket.App.Msg
  ( Now (..),
    Target (..),
    Intent (..),
    Msg (..),
    StepResult (..),
    publishes,
  )
where

import Data.Text (Text)
import Data.Time (TimeZone, UTCTime)
import Hocket.Agent.Protocol (FlagAction)
import Hocket.App.Batch (StepOutcome)
import Hocket.App.Model (BatchStep, VideoFilterMode)
import Hocket.App.Nav (Nav)
import Hocket.Domain.Bookmark (BookmarkItemBatch, BookmarkItemId)

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
  | -- | Key @/@: the filter editor takes the keys.
    FilterEnter
  | -- | The editor's text after a keystroke; filters as you type.
    FilterText !Text
  | -- | Enter in the editor: keep the query, leave the editor.
    FilterLock
  | -- | Esc in the editor: clear the query, leave the editor.
    FilterCancel
  | -- | Agent @set_filter@: the whole query in one step, editor left.
    SetFilter !Text
  | SetVideoFilter !VideoFilterMode
  | ToggleVideoOnly
  | ToggleHideVideos
  | ToggleFutureReminders
  | SetShowFutureReminders !Bool
  | SetStatus !Text
  | -- | Move the selection; the shell supplies the list viewport's height in
    -- rows, which page moves are measured in.
    Navigate !Int !Nav
  | Quit
  deriving (Show, Eq)

data Msg
  = Intent !Intent
  | -- | An agent write with the seq the server gave it: the 'Intent', and
    -- the seq becomes the model's applied seq.
    AgentWrite !Int !Intent
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
  | -- | The batch's one terminal message, with the error that ended it early
    -- (an exception, a cancel); update writes the summary from the
    -- 'StepDone's. The only thing that frees the batch slot.
    BatchFinished !(Maybe Text)
  deriving (Show, Eq)

-- | Whether the event loops publish a snapshot after this message. Not
-- after a client count: connects and disconnects would move the version
-- an agent waits on, and the header is not part of the snapshot.
publishes :: Msg -> Bool
publishes = \case
  AgentClients _ -> False
  _ -> True

-- | What a batch step got done, per step: set-reminders carries the times
-- that were sent (B3).
data StepResult
  = Archived !(StepOutcome BookmarkItemId Text)
  | RemindersSetAt !(StepOutcome (BookmarkItemId, UTCTime) Text)
  | RemindersRemoved !(StepOutcome BookmarkItemId Text)
  deriving (Show, Eq)
