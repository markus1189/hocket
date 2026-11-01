module Events
  ( HocketEvent (..),
    AsyncCommand (..),
    UiCommand (..),
    executeBatchEvt,
    executeBatchDoneEvt,
    archivedItemsEvt,
    remindersSetEvt,
    remindersRemovedEvt,
    setStatusEvt,
  )
where

import Data.Text (Text)
import Data.Time (UTCTime)
import Network.Bookmark.App.Msg (Msg)
import Network.Bookmark.Batch (StepOutcome)
import Network.Bookmark.Types

data HocketEvent
  = HocketAsync !AsyncCommand
  | HocketUi !UiCommand
  | -- | Handled by 'Network.Bookmark.App.Update.update'; the other two are
    -- the handler groups Phase 3 has not moved yet.
    HocketMsg !Msg
  deriving (Show, Eq)

data AsyncCommand
  = ExecuteBatch
  | ExecuteBatchDone
  | ArchivedItems !(StepOutcome BookmarkItemId Text)
  | RemindersSet !(StepOutcome (BookmarkItemId, UTCTime) Text)
  | RemindersRemoved !(StepOutcome BookmarkItemId Text)
  deriving (Show, Eq)

-- | A worker's progress line; the fetch and batch steps move it into update.
newtype UiCommand = SetStatus (Maybe Text)
  deriving (Show, Eq)

executeBatchEvt :: HocketEvent
executeBatchEvt = HocketAsync ExecuteBatch

executeBatchDoneEvt :: HocketEvent
executeBatchDoneEvt = HocketAsync ExecuteBatchDone

archivedItemsEvt :: StepOutcome BookmarkItemId Text -> HocketEvent
archivedItemsEvt outcome = HocketAsync (ArchivedItems outcome)

remindersSetEvt :: StepOutcome (BookmarkItemId, UTCTime) Text -> HocketEvent
remindersSetEvt outcome = HocketAsync (RemindersSet outcome)

remindersRemovedEvt :: StepOutcome BookmarkItemId Text -> HocketEvent
remindersRemovedEvt outcome = HocketAsync (RemindersRemoved outcome)

setStatusEvt :: Maybe Text -> HocketEvent
setStatusEvt mstatus = HocketUi (SetStatus mstatus)
