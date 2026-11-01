module Events
  ( HocketEvent (..),
    AsyncCommand (..),
    UiCommand (..),
    fetchItemsEvt,
    fetchCompletedEvt,
    fetchFailedEvt,
    executeBatchEvt,
    executeBatchDoneEvt,
    archivedItemsEvt,
    remindersSetEvt,
    remindersRemovedEvt,
    setStatusEvt,
    toggleRemindersEvt,
    toggleVideoFilterEvt,
    toggleInvertedVideoFilterEvt,
    setFilterQueryEvt,
    setVideoFilterModeEvt,
    setShowFutureRemindersEvt,
    selectItemEvt,
    setAgentClientsEvt,
    setAgentErrorEvt,
  )
where

import Data.Text (Text)
import Data.Time (UTCTime)
import Network.Bookmark.App.Msg (Msg)
import Network.Bookmark.Batch (StepOutcome)
import Network.Bookmark.Types
import Network.Bookmark.Ui.State (VideoFilterMode)

data HocketEvent
  = HocketAsync !AsyncCommand
  | HocketUi !UiCommand
  | -- | Handled by 'Network.Bookmark.App.Update.update'; the other two are
    -- the handler groups Phase 3 has not moved yet.
    HocketMsg !Msg
  deriving (Show, Eq)

data AsyncCommand
  = FetchItems
  | -- | The one terminal message of a fetch: all pages, and whether it was
    -- incremental. Only this or 'FetchFailed' frees the fetch slot.
    FetchCompleted !Bool ![BookmarkItemBatch]
  | FetchFailed !(Maybe Text)
  | ExecuteBatch
  | ExecuteBatchDone
  | ArchivedItems !(StepOutcome BookmarkItemId Text)
  | RemindersSet !(StepOutcome (BookmarkItemId, UTCTime) Text)
  | RemindersRemoved !(StepOutcome BookmarkItemId Text)
  deriving (Show, Eq)

data UiCommand
  = SetStatus !(Maybe Text)
  | ToggleReminders
  | ToggleVideoFilter
  | ToggleInvertedVideoFilter
  | -- Agent-socket commands: idempotent variants of the keyboard toggles.
    SetFilterQuery !Text
  | SetVideoFilterMode !VideoFilterMode
  | SetShowFutureReminders !Bool
  | SelectItem !BookmarkItemId
  | SetAgentClients !Int
  | SetAgentError !(Maybe Text)
  deriving (Show, Eq)

fetchItemsEvt :: HocketEvent
fetchItemsEvt = HocketAsync FetchItems

fetchCompletedEvt :: Bool -> [BookmarkItemBatch] -> HocketEvent
fetchCompletedEvt isAllCollections batches = HocketAsync (FetchCompleted isAllCollections batches)

fetchFailedEvt :: Maybe Text -> HocketEvent
fetchFailedEvt err = HocketAsync (FetchFailed err)

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

toggleRemindersEvt :: HocketEvent
toggleRemindersEvt = HocketUi ToggleReminders

toggleVideoFilterEvt :: HocketEvent
toggleVideoFilterEvt = HocketUi ToggleVideoFilter

toggleInvertedVideoFilterEvt :: HocketEvent
toggleInvertedVideoFilterEvt = HocketUi ToggleInvertedVideoFilter

setFilterQueryEvt :: Text -> HocketEvent
setFilterQueryEvt q = HocketUi (SetFilterQuery q)

setVideoFilterModeEvt :: VideoFilterMode -> HocketEvent
setVideoFilterModeEvt m = HocketUi (SetVideoFilterMode m)

setShowFutureRemindersEvt :: Bool -> HocketEvent
setShowFutureRemindersEvt b = HocketUi (SetShowFutureReminders b)

selectItemEvt :: BookmarkItemId -> HocketEvent
selectItemEvt bid = HocketUi (SelectItem bid)

setAgentClientsEvt :: Int -> HocketEvent
setAgentClientsEvt n = HocketUi (SetAgentClients n)

setAgentErrorEvt :: Maybe Text -> HocketEvent
setAgentErrorEvt e = HocketUi (SetAgentError e)
