module Events
  ( HocketEvent (..),
    AsyncCommand (..),
    UiCommand (..),
    FilterInput (..),
    fetchItemsEvt,
    fetchedItemsEvt,
    executeBatchEvt,
    executeBatchDoneEvt,
    archivedItemsEvt,
    remindersSetEvt,
    remindersRemovedEvt,
    asyncActionFailedEvt,
    shiftItemEvt,
    shiftItemReminderEvt,
    removeItemsEvt,
    setStatusEvt,
    browseItemEvt,
    editItemInBrowserEvt,
    copyUrlEvt,
    clearAllFlagsEvt,
    setAllFlagsToArchiveEvt,
    toggleRemindersEvt,
    toggleVideoFilterEvt,
    toggleInvertedVideoFilterEvt,
    enterFilterModeEvt,
    lockFilterEvt,
    cancelFilterEvt,
    filterCharEvt,
    filterBackspaceEvt,
    setPendingActionEvt,
    setFilterQueryEvt,
    setVideoFilterModeEvt,
    setShowFutureRemindersEvt,
    selectItemEvt,
    openItemByIdEvt,
    setAgentClientsEvt,
  )
where

import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Network.Bookmark.Types
import Network.Bookmark.Ui.State (VideoFilterMode)

data HocketEvent
  = HocketAsync !AsyncCommand
  | HocketUi !UiCommand
  deriving (Show, Eq)

data AsyncCommand
  = FetchItems
  | FetchedItems !POSIXTime ![BookmarkItem] !Bool
  | ExecuteBatch
  | ExecuteBatchDone
  | ArchivedItems ![BookmarkItemId]
  | RemindersSet ![BookmarkItemId]
  | RemindersRemoved ![BookmarkItemId]
  | AsyncActionFailed !(Maybe Text)
  deriving (Show, Eq)

data UiCommand
  = ShiftItem !BookmarkItemId
  | ShiftItemReminder !BookmarkItemId
  | RemoveItems !(Set BookmarkItemId)
  | SetStatus !(Maybe Text)
  | BrowseItem !BookmarkItem
  | EditItemInBrowser !BookmarkItem
  | CopyUrl !BookmarkItem
  | ClearAllFlags
  | SetAllFlagsToArchive
  | ToggleReminders
  | ToggleVideoFilter
  | ToggleInvertedVideoFilter
  | FilterInput !FilterInput
  | -- Agent-socket commands: idempotent variants of the keyboard toggles.
    SetPendingAction !BookmarkItemId !PendingAction
  | SetFilterQuery !Text
  | SetVideoFilterMode !VideoFilterMode
  | SetShowFutureReminders !Bool
  | SelectItem !BookmarkItemId
  | OpenItemById !BookmarkItemId
  | SetAgentClients !Int
  deriving (Show, Eq)

data FilterInput
  = EnterFilter
  | LockFilter
  | DoCancelFilter
  | FilterChar !Char
  | FilterBackspace
  deriving (Show, Eq)

fetchItemsEvt :: HocketEvent
fetchItemsEvt = HocketAsync FetchItems

fetchedItemsEvt :: POSIXTime -> [BookmarkItem] -> Bool -> HocketEvent
fetchedItemsEvt t itms isAllCollections = HocketAsync (FetchedItems t itms isAllCollections)

executeBatchEvt :: HocketEvent
executeBatchEvt = HocketAsync ExecuteBatch

executeBatchDoneEvt :: HocketEvent
executeBatchDoneEvt = HocketAsync ExecuteBatchDone

archivedItemsEvt :: [BookmarkItemId] -> HocketEvent
archivedItemsEvt bids = HocketAsync (ArchivedItems bids)

remindersSetEvt :: [BookmarkItemId] -> HocketEvent
remindersSetEvt bids = HocketAsync (RemindersSet bids)

remindersRemovedEvt :: [BookmarkItemId] -> HocketEvent
remindersRemovedEvt bids = HocketAsync (RemindersRemoved bids)

asyncActionFailedEvt :: Maybe Text -> HocketEvent
asyncActionFailedEvt maybeMsg = HocketAsync (AsyncActionFailed maybeMsg)

shiftItemEvt :: BookmarkItemId -> HocketEvent
shiftItemEvt bid = HocketUi (ShiftItem bid)

shiftItemReminderEvt :: BookmarkItemId -> HocketEvent
shiftItemReminderEvt bid = HocketUi (ShiftItemReminder bid)

removeItemsEvt :: Set BookmarkItemId -> HocketEvent
removeItemsEvt bids = HocketUi (RemoveItems bids)

setStatusEvt :: Maybe Text -> HocketEvent
setStatusEvt mstatus = HocketUi (SetStatus mstatus)

browseItemEvt :: BookmarkItem -> HocketEvent
browseItemEvt bit = HocketUi (BrowseItem bit)

editItemInBrowserEvt :: BookmarkItem -> HocketEvent
editItemInBrowserEvt bit = HocketUi (EditItemInBrowser bit)

copyUrlEvt :: BookmarkItem -> HocketEvent
copyUrlEvt bit = HocketUi (CopyUrl bit)

clearAllFlagsEvt :: HocketEvent
clearAllFlagsEvt = HocketUi ClearAllFlags

setAllFlagsToArchiveEvt :: HocketEvent
setAllFlagsToArchiveEvt = HocketUi SetAllFlagsToArchive

toggleRemindersEvt :: HocketEvent
toggleRemindersEvt = HocketUi ToggleReminders

toggleVideoFilterEvt :: HocketEvent
toggleVideoFilterEvt = HocketUi ToggleVideoFilter

toggleInvertedVideoFilterEvt :: HocketEvent
toggleInvertedVideoFilterEvt = HocketUi ToggleInvertedVideoFilter

enterFilterModeEvt :: HocketEvent
enterFilterModeEvt = HocketUi (FilterInput EnterFilter)

lockFilterEvt :: HocketEvent
lockFilterEvt = HocketUi (FilterInput LockFilter)

cancelFilterEvt :: HocketEvent
cancelFilterEvt = HocketUi (FilterInput DoCancelFilter)

filterCharEvt :: Char -> HocketEvent
filterCharEvt c = HocketUi (FilterInput (FilterChar c))

filterBackspaceEvt :: HocketEvent
filterBackspaceEvt = HocketUi (FilterInput FilterBackspace)

setPendingActionEvt :: BookmarkItemId -> PendingAction -> HocketEvent
setPendingActionEvt bid act = HocketUi (SetPendingAction bid act)

setFilterQueryEvt :: Text -> HocketEvent
setFilterQueryEvt q = HocketUi (SetFilterQuery q)

setVideoFilterModeEvt :: VideoFilterMode -> HocketEvent
setVideoFilterModeEvt m = HocketUi (SetVideoFilterMode m)

setShowFutureRemindersEvt :: Bool -> HocketEvent
setShowFutureRemindersEvt b = HocketUi (SetShowFutureReminders b)

selectItemEvt :: BookmarkItemId -> HocketEvent
selectItemEvt bid = HocketUi (SelectItem bid)

openItemByIdEvt :: BookmarkItemId -> HocketEvent
openItemByIdEvt bid = HocketUi (OpenItemById bid)

setAgentClientsEvt :: Int -> HocketEvent
setAgentClientsEvt n = HocketUi (SetAgentClients n)
