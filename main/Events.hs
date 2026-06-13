module Events
  ( HocketEvent (..),
    AsyncCommand (..),
    UiCommand (..),
    FilterInput (..),
    fetchItemsEvt,
    fetchedItemsEvt,
    archiveItemsEvt,
    archivedItemsEvt,
    setRemindersEvt,
    remindersSetEvt,
    removeRemindersEvt,
    remindersRemovedEvt,
    asyncActionFailedEvt,
    shiftItemEvt,
    shiftItemReminderEvt,
    removeItemsEvt,
    setStatusEvt,
    browseItemEvt,
    editItemInBrowserEvt,
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
  )
where

import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import Network.Bookmark.Types

data HocketEvent
  = HocketAsync !AsyncCommand
  | HocketUi !UiCommand
  deriving (Show, Eq)

data AsyncCommand
  = FetchItems
  | FetchedItems !POSIXTime ![BookmarkItem] !Bool
  | ArchiveItems
  | ArchivedItems ![BookmarkItemId]
  | SetReminders
  | RemindersSet ![BookmarkItemId]
  | RemoveReminders
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
  | ClearAllFlags
  | SetAllFlagsToArchive
  | ToggleReminders
  | ToggleVideoFilter
  | ToggleInvertedVideoFilter
  | FilterInput !FilterInput
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

archiveItemsEvt :: HocketEvent
archiveItemsEvt = HocketAsync ArchiveItems

archivedItemsEvt :: [BookmarkItemId] -> HocketEvent
archivedItemsEvt bids = HocketAsync (ArchivedItems bids)

setRemindersEvt :: HocketEvent
setRemindersEvt = HocketAsync SetReminders

remindersSetEvt :: [BookmarkItemId] -> HocketEvent
remindersSetEvt bids = HocketAsync (RemindersSet bids)

removeRemindersEvt :: HocketEvent
removeRemindersEvt = HocketAsync RemoveReminders

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
