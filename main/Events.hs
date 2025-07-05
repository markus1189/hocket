module Events
  ( HocketEvent (..),
    AsyncCommand (..),
    UiCommand (..),
    fetchItemsEvt,
    fetchedItemsEvt,
    archiveItemsEvt,
    archivedItemsEvt,
    asyncActionFailedEvt,
    shiftItemEvt,
    removeItemsEvt,
    setStatusEvt,
    browseItemEvt,
    clearAllFlagsEvt,
    setAllFlagsToArchiveEvt,
    toggleRemindersEvt,
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
  | AsyncActionFailed !(Maybe Text)
  deriving (Show, Eq)

data UiCommand
  = ShiftItem !BookmarkItemId
  | RemoveItems !(Set BookmarkItemId)
  | SetStatus !(Maybe Text)
  | BrowseItem !BookmarkItem
  | ClearAllFlags
  | SetAllFlagsToArchive
  | ToggleReminders
  deriving (Show, Eq)

fetchItemsEvt :: HocketEvent
fetchItemsEvt = HocketAsync FetchItems

fetchedItemsEvt :: POSIXTime -> [BookmarkItem] -> Bool -> HocketEvent
fetchedItemsEvt t itms isAllCollections = HocketAsync (FetchedItems t itms isAllCollections)

archiveItemsEvt :: HocketEvent
archiveItemsEvt = HocketAsync ArchiveItems

archivedItemsEvt :: [BookmarkItemId] -> HocketEvent
archivedItemsEvt bids = HocketAsync (ArchivedItems bids)

asyncActionFailedEvt :: Maybe Text -> HocketEvent
asyncActionFailedEvt maybeMsg = HocketAsync (AsyncActionFailed maybeMsg)

shiftItemEvt :: BookmarkItemId -> HocketEvent
shiftItemEvt bid = HocketUi (ShiftItem bid)

removeItemsEvt :: Set BookmarkItemId -> HocketEvent
removeItemsEvt bids = HocketUi (RemoveItems bids)

setStatusEvt :: Maybe Text -> HocketEvent
setStatusEvt mstatus = HocketUi (SetStatus mstatus)

browseItemEvt :: BookmarkItem -> HocketEvent
browseItemEvt bit = HocketUi (BrowseItem bit)

clearAllFlagsEvt :: HocketEvent
clearAllFlagsEvt = HocketUi ClearAllFlags

setAllFlagsToArchiveEvt :: HocketEvent
setAllFlagsToArchiveEvt = HocketUi SetAllFlagsToArchive

toggleRemindersEvt :: HocketEvent
toggleRemindersEvt = HocketUi ToggleReminders
