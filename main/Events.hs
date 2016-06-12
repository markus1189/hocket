module Events (HocketEvent(..)
              ,InternalEvent(..)
              ,AsyncCommand(..)
              ,UiCommand(..)
              ,fetchItemsEvt
              ,fetchedItemsEvt
              ,archiveItemsEvt
              ,archivedItemsEvt
              ,asyncActionFailedEvt
              ,shiftItemEvt
              ,removeItemsEvt
              ,setStatusEvt
              ,browseItemEvt
              ) where

import           Data.Set (Set)
import           Data.Text (Text)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Graphics.Vty (Event)

import           Network.Pocket

data HocketEvent = Internal InternalEvent
                 | VtyEvent Event
                 deriving (Show,Eq)

data InternalEvent = InternalAsync AsyncCommand
                   | InternalUi UiCommand
                   deriving (Show,Eq)

data AsyncCommand = FetchItems
                  | FetchedItems POSIXTime [PocketItem]
                  | ArchiveItems
                  | ArchivedItems [PocketItemId]
                  | AsyncActionFailed (Maybe Text)
                  deriving (Show,Eq)

data UiCommand = ShiftItem PocketItemId
               | RemoveItems (Set PocketItemId)
               | SetStatus (Maybe Text)
               | BrowseItem PocketItem
               deriving (Show,Eq)

fetchItemsEvt :: HocketEvent
fetchItemsEvt = Internal (InternalAsync FetchItems)

fetchedItemsEvt :: POSIXTime -> [PocketItem] -> HocketEvent
fetchedItemsEvt t itms = Internal (InternalAsync (FetchedItems t itms))

archiveItemsEvt :: HocketEvent
archiveItemsEvt = Internal (InternalAsync ArchiveItems)

archivedItemsEvt :: [PocketItemId] -> HocketEvent
archivedItemsEvt pids = Internal (InternalAsync (ArchivedItems pids))

asyncActionFailedEvt :: Maybe Text -> HocketEvent
asyncActionFailedEvt maybeMsg = Internal (InternalAsync (AsyncActionFailed maybeMsg))

shiftItemEvt :: PocketItemId -> HocketEvent
shiftItemEvt pid = Internal (InternalUi (ShiftItem pid))

removeItemsEvt :: Set PocketItemId -> HocketEvent
removeItemsEvt pids = Internal (InternalUi (RemoveItems pids))

setStatusEvt :: Maybe Text -> HocketEvent
setStatusEvt mstatus= Internal (InternalUi (SetStatus mstatus))

browseItemEvt :: PocketItem -> HocketEvent
browseItemEvt pit = Internal (InternalUi (BrowseItem pit))
