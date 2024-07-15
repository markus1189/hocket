module Events (HocketEvent(..)
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

import           Network.Pocket

data HocketEvent = HocketAsync !AsyncCommand
                 | HocketUi !UiCommand
                 deriving (Show,Eq)

data AsyncCommand = FetchItems
                  | FetchedItems !POSIXTime ![PocketItem]
                  | ArchiveItems
                  | ArchivedItems ![PocketItemId]
                  | AsyncActionFailed !(Maybe Text)
                  deriving (Show,Eq)

data UiCommand = ShiftItem !PocketItemId
               | RemoveItems !(Set PocketItemId)
               | SetStatus !(Maybe Text)
               | BrowseItem !PocketItem
               deriving (Show,Eq)

fetchItemsEvt :: HocketEvent
fetchItemsEvt = HocketAsync FetchItems

fetchedItemsEvt :: POSIXTime -> [PocketItem] -> HocketEvent
fetchedItemsEvt t itms = HocketAsync (FetchedItems t itms)

archiveItemsEvt :: HocketEvent
archiveItemsEvt = HocketAsync ArchiveItems

archivedItemsEvt :: [PocketItemId] -> HocketEvent
archivedItemsEvt pids = HocketAsync (ArchivedItems pids)

asyncActionFailedEvt :: Maybe Text -> HocketEvent
asyncActionFailedEvt maybeMsg = HocketAsync (AsyncActionFailed maybeMsg)

shiftItemEvt :: PocketItemId -> HocketEvent
shiftItemEvt pid = HocketUi (ShiftItem pid)

removeItemsEvt :: Set PocketItemId -> HocketEvent
removeItemsEvt pids = HocketUi (RemoveItems pids)

setStatusEvt :: Maybe Text -> HocketEvent
setStatusEvt mstatus= HocketUi (SetStatus mstatus)

browseItemEvt :: PocketItem -> HocketEvent
browseItemEvt pit = HocketUi (BrowseItem pit)
