{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Pocket.Ui.State (HocketState
                               ,pendingList
                               ,itemList
                               ,itemListVi
                               ,pendingListVi
                               ,focusRing
                               ,hsNumItems
                               ,hsLastUpdated
                               ,hsContents
                               ,itemListName
                               ,pendingListName
                               ,hsAsync
                               ,hsStatus

                               ,initialState
                               ,insertItem
                               ,insertItems
                               ,removeItem
                               ,removeItems

                               ,Status(..)
                               ,toggleStatus

                               ,SortByUpdated

                               ,syncForRender
                               ) where

import Control.Applicative ((<|>))
import           Brick (Name)
import qualified Brick.Focus as F
import qualified Brick.Widgets.List as L
import           Control.Concurrent.Async (Async)
import           Control.Lens
import           Data.Foldable (foldl',maximumBy,toList)
import           Data.Function (on)
import           Data.Functor (($>))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Ord (comparing,Down(..))
import           Data.SortedList (SortedList)
import qualified Data.SortedList as SL
import           Data.Text (Text)
import           Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Vector as V
import           Data.Vector.Lens (toVectorOf)

import           Network.Pocket.Ui.Widgets
import           Network.Pocket.Types

data Status = Pending | Unread deriving (Show,Eq,Ord)

data HocketState = HocketState { _itemListVi :: ViList PocketItem
                               , _pendingListVi :: ViList PocketItem
                               , _focusRing :: F.FocusRing
                               , _hsLastUpdated :: Maybe POSIXTime
                               , _hsAsync :: Maybe (Async ())
                               , _hsStatus :: Maybe Text
                               , _hsContents :: Map PocketItemId (Status,PocketItem)
                               }

makeLenses ''HocketState

newtype SortByUpdated = SBU PocketItem

instance Eq SortByUpdated where
  SBU pi1 == SBU pi2 = ((==) `on` view timeUpdated) pi1 pi2

instance Ord SortByUpdated where
  compare (SBU pi1) (SBU pi2) = (compare `on` view timeUpdated) pi1 pi2

partitionItems :: HocketState -> Map Status (SortedList SortByUpdated)
partitionItems = Map.fromListWith (<>)
               . over (mapped . _2) (SL.singleton . SBU)
               . toList
               . view hsContents

hsNumItems :: HocketState -> (Int,Int)
hsNumItems s = (length $ partitioned ^. at Unread . non (SL.toSortedList [])
               ,length $ partitioned ^. at Pending . non (SL.toSortedList []))
  where partitioned = partitionItems s

initialState :: HocketState
initialState = HocketState (ViList $ L.list itemListName V.empty 1)
                           (ViList $ L.list pendingListName V.empty 1)
                           (F.focusRing [itemListName, pendingListName])
                           Nothing
                           Nothing
                           Nothing
                           Map.empty

itemListName :: Name
itemListName = "items"

pendingListName :: Name
pendingListName = "pending"

itemList :: Lens' HocketState (L.List PocketItem)
itemList = itemListVi . _ViList

pendingList :: Lens' HocketState (L.List PocketItem)
pendingList = pendingListVi . _ViList

insertItem :: PocketItem -> HocketState -> HocketState
insertItem pit@(view itemId -> pid) s =
  s & hsContents %~ Map.insertWith newer pid (Unread,pit)
  where newer :: (a,PocketItem) -> (a,PocketItem) -> (a,PocketItem)
        newer pi1 pi2 = maximumBy (comparing (view timeUpdated . snd)) [pi1,pi2]

insertItems :: Foldable f => f PocketItem -> HocketState -> HocketState
insertItems = flip (foldl' (flip insertItem))

removeItem :: PocketItemId -> HocketState -> HocketState
removeItem pid s = s & hsContents . at pid .~ Nothing

removeItems :: Foldable f => f PocketItemId -> HocketState -> HocketState
removeItems = flip (foldl' (flip removeItem))

toggleStatus :: PocketItemId -> HocketState -> HocketState
toggleStatus pid = hsContents . ix pid . _1 %~ toggle
  where toggle Unread = Pending
        toggle Pending = Unread

syncForRender :: HocketState -> HocketState
syncForRender s = s & itemList . L.listElementsL .~ sortedUnread
                    & itemList . L.listSelectedL .~ (view (itemList . L.listSelectedL) s <|> (sortedUnread ^? _head $> 0))
                    & pendingList . L.listElementsL .~ sortedPending
                    & pendingList . L.listSelectedL .~ (view (pendingList . L.listSelectedL) s <|> (sortedPending ^? _head $> 0))
                    & itemList %~ adjustFocus
                    & pendingList %~ adjustFocus
  where partitioned = partitionItems s
        sortedUnread = toVectorOf (at Unread . each . to SL.reverse . to toList . each . to (\(Down (SBU x)) -> x)) partitioned
        sortedPending = toVectorOf (at Pending . each . to SL.reverse . to toList . each . to (\(Down (SBU x)) -> x)) partitioned

adjustFocus :: L.List a -> L.List a
adjustFocus l = if n > 0
                  then l & L.listSelectedL . _Just %~ clamp 0 (n-1)
                  else l & L.listSelectedL .~ Nothing
 where n = length (l ^. L.listElementsL)
