{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Bookmark.Ui.State
  ( HocketState,
    pendingList,
    itemList,
    focusRing,
    hsNumItems,
    hsLastUpdated,
    hsContents,
    Name (..),
    hsAsync,
    hsStatus,
    hsCredentials,
    initialState,
    insertItem,
    insertItems,
    removeItem,
    removeItems,
    Status (..),
    toggleStatus,
    SortByUpdated,
    syncForRender,
  )
where

import qualified Brick.Focus as F
import Brick.Widgets.List (List)
import qualified Brick.Widgets.List as L
import Control.Applicative ((<|>))
import Control.Concurrent.Async (Async)
import Control.Lens
import Data.Foldable (foldl', maximumBy, toList)
import Data.Function (on)
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down (..), comparing)
import Data.SortedList (SortedList)
import qualified Data.SortedList as SL
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Vector as V
import Data.Vector.Lens (toVectorOf)
import Network.Bookmark.Types
import Network.Bookmark.Ui.Widgets

data Status = Pending | Unread deriving (Show, Eq, Ord)

data Name = ItemListName | PendingListName deriving (Show, Eq, Ord)

data HocketState = HocketState
  { _itemList :: !(List Name BookmarkItem),
    _pendingList :: !(List Name BookmarkItem),
    _focusRing :: !(F.FocusRing Name),
    _hsLastUpdated :: !(Maybe POSIXTime),
    _hsAsync :: !(Maybe (Async ())),
    _hsStatus :: !(Maybe Text),
    _hsContents :: !(Map BookmarkItemId (Status, BookmarkItem)),
    _hsCredentials :: !BookmarkCredentials
  }

makeLenses ''HocketState

newtype SortByUpdated = SBU BookmarkItem

instance Eq SortByUpdated where
  SBU bi1 == SBU bi2 = ((==) `on` _biCreated) bi1 bi2

instance Ord SortByUpdated where
  compare (SBU bi1) (SBU bi2) = (compare `on` _biCreated) bi1 bi2

partitionItems :: HocketState -> Map Status (SortedList SortByUpdated)
partitionItems =
  Map.fromListWith (<>)
    . over (mapped . _2) (SL.singleton . SBU)
    . toList
    . view hsContents

hsNumItems :: HocketState -> (Int, Int)
hsNumItems s =
  ( length $ partitioned ^. at Unread . non (SL.toSortedList []),
    length $ partitioned ^. at Pending . non (SL.toSortedList [])
  )
  where
    partitioned = partitionItems s

initialState :: BookmarkCredentials -> HocketState
initialState cred =
  HocketState
    (L.list ItemListName V.empty 1)
    (L.list PendingListName V.empty 1)
    (F.focusRing [ItemListName, PendingListName])
    Nothing
    Nothing
    Nothing
    Map.empty
    cred

insertItem :: BookmarkItem -> HocketState -> HocketState
insertItem bit s =
  s & hsContents %~ Map.insertWith newer (_biId bit) (Unread, bit)
  where
    newer :: (a, BookmarkItem) -> (a, BookmarkItem) -> (a, BookmarkItem)
    newer newBi oldBi = maximumBy (comparing (_biLastUpdate . snd)) [oldBi, newBi]

insertItems :: (Foldable f) => f BookmarkItem -> HocketState -> HocketState
insertItems = flip (foldl' (flip insertItem))

removeItem :: BookmarkItemId -> HocketState -> HocketState
removeItem bid s = s & hsContents . at bid .~ Nothing

removeItems :: (Foldable f) => f BookmarkItemId -> HocketState -> HocketState
removeItems = flip (foldl' (flip removeItem))

toggleStatus :: BookmarkItemId -> HocketState -> HocketState
toggleStatus bid = hsContents . ix bid . _1 %~ toggle
  where
    toggle Unread = Pending
    toggle Pending = Unread

syncForRender :: HocketState -> HocketState
syncForRender s =
  s
    & itemList . L.listElementsL .~ sortedUnread
    & itemList . L.listSelectedL .~ (view (itemList . L.listSelectedL) s <|> (sortedUnread ^? _head $> 0))
    & pendingList . L.listElementsL .~ sortedPending
    & pendingList . L.listSelectedL .~ (view (pendingList . L.listSelectedL) s <|> (sortedPending ^? _head $> 0))
    & itemList %~ adjustFocus
    & pendingList %~ adjustFocus
  where
    partitioned = partitionItems s
    sortedUnread = toVectorOf (at Unread . each . to SL.reverse . to toList . each . to (\(Down (SBU x)) -> x)) partitioned
    sortedPending = toVectorOf (at Pending . each . to SL.reverse . to toList . each . to (\(Down (SBU x)) -> x)) partitioned

adjustFocus :: L.List n a -> L.List n a
adjustFocus l =
  if n > 0
    then l & L.listSelectedL . _Just %~ clamp 0 (n - 1)
    else l & L.listSelectedL .~ Nothing
  where
    n = length (l ^. L.listElementsL)
