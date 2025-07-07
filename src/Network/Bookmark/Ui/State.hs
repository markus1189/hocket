{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Bookmark.Ui.State
  ( HocketState,
    itemList,
    focusRing,
    hsNumItems,
    hsLastUpdated,
    hsContents,
    Name (..),
    hsAsync,
    hsStatus,
    hsCredentials,
    hsShowFutureReminders,
    initialState,
    insertItem,
    insertItems,
    removeItem,
    removeItems,
    togglePendingAction,
    clearAllFlags,
    setAllFlagsToArchive,
    toggleShowFutureReminders,
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
import Data.Maybe (isJust)
import Data.Ord (Down (..), comparing)
import Data.SortedList (SortedList)
import qualified Data.SortedList as SL
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Vector as V
import Network.Bookmark.Types
import Network.Bookmark.Ui.Widgets

data Name = ItemListName deriving (Show, Eq, Ord)

data HocketState = HocketState
  { _itemList :: !(List Name BookmarkItem),
    _focusRing :: !(F.FocusRing Name),
    _hsLastUpdated :: !(Maybe POSIXTime),
    _hsAsync :: !(Maybe (Async ())),
    _hsStatus :: !(Maybe Text),
    _hsContents :: !(Map BookmarkItemId (PendingAction, BookmarkItem)),
    _hsCredentials :: !BookmarkCredentials,
    _hsShowFutureReminders :: !Bool
  }

makeLenses ''HocketState

newtype SortByUpdated = SBU BookmarkItem

instance Eq SortByUpdated where
  SBU bi1 == SBU bi2 = ((==) `on` getSortDate) bi1 bi2

instance Ord SortByUpdated where
  compare (SBU bi1) (SBU bi2) = (compare `on` getSortDate) bi1 bi2

getSortDate :: BookmarkItem -> UTCTime
getSortDate item = case _biReminder item of
  Just reminderDate -> reminderDate
  Nothing -> _biCreated item

partitionItems :: HocketState -> Map PendingAction (SortedList SortByUpdated)
partitionItems =
  Map.fromListWith (<>)
    . over (mapped . _2) (SL.singleton . SBU)
    . toList
    . view hsContents

hsNumItems :: HocketState -> (Int, Int, Int)
hsNumItems s =
  ( length $ partitioned ^. at None . non (SL.toSortedList []),
    length $ partitioned ^. at ToBeArchived . non (SL.toSortedList []),
    if s ^. hsShowFutureReminders
      then 0
      else length $ filter (isJust . view biReminder . snd) $ Map.elems (s ^. hsContents)
  )
  where
    partitioned = partitionItems s

initialState :: BookmarkCredentials -> HocketState
initialState creds =
  HocketState
    (L.list ItemListName V.empty 1)
    (F.focusRing [ItemListName])
    Nothing
    Nothing
    Nothing
    Map.empty
    creds
    False

insertItem :: BookmarkItem -> HocketState -> HocketState
insertItem bit s =
  s & hsContents %~ Map.insertWith newer (_biId bit) (None, bit)
  where
    newer :: (a, BookmarkItem) -> (a, BookmarkItem) -> (a, BookmarkItem)
    newer newBi oldBi = (fst oldBi, maximumBy (comparing _biLastUpdate) (map snd [oldBi, newBi]))

insertItems :: (Foldable f) => f BookmarkItem -> HocketState -> HocketState
insertItems = flip (foldl' (flip insertItem))

removeItem :: BookmarkItemId -> HocketState -> HocketState
removeItem bid s = s & hsContents . at bid .~ Nothing

removeItems :: (Foldable f) => f BookmarkItemId -> HocketState -> HocketState
removeItems = flip (foldl' (flip removeItem))

togglePendingAction :: BookmarkItemId -> HocketState -> HocketState
togglePendingAction bid = hsContents . ix bid . _1 %~ toggle
  where
    toggle None = ToBeArchived
    toggle ToBeArchived = None

setAllFlags :: PendingAction -> HocketState -> HocketState
setAllFlags action = hsContents . mapped . _1 .~ action

clearAllFlags :: HocketState -> HocketState
clearAllFlags = setAllFlags None

setAllFlagsToArchive :: HocketState -> HocketState
setAllFlagsToArchive = setAllFlags ToBeArchived

toggleShowFutureReminders :: HocketState -> HocketState
toggleShowFutureReminders s = s & hsShowFutureReminders %~ not

syncForRender :: HocketState -> HocketState
syncForRender s =
  s
    & itemList . L.listElementsL .~ allSortedItems
    & itemList . L.listSelectedL .~ (view (itemList . L.listSelectedL) s <|> (allSortedItems ^? _head $> 0))
    & itemList %~ adjustFocus
  where
    allSortedItems =
      V.fromList $
        map (\(Down (SBU x)) -> x) $
          SL.fromSortedList $
            SL.toSortedList $
              map (Down . SBU . snd) $
                filteredContents
    filteredContents =
      if s ^. hsShowFutureReminders
        then Map.elems (s ^. hsContents)
        else filter (not . hasFutureReminder . snd) (Map.elems (s ^. hsContents))

    hasFutureReminder :: BookmarkItem -> Bool
    hasFutureReminder item =
      case view biReminder item of
        Nothing -> False
        Just _reminderTime -> True

adjustFocus :: L.List n a -> L.List n a
adjustFocus l =
  if n > 0
    then l & L.listSelectedL . _Just %~ clamp 0 (n - 1)
    else l & L.listSelectedL .~ Nothing
  where
    n = length (l ^. L.listElementsL)
