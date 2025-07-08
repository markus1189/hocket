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
    ItemCounts,
    icNone,
    icToBeArchived,
    icToBeReminded,
    icReminderToBeRemoved,
    icFutureReminders,
    initialState,
    insertItem,
    insertItems,
    removeItem,
    removeItems,
    togglePendingAction,
    togglePendingActionToReminder,
    clearAllFlags,
    clearFlagsForItems,
    setAllFlagsToArchive,
    toggleShowFutureReminders,
    updateItemsWithReminder,
    updateItemsWithStoredReminderTimes,
    removeReminderFromItems,
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
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
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

-- Normalize PendingAction for grouping - all ToBeReminded times become ToBeReminded with epoch
normalizePendingAction :: PendingAction -> PendingAction
normalizePendingAction (ToBeReminded _) = ToBeReminded (posixSecondsToUTCTime 0)
normalizePendingAction other = other

partitionItems :: HocketState -> Map PendingAction (SortedList SortByUpdated)
partitionItems =
  Map.fromListWith (<>)
    . over (mapped . _1) normalizePendingAction
    . over (mapped . _2) (SL.singleton . SBU)
    . toList
    . view hsContents

data ItemCounts = ItemCounts
  { _icNone :: Int,
    _icToBeArchived :: Int,
    _icToBeReminded :: Int,
    _icReminderToBeRemoved :: Int,
    _icFutureReminders :: Int
  }

makeLenses ''ItemCounts

hsNumItems :: HocketState -> ItemCounts
hsNumItems s =
  ItemCounts
    { _icNone = length $ partitioned ^. at None . non (SL.toSortedList []),
      _icToBeArchived = length $ partitioned ^. at ToBeArchived . non (SL.toSortedList []),
      _icToBeReminded = length $ partitioned ^. at (ToBeReminded (posixSecondsToUTCTime 0)) . non (SL.toSortedList []),
      _icReminderToBeRemoved = length $ partitioned ^. at ReminderToBeRemoved . non (SL.toSortedList []),
      _icFutureReminders =
        if s ^. hsShowFutureReminders
          then 0
          else length $ filter (isJust . view biReminder . snd) $ Map.elems (s ^. hsContents)
    }
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
    toggle (ToBeReminded _) = None
    toggle ReminderToBeRemoved = None

togglePendingActionToReminder :: BookmarkItemId -> UTCTime -> HocketState -> HocketState
togglePendingActionToReminder bid reminderTime s =
  case s ^. hsContents . at bid of
    Just (currentAction, item) ->
      let hasExistingReminder = isJust (item ^. biReminder)
          newAction = case currentAction of
            None -> if hasExistingReminder then ReminderToBeRemoved else ToBeReminded reminderTime
            ToBeReminded _ -> None
            ReminderToBeRemoved -> None
            ToBeArchived -> if hasExistingReminder then ReminderToBeRemoved else ToBeReminded reminderTime
       in s & hsContents . at bid ?~ (newAction, item)
    Nothing -> s

setAllFlags :: PendingAction -> HocketState -> HocketState
setAllFlags action = hsContents . mapped . _1 .~ action

clearAllFlags :: HocketState -> HocketState
clearAllFlags = setAllFlags None

clearFlagsForItems :: PendingAction -> [BookmarkItemId] -> HocketState -> HocketState
clearFlagsForItems targetAction bids s =
  foldl'
    ( \st bid ->
        case st ^. hsContents . at bid of
          Just (action, item) | action == targetAction -> st & hsContents . at bid ?~ (None, item)
          _ -> st
    )
    s
    bids

setAllFlagsToArchive :: HocketState -> HocketState
setAllFlagsToArchive = setAllFlags ToBeArchived

toggleShowFutureReminders :: HocketState -> HocketState
toggleShowFutureReminders s = s & hsShowFutureReminders %~ not

updateItemsWithReminder :: [BookmarkItemId] -> UTCTime -> HocketState -> HocketState
updateItemsWithReminder bids reminderTime s =
  foldl'
    ( \st bid ->
        case st ^. hsContents . at bid of
          Just (action, item) ->
            let updatedItem = item & biReminder ?~ reminderTime
             in st & hsContents . at bid ?~ (action, updatedItem)
          Nothing -> st
    )
    s
    bids

updateItemsWithStoredReminderTimes :: [BookmarkItemId] -> HocketState -> HocketState
updateItemsWithStoredReminderTimes bids s =
  foldl'
    ( \st bid ->
        case st ^. hsContents . at bid of
          Just (ToBeReminded reminderTime, item) ->
            let updatedItem = item & biReminder ?~ reminderTime
             in st & hsContents . at bid ?~ (ToBeReminded reminderTime, updatedItem)
          Just _ -> st -- Keep non-reminder actions unchanged
          Nothing -> st
    )
    s
    bids

removeReminderFromItems :: [BookmarkItemId] -> HocketState -> HocketState
removeReminderFromItems bids s =
  foldl'
    ( \st bid ->
        case st ^. hsContents . at bid of
          Just (action, item) ->
            let updatedItem = item & biReminder .~ Nothing
             in st & hsContents . at bid ?~ (action, updatedItem)
          Nothing -> st
    )
    s
    bids

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
              map
                (Down . SBU . snd)
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
