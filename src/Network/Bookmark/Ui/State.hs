{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Bookmark.Ui.State
  ( HocketState,
    hsNumItems,
    hsLastUpdated,
    hsContents,
    Name (..),
    VideoFilterMode (..),
    AsyncOp (..),
    BatchStep (..),
    BatchProgress (BatchProgress),
    bpPlanned,
    bpRan,
    hsBatch,
    hsAsyncOp,
    tryAcquireAsyncOp,
    completeAsyncOp,
    asyncOpRunning,
    batchStepsWithWork,
    hsStatus,
    hsShowFutureReminders,
    hsVideoFilter,
    hsFilterActive,
    hsFilterQuery,
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
    setPendingAction,
    setFilterQuery,
    setVideoFilterMode,
    setShowFutureReminders,
    setAgentClients,
    hsAgentClients,
    setAgentError,
    hsAgentError,
    clearAllFlags,
    clearFlagsForItems,
    clearToBeRemindedFlags,
    isToBeReminded,
    isReminderAction,
    focusedItem,
    getPendingActionForItem,
    shiftItem,
    archiveItem,
    unflagItem,
    shiftItemReminder,
    setAllFlagsToArchive,
    toggleShowFutureReminders,
    toggleVideoFilter,
    toggleInvertedVideoFilter,
    enterFilterMode,
    lockFilter,
    cancelFilter,
    bookmarkSearchText,
    updateItemsWithReminder,
    removeReminderFromItems,
    SortByUpdated,
    settle,
    hsVisible,
    hsSelected,
    visibleItems,
    getSortDate,
    isVideoBookmark,
  )
where

import Control.Lens
#if MIN_VERSION_base(4,20,0)
import Data.Foldable (maximumBy)
#else
import Data.Foldable (foldl', maximumBy)
#endif
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (Down (..), comparing)
import qualified Data.SortedList as SL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Vector as V
import qualified Network.Bookmark.App.Nav as Nav
import Network.Bookmark.Batch (StepOutcome)
import Network.Bookmark.Types
import Network.Bookmark.Ui.Widgets

data Name = ItemListName | FilterEditorName deriving (Show, Eq, Ord)

-- | The async operations the TUI may start. Only one is ever in flight at a
-- time (single-slot lock). The identity is recorded so the lock can be tested
-- purely, instead of being an opaque 'Async' handle nobody reads.
data AsyncOp
  = OpFetchItems
  | OpExecuteBatch
  deriving (Show, Eq)

data VideoFilterMode
  = NoVideoFilter
  | ShowOnlyVideos
  | HideVideos
  deriving (Show, Eq)

data BatchStep
  = StepArchive
  | StepSetReminders
  | StepRemoveReminders
  deriving (Show, Eq)

-- | A running batch: its steps, and the outcomes 'StepDone' brought so far,
-- from which update writes the summary.
data BatchProgress = BatchProgress
  { _bpPlanned :: ![BatchStep],
    _bpRan :: ![(BatchStep, StepOutcome () Text)]
  }
  deriving (Show, Eq)

makeLenses ''BatchProgress

data HocketState = HocketState
  { _hsLastUpdated :: !(Maybe POSIXTime),
    _hsAsyncOp :: !(Maybe AsyncOp),
    _hsBatch :: !(Maybe BatchProgress),
    _hsStatus :: !(Maybe Text),
    _hsContents :: !(Map BookmarkItemId (PendingAction, BookmarkItem)),
    _hsShowFutureReminders :: !Bool,
    _hsVideoFilter :: !VideoFilterMode,
    _hsFilterActive :: !Bool,
    -- | The filter text (plan D3): the core's; the editor only edits it.
    _hsFilterQuery :: !Text,
    _hsAgentClients :: !Int,
    -- | Set when the agent socket server died; the shared status line is
    -- overwritten within a frame, so the header carries this instead.
    _hsAgentError :: !(Maybe Text),
    -- | What the list shows, in order; only 'settle' writes it.
    _hsVisible :: !(V.Vector BookmarkItem),
    -- | The selection, by id: always visible, or 'Nothing' when nothing is
    -- (I4). The brick list follows it, never the other way round.
    _hsSelected :: !(Maybe BookmarkItemId)
  }

makeLenses ''HocketState

asyncOpRunning :: HocketState -> Bool
asyncOpRunning s = case s ^. hsAsyncOp of
  Nothing -> False
  Just _ -> True

-- | Acquire the single async slot (marking it in-flight), or 'Nothing' if
-- another op already owns it.
tryAcquireAsyncOp :: AsyncOp -> HocketState -> Maybe HocketState
tryAcquireAsyncOp whichOp s
  | asyncOpRunning s = Nothing
  | otherwise = Just (s & hsAsyncOp ?~ whichOp)

completeAsyncOp :: HocketState -> HocketState
completeAsyncOp = hsAsyncOp .~ Nothing

newtype SortByUpdated = SBU BookmarkItem

instance Eq SortByUpdated where
  SBU bi1 == SBU bi2 = ((==) `on` getSortDate) bi1 bi2

instance Ord SortByUpdated where
  compare (SBU bi1) (SBU bi2) = (compare `on` getSortDate) bi1 bi2

getSortDate :: BookmarkItem -> UTCTime
getSortDate item = case _biReminder item of
  Just reminderDate -> reminderDate
  Nothing -> _biCreated item

data ItemCounts = ItemCounts
  { _icNone :: !Int,
    _icToBeArchived :: !Int,
    _icToBeReminded :: !Int,
    _icReminderToBeRemoved :: !Int,
    _icFutureReminders :: !Int
  }

makeLenses ''ItemCounts

hsNumItems :: HocketState -> ItemCounts
hsNumItems s = foldl' step (ItemCounts 0 0 0 0 0) (s ^. hsContents)
  where
    countFuture = not (s ^. hsShowFutureReminders)
    step c (action, item) =
      (if countFuture && isJust (item ^. biReminder) then icFutureReminders +~ 1 else id) $
        case action of
          None -> c & icNone +~ 1
          ToBeArchived -> c & icToBeArchived +~ 1
          ToBeReminded _ -> c & icToBeReminded +~ 1
          ReminderToBeRemoved -> c & icReminderToBeRemoved +~ 1

-- | The non-empty 'X' sub-buckets, in the order the batch op runs them.
batchStepsWithWork :: HocketState -> [BatchStep]
batchStepsWithWork s =
  let counts = hsNumItems s
      stepIf pending step = [step | pending]
   in concat
        [ stepIf (counts ^. icToBeArchived > 0) StepArchive,
          stepIf (counts ^. icToBeReminded > 0) StepSetReminders,
          stepIf (counts ^. icReminderToBeRemoved > 0) StepRemoveReminders
        ]

initialState :: HocketState
initialState =
  HocketState
    Nothing
    Nothing
    Nothing
    Nothing
    Map.empty
    False
    NoVideoFilter
    False
    T.empty
    0
    Nothing
    V.empty
    Nothing

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

focusedItem :: HocketState -> Maybe BookmarkItem
focusedItem s = s ^. hsSelected >>= \bid -> s ^? hsContents . ix bid . _2

getPendingActionForItem :: BookmarkItemId -> HocketState -> PendingAction
getPendingActionForItem bid s =
  case s ^. hsContents . at bid of
    Just (action, _) -> action
    Nothing -> None

-- | The cursor's move after a key's flag change: the next visible item.
-- Flags do not change what is visible, so the move sees the same list.
selectNext :: HocketState -> HocketState
selectNext s = s & hsSelected .~ Nav.navigate 0 Nav.Down (s ^. hsVisible) (`getPendingActionForItem` s) (s ^. hsSelected)

-- | The key-path toggles: flip the flag, then move the cursor down.
shiftItem :: BookmarkItemId -> HocketState -> HocketState
shiftItem bid = selectNext . togglePendingAction bid

-- | Archive from any flag; 'shiftItem' would clear a reminder flag instead.
archiveItem :: BookmarkItemId -> HocketState -> HocketState
archiveItem bid = selectNext . setPendingAction bid ToBeArchived

-- | Key @u@: what 'shiftItem' and 'shiftItemReminder' do to a flagged item,
-- which ends at 'None' whatever the reminder time.
unflagItem :: BookmarkItemId -> HocketState -> HocketState
unflagItem bid = selectNext . setPendingAction bid None

shiftItemReminder :: BookmarkItemId -> UTCTime -> HocketState -> HocketState
shiftItemReminder bid reminderTime = selectNext . togglePendingActionToReminder bid reminderTime

-- | Idempotent flag assignment for the agent socket: unlike the keyboard
-- toggles this cannot flip the wrong way when the caller's view is stale.
-- A no-op for unknown ids.
setPendingAction :: BookmarkItemId -> PendingAction -> HocketState -> HocketState
setPendingAction bid act = hsContents . ix bid . _1 .~ act

-- | Replace the fuzzy filter query wholesale (agents set it in one step
-- rather than char-by-char) and leave filter-input mode locked in.
setFilterQuery :: Text -> HocketState -> HocketState
setFilterQuery q = (hsFilterQuery .~ q) . (hsFilterActive .~ False)

setVideoFilterMode :: VideoFilterMode -> HocketState -> HocketState
setVideoFilterMode m = hsVideoFilter .~ m

setShowFutureReminders :: Bool -> HocketState -> HocketState
setShowFutureReminders b = hsShowFutureReminders .~ b

setAgentClients :: Int -> HocketState -> HocketState
setAgentClients n = hsAgentClients .~ n

setAgentError :: Maybe Text -> HocketState -> HocketState
setAgentError e = hsAgentError .~ e

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

-- Helper functions to check action types without caring about parameters
isToBeReminded :: PendingAction -> Bool
isToBeReminded (ToBeReminded _) = True
isToBeReminded _ = False

isReminderAction :: PendingAction -> Bool
isReminderAction (ToBeReminded _) = True
isReminderAction ReminderToBeRemoved = True
isReminderAction _ = False

clearToBeRemindedFlags :: [BookmarkItemId] -> HocketState -> HocketState
clearToBeRemindedFlags bids s =
  foldl'
    ( \st bid ->
        case st ^. hsContents . at bid of
          Just (action, item) | isToBeReminded action -> st & hsContents . at bid ?~ (None, item)
          _ -> st
    )
    s
    bids

setAllFlagsToArchive :: HocketState -> HocketState
setAllFlagsToArchive = setAllFlags ToBeArchived

toggleShowFutureReminders :: HocketState -> HocketState
toggleShowFutureReminders s = s & hsShowFutureReminders %~ not

toggleVideoFilter :: HocketState -> HocketState
toggleVideoFilter s = s & hsVideoFilter %~ toggleShowMode
  where
    toggleShowMode NoVideoFilter = ShowOnlyVideos
    toggleShowMode ShowOnlyVideos = NoVideoFilter
    toggleShowMode HideVideos = ShowOnlyVideos

toggleInvertedVideoFilter :: HocketState -> HocketState
toggleInvertedVideoFilter s = s & hsVideoFilter %~ toggleHideMode
  where
    toggleHideMode NoVideoFilter = HideVideos
    toggleHideMode HideVideos = NoVideoFilter
    toggleHideMode ShowOnlyVideos = HideVideos

enterFilterMode :: HocketState -> HocketState
enterFilterMode = hsFilterActive .~ True

lockFilter :: HocketState -> HocketState
lockFilter = hsFilterActive .~ False

cancelFilter :: HocketState -> HocketState
cancelFilter = (hsFilterActive .~ False) . (hsFilterQuery .~ T.empty)

bookmarkSearchText :: BookmarkItem -> Text
bookmarkSearchText bi =
  T.unwords [bi ^. biTitle, bi ^. biDomain, bi ^. biExcerpt, bi ^. biNote]

isVideoBookmark :: BookmarkItem -> Bool
isVideoBookmark item = hasYouTubeInUrl || hasVideoTag
  where
    hasYouTubeInUrl = T.isInfixOf "youtu.be" (T.toLower $ item ^. biLink) || T.isInfixOf "youtube" (T.toLower $ item ^. biLink)
    hasVideoTag = any (\tag -> T.toLower tag == "video") (item ^. biTags)

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

-- | Recompute what is visible and keep the selection valid (plan D2): the
-- selected item stays selected while it is visible; when it disappears the
-- item now at its index takes over (clamped at the end); with no selection
-- the first item is selected.
settle :: HocketState -> HocketState
settle s =
  s
    & hsVisible .~ visible
    & hsSelected .~ selected
  where
    visible = visibleItems (s ^. hsShowFutureReminders) (s ^. hsVideoFilter) (s ^. hsFilterQuery) (s ^. hsContents)
    indexIn items bid = V.findIndex ((== bid) . view biId) items
    selected = case s ^. hsSelected of
      Just bid | isJust (indexIn visible bid) -> Just bid
      prev -> view biId <$> visible V.!? clamp 0 (V.length visible - 1) (fromMaybe 0 (prev >>= indexIn (s ^. hsVisible)))

-- | The items the list shows, newest sort date ('getSortDate') first: the
-- future-reminder, video and text filters, in that order.
visibleItems :: Bool -> VideoFilterMode -> Text -> Map BookmarkItemId (PendingAction, BookmarkItem) -> V.Vector BookmarkItem
visibleItems showFutureReminders videoFilter query contents =
  V.fromList $
    map (\(Down (SBU x)) -> x) $
      SL.fromSortedList $
        SL.toSortedList $
          map
            (Down . SBU . snd)
            filteredContents
  where
    filteredContents =
      let reminderFiltered =
            if showFutureReminders
              then Map.elems contents
              else filter (not . hasFutureReminder . snd) (Map.elems contents)
          videoFiltered =
            case videoFilter of
              NoVideoFilter -> reminderFiltered
              ShowOnlyVideos -> filter (isVideoBookmark . snd) reminderFiltered
              HideVideos -> filter (not . isVideoBookmark . snd) reminderFiltered
          textFiltered =
            if T.null query
              then videoFiltered
              else filter (fuzzyFilterMatch query . bookmarkSearchText . snd) videoFiltered
       in textFiltered

    hasFutureReminder :: BookmarkItem -> Bool
    hasFutureReminder item =
      case view biReminder item of
        Nothing -> False
        Just _reminderTime -> True
