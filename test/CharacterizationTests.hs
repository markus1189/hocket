{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Phase 0 pins of today's behaviour (plan §6), bugs included. Rows marked
-- "pins bug" are expected to flip in Phase 1.
module CharacterizationTests (characterizationTests, emptyState, item) where

import qualified Brick.Widgets.List as L
import Control.Lens
import Data.Foldable (for_)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (minutesToTimeZone)
import qualified Graphics.Vty as Vty
import Keys (keyIntents)
import Network.Bookmark.App.Effect (Effect (OpenUrl), Opener (Firefox))
import Network.Bookmark.App.Msg (Msg (Intent), Now (..))
import Network.Bookmark.App.Update (update)
import Network.Bookmark.Types
import Network.Bookmark.Ui.State
import Network.Bookmark.Ui.Sync
import Test.Tasty
import Test.Tasty.HUnit

characterizationTests :: TestTree
characterizationTests =
  testGroup "Characterization (Phase 0 pins)" [syncTests, stepResultTests, keyTableTests, selectionTests]

emptyState :: HocketState
emptyState = initialState

item :: Text -> Int -> BookmarkItem
item i coll =
  BookmarkItem
    (BookmarkItemId i)
    ("https://example.com/" <> i)
    ""
    ""
    "link"
    []
    False
    (read "2016-05-22 12:00:00 UTC")
    (read "2016-05-22 12:00:00 UTC")
    "example.com"
    ("title " <> i)
    0
    []
    coll
    False
    Nothing

posix :: String -> POSIXTime
posix = utcTimeToPOSIXSeconds . read

flagOf :: Text -> HocketState -> Maybe PendingAction
flagOf i s = s ^? hsContents . ix (BookmarkItemId i) . _1

reminderOf :: Text -> HocketState -> Maybe (Maybe UTCTime)
reminderOf i s = s ^? hsContents . ix (BookmarkItemId i) . _2 . biReminder

ids :: HocketState -> [Text]
ids s = [i | BookmarkItemId i <- Map.keys (s ^. hsContents)]

syncTests :: TestTree
syncTests =
  testGroup
    "sync"
    [ testCase "first fetch: Unsorted (-1), no search" $
        let plan = planSync Nothing
         in (plan ^. spKind, plan ^. spCollection, plan ^. spSearch)
              @?= (InitialUnsorted, RaindropCollectionId "-1", Nothing),
      testCase "incremental fetch: all collections (0), lastUpdate:> one UTC day back"
        $ for_
          [ ("2026-10-25 12:00:00 UTC", "lastUpdate:>2026-10-24"),
            ("2026-10-25 00:00:30 UTC", "lastUpdate:>2026-10-24"),
            ("2026-10-25 23:59:59 UTC", "lastUpdate:>2026-10-24"),
            ("2026-03-01 06:00:00 UTC", "lastUpdate:>2026-02-28")
          ]
        $ \(t, search) -> do
          let plan = planSync (Just (posix t))
          (t, plan ^. spKind, plan ^. spCollection, plan ^. spSearch)
            @?= (t, IncrementalAll, RaindropCollectionId "0", Just search),
      testCase "first fetch inserts every item as-is, whatever its collection" $
        ids (applyFetched 1 [item "a" (-1), item "b" 7] False emptyState) @?= ["a", "b"],
      testCase "incremental fetch: items that left Unsorted are removed, flag and all" $ do
        let before =
              insertItems [item "stay" (-1), item "moved" (-1)] emptyState
                & setPendingAction (BookmarkItemId "stay") ToBeArchived
                & setPendingAction (BookmarkItemId "moved") ToBeArchived
            after = applyFetched 1 [item "stay" (-1), item "moved" 7, item "elsewhere" 7, item "new" (-1)] True before
        ids after @?= ["new", "stay"]
        flagOf "stay" after @?= Just ToBeArchived
        flagOf "new" after @?= Just None,
      testCase "lastSync only grows, and an empty page leaves it alone"
        $ for_
          [ (Nothing, 200, True, Just 200),
            (Just 100, 200, True, Just 200),
            (Just 200, 200, True, Just 200),
            (Just 200, 100, True, Just 200),
            (Nothing, 200, False, Nothing),
            (Just 100, 200, False, Just 100)
          ]
        $ \(old, ts, nonEmpty, expected) -> do
          let s = emptyState & hsLastUpdated .~ old
              page = [item "a" (-1) | nonEmpty]
          ((old, ts, nonEmpty), applyFetched ts page True s ^. hsLastUpdated)
            @?= ((old, ts, nonEmpty), expected),
      testCase "merging a page leaves the fetch slot alone (B1b)" $ do
        let Just fetching = tryAcquireAsyncOp OpFetchItems emptyState
            afterPage1 = applyFetched 1 [item "a" (-1)] False fetching
        asyncOpRunning afterPage1 @?= True
    ]

stepResultTests :: TestTree
stepResultTests =
  testGroup
    "batch step results"
    [ testCase "archive removes the reported items only" $
        ids (applyArchived [BookmarkItemId "a"] twoFlagged) @?= ["b"],
      testCase "set-reminders stores the time that was sent and clears the flag" $ do
        let after = applyRemindersSet [(BookmarkItemId "r", tPrev)] reminderFlagged
        flagOf "r" after @?= Just None
        reminderOf "r" after @?= Just (Just tPrev),
      testCase "set-reminders after an unflag during the batch keeps the sent reminder (B3)" $ do
        let unflagged = reminderFlagged & setPendingAction (BookmarkItemId "r") None
            after = applyRemindersSet [(BookmarkItemId "r", t7)] unflagged
        flagOf "r" after @?= Just None
        reminderOf "r" after @?= Just (Just t7),
      testCase "remove-reminders clears the reminder, and the flag only if still ReminderToBeRemoved" $ do
        let s =
              insertItems [withRem (item "x" (-1)), withRem (item "y" (-1))] emptyState
                & setPendingAction (BookmarkItemId "x") ReminderToBeRemoved
                & setPendingAction (BookmarkItemId "y") ToBeArchived
            after = applyRemindersRemoved [BookmarkItemId "x", BookmarkItemId "y"] s
        (flagOf "x" after, reminderOf "x" after) @?= (Just None, Just Nothing)
        (flagOf "y" after, reminderOf "y" after) @?= (Just ToBeArchived, Just Nothing)
    ]
  where
    twoFlagged = insertItems [item "a" (-1), item "b" (-1)] emptyState & setAllFlagsToArchive
    reminderFlagged = insertItems [item "r" (-1)] emptyState & setPendingAction (BookmarkItemId "r") (ToBeReminded t7)

-- | What the reminder handler would get from 'nextDayAt7AM'.
t7 :: UTCTime
t7 = read "2026-11-02 06:00:00 UTC"

-- | A reminder time already on a flag before the key press.
tPrev :: UTCTime
tPrev = read "2026-10-30 06:00:00 UTC"

withRem :: BookmarkItem -> BookmarkItem
withRem = biReminder ?~ read "2026-01-01 07:00:00 UTC"

-- | Three visible items with the target focused at index 0. The target sorts
-- first by its far-future date; future reminders are shown so a target with
-- a reminder stays visible.
keyState :: Bool -> PendingAction -> HocketState
keyState hasReminder flag =
  insertItems [target, item "n1" (-1), item "n2" (-1)] emptyState
    & setShowFutureReminders True
    & setPendingAction (BookmarkItemId "t") flag
    & syncForRender
  where
    late = read "2030-01-01 00:00:00 UTC"
    target = item "t" (-1) & biCreated .~ late & biReminder .~ (if hasReminder then Just late else Nothing)

-- | The keys the table covers, named as in Phase 0.
data ItemKey = KeyArchive | KeyRemind | KeyUnflag | KeyEnter | KeySpace
  deriving (Show, Eq, Enum, Bounded)

keyEvent :: ItemKey -> Vty.Event
keyEvent = \case
  KeyArchive -> Vty.EvKey (Vty.KChar 'a') []
  KeyRemind -> Vty.EvKey (Vty.KChar 's') []
  KeyUnflag -> Vty.EvKey (Vty.KChar 'u') []
  KeyEnter -> Vty.EvKey Vty.KEnter []
  KeySpace -> Vty.EvKey (Vty.KChar ' ') []

-- | The 's' key's clock: 07:00 CET on the next day is 't7'.
keyNow :: Now
keyNow = Now (read "2026-11-01 12:00:00 UTC") (minutesToTimeZone 60)

-- | Press a key the way the shell does: its intents through 'update' within
-- one event, then the render sync. Returns the opened item ids.
press :: ItemKey -> HocketState -> (HocketState, [Text])
press key s0 = case keyIntents keyNow (keyEvent key) start of
  Nothing -> error ("not a keymap key: " <> show key)
  Just intents ->
    let (s, effects) = foldl apply (start, []) intents
     in (syncForRender s, mapMaybe opened effects)
  where
    start = syncForRender s0
    apply (s, effects) i = let (s', more) = update (Intent i) s in (s', effects <> more)
    opened (OpenUrl Firefox url) = T.stripPrefix "https://example.com/" url
    opened _ = Nothing

-- | Key, target has a reminder, flag before, flag after, cursor moves down.
data Row = Row ItemKey Bool PendingAction PendingAction Bool

keyTableTests :: TestTree
keyTableTests =
  testGroup "key semantics (focused item, flag before → after)" $
    map rowTest rows
      <> [ testCase "no focused item: every key changes nothing and opens nothing" $
             for_ [minBound .. maxBound] $ \k -> do
               let (after, browsed) = press k emptyState
               (k, browsed, Map.null (after ^. hsContents), after ^. itemList . L.listSelectedL) @?= (k, [], True, Nothing)
         ]
  where
    rowTest (Row k hasReminder from to moves) =
      testCase (show k <> (if hasReminder then " (has reminder) " else " ") <> show from <> " → " <> show to <> (if moves then " ↓" else "")) $ do
        let (after, browsed) = press k (keyState hasReminder from)
        flagOf "t" after @?= Just to
        after ^. itemList . L.listSelectedL @?= Just (if moves then 1 else 0)
        browsed @?= ["t" | k `elem` [KeyEnter, KeySpace]]
    rows =
      [ Row KeyArchive False None ToBeArchived True,
        Row KeyArchive False ToBeArchived ToBeArchived False,
        Row KeyArchive False (ToBeReminded tPrev) ToBeArchived True,
        Row KeyArchive True ReminderToBeRemoved ToBeArchived True,
        Row KeyRemind False None (ToBeReminded t7) True,
        Row KeyRemind False ToBeArchived (ToBeReminded t7) True,
        Row KeyRemind False (ToBeReminded tPrev) (ToBeReminded tPrev) False,
        Row KeyRemind True None ReminderToBeRemoved True,
        Row KeyRemind True ToBeArchived ReminderToBeRemoved True,
        Row KeyRemind True (ToBeReminded tPrev) (ToBeReminded tPrev) False,
        Row KeyRemind True ReminderToBeRemoved None True,
        Row KeyUnflag False None None False,
        Row KeyUnflag False ToBeArchived None True,
        Row KeyUnflag False (ToBeReminded tPrev) None True,
        Row KeyUnflag True ReminderToBeRemoved None True,
        Row KeyEnter False None ToBeArchived True,
        Row KeyEnter False ToBeArchived None True,
        Row KeyEnter False (ToBeReminded tPrev) None True,
        Row KeyEnter True ReminderToBeRemoved None True,
        Row KeySpace False None None False,
        Row KeySpace False ToBeArchived ToBeArchived False,
        Row KeySpace False (ToBeReminded tPrev) (ToBeReminded tPrev) False,
        Row KeySpace True ReminderToBeRemoved ReminderToBeRemoved False
      ]

-- | Items newest first: "a" (created last) sorts to the top.
dated :: Text -> String -> BookmarkItem
dated i day = item i (-1) & biCreated .~ read (day <> " 12:00:00 UTC")

threeItems :: HocketState
threeItems = syncForRender (insertItems [dated "a" "2026-01-03", dated "b" "2026-01-02", dated "c" "2026-01-01"] emptyState)

selectedId :: HocketState -> Maybe Text
selectedId s = (\(_, bit) -> bit ^. biId . _BookmarkItemId) <$> L.listSelectedElement (s ^. itemList)

at' :: Int -> HocketState -> HocketState
at' i = itemList %~ L.listMoveTo i

-- | How the list selection follows changes, before D2 makes it by id.
selectionTests :: TestTree
selectionTests =
  testGroup
    "selection"
    [ testCase "nothing selected yet: the first item" $
        selectedId threeItems @?= Just "a",
      testCase "the selected item disappears: the one now at its index" $ do
        selectedId (syncForRender (removeItem (BookmarkItemId "b") (at' 1 threeItems))) @?= Just "c"
        -- At the end, the index is clamped to the new last item.
        selectedId (syncForRender (removeItem (BookmarkItemId "c") (at' 2 threeItems))) @?= Just "b"
        selectedId (syncForRender (removeItems (map BookmarkItemId ["a", "b", "c"]) threeItems)) @?= Nothing,
      -- pins today's index-stable selection, flipped in Phase 4 (D2, plan §11 #20)
      testCase "an item inserted above keeps the index, so the cursor lands on another item" $
        selectedId (syncForRender (insertItem (dated "n" "2026-01-04") (at' 1 threeItems))) @?= Just "a",
      -- pins today's index-stable selection, flipped in Phase 4 (D2, plan §11 #20)
      testCase "a refilter that hides items above keeps the index too" $ do
        -- "a" has a reminder, so hiding future reminders hides it.
        let shown = syncForRender (threeItems & hsContents . ix (BookmarkItemId "a") . _2 . biReminder ?~ read "2030-01-01 07:00:00 UTC" & setShowFutureReminders True)
        selectedId (at' 1 shown) @?= Just "b"
        selectedId (syncForRender (setShowFutureReminders False (at' 1 shown))) @?= Just "c"
    ]
