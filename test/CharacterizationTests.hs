{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Phase 0 pins of today's behaviour (plan §6), bugs included. Rows marked
-- "pins bug" are expected to flip in Phase 1.
module CharacterizationTests (characterizationTests) where

import qualified Brick.Widgets.List as L
import Control.Lens
import Data.Foldable (for_)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import Events
import Keys
import Network.Bookmark.Types
import Network.Bookmark.Ui.State
import Network.Bookmark.Ui.Sync
import Test.Tasty
import Test.Tasty.HUnit

characterizationTests :: TestTree
characterizationTests =
  testGroup "Characterization (Phase 0 pins)" [syncTests, stepResultTests, keyTableTests]

emptyState :: HocketState
emptyState = initialState (BookmarkCredentials (RaindropToken "") 0)

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
      -- pins bug B1b, flipped in Phase 1
      testCase "every fetched page frees the slot, even with more pages to come" $ do
        let Just fetching = tryAcquireAsyncOp OpFetchItems emptyState
            afterPage1 = applyFetched 1 [item "a" (-1)] False fetching
        asyncOpRunning fetching @?= True
        asyncOpRunning afterPage1 @?= False
    ]

stepResultTests :: TestTree
stepResultTests =
  testGroup
    "applyStepResult"
    [ testCase "archive removes the reported items only" $
        ids (applyStepResult StepArchive [BookmarkItemId "a"] twoFlagged) @?= ["b"],
      testCase "set-reminders stores the flag's time and clears the flag" $ do
        let after = applyStepResult StepSetReminders [BookmarkItemId "r"] reminderFlagged
        flagOf "r" after @?= Just None
        reminderOf "r" after @?= Just (Just t7),
      -- pins bug B3, flipped in Phase 1
      testCase "set-reminders after an unflag during the batch drops the reminder locally" $ do
        let unflagged = reminderFlagged & setPendingAction (BookmarkItemId "r") None
            after = applyStepResult StepSetReminders [BookmarkItemId "r"] unflagged
        flagOf "r" after @?= Just None
        reminderOf "r" after @?= Just Nothing,
      testCase "remove-reminders clears the reminder, and the flag only if still ReminderToBeRemoved" $ do
        let s =
              insertItems [withRem (item "x" (-1)), withRem (item "y" (-1))] emptyState
                & setPendingAction (BookmarkItemId "x") ReminderToBeRemoved
                & setPendingAction (BookmarkItemId "y") ToBeArchived
            after = applyStepResult StepRemoveReminders [BookmarkItemId "x", BookmarkItemId "y"] s
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

-- | Press a key, then deliver what it posted the way the handlers do, with
-- the render sync after every event. Returns the browsed item ids.
press :: ItemKey -> HocketState -> (HocketState, [Text])
press key s0 = foldl deliver (syncForRender s0, []) (itemKeyEvents key s0)
  where
    deliver (s, browsed) = \case
      HocketUi (ShiftItem bid) -> (syncForRender (shiftItem bid s), browsed)
      HocketUi (ShiftItemReminder bid) -> (syncForRender (shiftItemReminder bid t7 s), browsed)
      HocketUi (BrowseItem bit) -> (s, browsed <> [bit ^. biId . _BookmarkItemId])
      other -> error ("unexpected event from a key: " <> show other)

-- | Key, target has a reminder, flag before, flag after, cursor moves down.
data Row = Row ItemKey Bool PendingAction PendingAction Bool

keyTableTests :: TestTree
keyTableTests =
  testGroup "key semantics (focused item, flag before → after)" $
    map rowTest rows
      <> [ testCase "no focused item: every key posts nothing" $
             for_ [minBound .. maxBound] $ \k ->
               (k, length (itemKeyEvents k (syncForRender emptyState))) @?= (k, 0)
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
        -- pins bug B4, flipped in Phase 1
        Row KeyArchive False (ToBeReminded tPrev) None True,
        -- pins bug B4, flipped in Phase 1
        Row KeyArchive True ReminderToBeRemoved None True,
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
