{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core.StateTests
  ( xKeyBatchConcurrencyTests,
    hocketStateTests,
  )
where

import Control.Lens (ix, view, _1, _2)
import Control.Lens.Operators
import qualified Data.Map as Map
import Data.Time.Clock (UTCTime (..))
import Hocket.App.Model
import Hocket.Domain.Bookmark
import Test.Tasty
import Test.Tasty.HUnit
import TestUtil

bookmarkItem2 :: BookmarkItem
bookmarkItem2 = bookmarkItem1 {_biLastUpdate = read "2016-05-22 12:54:59 UTC", _biTitle = "newer title"}

bookmarkItem1_same_ts_diff_title :: BookmarkItem
bookmarkItem1_same_ts_diff_title = bookmarkItem1 {_biTitle = "title for same ts item"}

-- Fixtures for the 'X'-batch regression test: three items, one per bucket.
batchReminderTime :: UTCTime
batchReminderTime = read "2016-05-23 07:00:00 UTC"

batchArmArchive :: BookmarkItem
batchArmArchive = bookmarkItem1 {_biId = BookmarkItemId "bx1"}

batchArmSetRem :: BookmarkItem
batchArmSetRem = bookmarkItem1 {_biId = BookmarkItemId "bx2", _biReminder = Nothing}

batchArmRmvRem :: BookmarkItem
batchArmRmvRem = bookmarkItem1 {_biId = BookmarkItemId "bx3", _biReminder = Just (read "2016-05-22 12:33:11 UTC")}

-- State with pending work in all three 'X' buckets at once.
armedBatchState :: Model
armedBatchState =
  insertItems [batchArmArchive, batchArmSetRem, batchArmRmvRem] testState
    & togglePendingAction (_biId batchArmArchive)
    & togglePendingActionToReminder (_biId batchArmSetRem) batchReminderTime
    & togglePendingActionToReminder (_biId batchArmRmvRem) batchReminderTime

-- One 'X' press must cover every pending bucket via a single slot-owning batch,
-- and a second press/fetch is refused while it runs. These assertions fail if
-- the three ops are ever uncoordinated again or a bucket is silently dropped.
xKeyBatchConcurrencyTests :: TestTree
xKeyBatchConcurrencyTests =
  testGroup
    "X-key batch (regression)"
    [ testCase "one X press plans every non-empty bucket" $ do
        batchStepsWithWork testState @?= []
        batchStepsWithWork justArchive @?= [StepArchive]
        batchStepsWithWork justSetRem @?= [StepSetReminders]
        batchStepsWithWork justRmvRem @?= [StepRemoveReminders]
        batchStepsWithWork armedBatchState @?= [StepArchive, StepSetReminders, StepRemoveReminders],
      testCase "batch holds the single slot; a double X is refused, completion frees it" $ do
        asyncOpRunning armedBatchState @?= False
        let Just fired = tryAcquireAsyncOp OpExecuteBatch armedBatchState
        asyncOpRunning fired @?= True
        ( case tryAcquireAsyncOp OpExecuteBatch fired of
            Nothing -> pure ()
            Just _ -> assertFailure "a second X started while a batch owns the slot"
          )
        ( case tryAcquireAsyncOp OpFetchItems fired of
            Nothing -> pure ()
            Just _ -> assertFailure "fetch started while the batch owns the slot"
          )
        let done = completeAsyncOp fired
        asyncOpRunning done @?= False
        case tryAcquireAsyncOp OpExecuteBatch done of
          Just inFlight -> asyncOpRunning inFlight @?= True
          Nothing -> assertFailure "batch refused after slot freed"
    ]
  where
    justArchive = insertItems [batchArmArchive] testState & togglePendingAction (_biId batchArmArchive)
    justSetRem = insertItems [batchArmSetRem] testState & togglePendingActionToReminder (_biId batchArmSetRem) batchReminderTime
    justRmvRem = insertItems [batchArmRmvRem] testState & togglePendingActionToReminder (_biId batchArmRmvRem) batchReminderTime

hocketStateTests =
  testGroup
    "Model insertItem/insertItems"
    [ testCase "insertItem: new item gets None status and correct data" $
        let s = insertItem bookmarkItem1 testState
         in Map.lookup (_biId bookmarkItem1) (view mContents s) @?= Just (None, bookmarkItem1),
      testCase "insertItem: updates with newer item" $
        let stateWithOldItem = insertItem bookmarkItem1 testState
            stateWithOldItemPending = stateWithOldItem & mContents . ix (_biId bookmarkItem1) . _1 .~ ToBeArchived
            stateAfterUpdate = insertItem bookmarkItem2 stateWithOldItemPending
         in Map.lookup (_biId bookmarkItem1) (view mContents stateAfterUpdate) @?= Just (ToBeArchived, bookmarkItem2),
      testCase "insertItem: older item does not overwrite newer; status and data preserved" $
        let stateWithNewerItem = insertItem bookmarkItem2 testState
            stateWithNewerItemPending = stateWithNewerItem & mContents . ix (_biId bookmarkItem2) . _1 .~ ToBeArchived
            stateAfterAttemptedUpdate = insertItem bookmarkItem1 stateWithNewerItemPending
         in Map.lookup (_biId bookmarkItem2) (view mContents stateAfterAttemptedUpdate) @?= Just (ToBeArchived, bookmarkItem2),
      testCase "insertItem: item with same timestamp does overwrite; status preserved, data updated" $
        let stateWithOriginalItem = insertItem bookmarkItem1 testState
            stateWithOriginalItemPending = stateWithOriginalItem & mContents . ix (_biId bookmarkItem1) . _1 .~ ToBeArchived
            stateAfterAttemptedUpdate = insertItem bookmarkItem1_same_ts_diff_title stateWithOriginalItemPending
         in Map.lookup (_biId bookmarkItem1) (view mContents stateAfterAttemptedUpdate) @?= Just (ToBeArchived, bookmarkItem1_same_ts_diff_title),
      testCase "insertItems: inserting an item that is present overwrites if newer" $
        let s = insertItems [bookmarkItem1, bookmarkItem2] testState
         in fmap (view (_2 . biTitle)) (Map.lookup (_biId bookmarkItem1) (view mContents s)) @?= Just "newer title",
      testCase "insertItems: inserting an item that is present overwrites if newer, insertion order does not matter" $
        let s = insertItems [bookmarkItem2, bookmarkItem1] testState
         in fmap (view (_2 . biTitle)) (Map.lookup (_biId bookmarkItem1) (view mContents s)) @?= Just "newer title",
      testCase "insertItems: basic insertion count" $
        length (insertItems [bookmarkItem1] testState ^. mContents) @?= 1
    ]
