{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module WorkerTests (workerTests) where

import CharacterizationTests (emptyState, item)
import ClassifyTests (failure, failureWithXError, response)
import Control.Exception (AsyncException (ThreadKilled), throwIO, toException, try)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.State.Strict (execState, modify, runState)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BSC
import Data.Functor.Identity (runIdentity)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Events
import Network.Bookmark.Batch
import Network.Bookmark.Types (BookmarkItemBatch (..), BookmarkItemId (..), PendingAction (..))
import Network.Bookmark.Ui.State
import Network.Bookmark.Ui.Sync
import Servant.Client.Core (ClientError (..))
import Test.Tasty
import Test.Tasty.HUnit
import Worker

workerTests :: TestTree
workerTests = testGroup "async workers" [fetchEmissionTests, fetchSlotTests, stepEmissionTests, batchSlotTests, stepExceptionTests, errorRenderingTests]

page1, page2 :: BookmarkItemBatch
page1 = BookmarkItemBatch 10 [item "a" (-1)] 1
page2 = BookmarkItemBatch 20 [item "b" (-1)] 1

fetchEmissionTests :: TestTree
fetchEmissionTests =
  testGroup
    "fetch emission"
    [ testCase "a failed fetch posts FetchFailed" $
        fetchResultEvents False (Left (Just "boom")) @?= [fetchFailedEvt (Just "boom")],
      testCase "a fetch posts one FetchCompleted with every page" $
        fetchResultEvents True (Right [page1, page2]) @?= [setStatusEvt Nothing, fetchCompletedEvt True [page1, page2]],
      testCase "an empty fetch still posts FetchCompleted" $
        fetchResultEvents False (Right []) @?= [setStatusEvt Nothing, fetchCompletedEvt False []],
      testCase "FetchCompleted merges the pages in order, as page-by-page delivery did" $ do
        let s = applyFetchCompleted False [page1, page2] emptyState
        Map.keys (s ^. hsContents) @?= [BookmarkItemId "a", BookmarkItemId "b"]
        s ^. hsLastUpdated @?= Just 20,
      testCase "trySync returns a synchronous exception, rendered on one line" $ do
        r <- trySync (throwIO (userError "disk full") :: IO ())
        either (Just . exceptionMessage) (const Nothing) r @?= Just "user error (disk full)",
      testCase "trySync rethrows an asynchronous exception" $ do
        r <- try @AsyncException (trySync (throwIO ThreadKilled :: IO ()))
        either Just (const Nothing) r @?= Just ThreadKilled
    ]

-- | Deliver a fetch's events the way the handlers apply them, starting from
-- an acquired fetch slot; whether the slot is still held after each event.
slotTrace :: [HocketEvent] -> [Bool]
slotTrace = map asyncOpRunning . drop 1 . scanl deliver acquired
  where
    Just acquired = tryAcquireAsyncOp OpFetchItems emptyState
    deliver s = \case
      HocketAsync (FetchCompleted isAll batches) -> applyFetchCompleted isAll batches s
      HocketAsync (FetchFailed _) -> completeAsyncOp s
      HocketUi (SetStatus _) -> s
      other -> error ("unexpected event from a fetch: " <> show other)

-- | Held through every event but the last, which frees it.
freedByLastOnly :: [Bool] -> [Bool]
freedByLastOnly trace = (True <$ drop 1 trace) <> [False]

fetchSlotTests :: TestTree
fetchSlotTests =
  testGroup
    "fetch slot"
    [ testCase "an empty fetch frees the slot (B1)" $
        let trace = slotTrace (fetchResultEvents False (Right [])) in trace @?= freedByLastOnly trace,
      testCase "a multi-page fetch holds the slot until its last event (B1b)" $
        let trace = slotTrace (fetchResultEvents True (Right [page1, page2])) in trace @?= freedByLastOnly trace,
      testCase "a failed fetch frees the slot" $
        let trace = slotTrace (fetchResultEvents False (Left Nothing)) in trace @?= freedByLastOnly trace
    ]

-- | A step outcome from a script; errors starting with "5" are global.
scriptedOutcome :: (Eq a) => [(a, Either T.Text Bool)] -> StepOutcome a T.Text
scriptedOutcome script = runIdentity (runItems scope (\x -> pure (fromMaybe (Right True) (lookup x script))) (map fst script))
  where
    scope e = if "5" `T.isPrefixOf` e then Global else ItemScoped

outcomeOf :: [(T.Text, Either T.Text Bool)] -> StepOutcome BookmarkItemId T.Text
outcomeOf script = scriptedOutcome [(BookmarkItemId i, r) | (i, r) <- script]

stepEmissionTests :: TestTree
stepEmissionTests =
  testGroup
    "batch steps"
    [ testCase "runSteps stops after the first aborted step" $ do
        let run step = modify (<> [step]) >> pure (first (const ()) (outcomeOf [("x", if step == 2 then Left "503" else Right True)]))
            (ran, called) = runState (runSteps run [1 :: Int, 2, 3]) []
        (map fst ran, called) @?= ([1, 2], [1, 2]),
      testCase "batchStatus names every failure, the abort and what was skipped" $
        batchStatus
          [StepArchive, StepSetReminders, StepRemoveReminders]
          [ (StepArchive, erase (outcomeOf [("a", Right True)])),
            (StepSetReminders, erase (outcomeOf [("r1", Left "404"), ("r2", Right False), ("r3", Right True), ("r4", Left "503"), ("r5", Right True)]))
          ]
          @?= Just "archive ok · reminders 1/5 (2 failed: 404, result: false; aborted: 503) · remove-reminders skipped: 503",
      testCase "batchStatus is Nothing when every step was clean" $
        batchStatus [StepArchive] [(StepArchive, erase (outcomeOf [("a", Right True)]))] @?= Nothing
    ]
  where
    erase = first (const ())

-- | Items for one archive and one set-reminders bucket.
batchState :: HocketState
batchState =
  insertItems [item "a" (-1), item "r" (-1)] emptyState
    & setPendingAction (BookmarkItemId "a") ToBeArchived
    & setPendingAction (BookmarkItemId "r") (ToBeReminded batchTime)

batchTime :: UTCTime
batchTime = read "2026-11-02 06:00:00 UTC"

-- | Deliver a batch's events the way the handlers apply them, from an
-- acquired batch slot; the slot and the status after each event.
batchTrace :: [HocketEvent] -> [(Bool, Maybe T.Text)]
batchTrace = map (\s -> (asyncOpRunning s, s ^. hsStatus)) . drop 1 . scanl deliver acquired
  where
    Just acquired = tryAcquireAsyncOp OpExecuteBatch batchState
    deliver s = \case
      HocketAsync (ArchivedItems o) -> applyArchived (o ^. soOk) s
      HocketAsync (RemindersSet o) -> applyRemindersSet (o ^. soOk) s
      HocketAsync (RemindersRemoved o) -> applyRemindersRemoved (o ^. soOk) s
      HocketAsync ExecuteBatchDone -> completeAsyncOp s
      HocketUi (SetStatus t) -> s & hsStatus .~ t
      other -> error ("unexpected event from a batch: " <> show other)

-- | What the batch worker posts for these step results, ending with the
-- 'finally'-guarded ExecuteBatchDone.
batchEvents :: StepOutcome BookmarkItemId T.Text -> StepOutcome (BookmarkItemId, UTCTime) T.Text -> [HocketEvent]
batchEvents archive reminders = execState (runBatch post run [StepArchive, StepSetReminders]) [] <> [executeBatchDoneEvt]
  where
    post e = modify (<> [e])
    run = \case
      StepArchive -> postOutcome post archivedItemsEvt archive
      StepSetReminders -> postOutcome post remindersSetEvt reminders
      StepRemoveReminders -> error "not planned"

batchSlotTests :: TestTree
batchSlotTests =
  testGroup
    "batch slot and status (B2)"
    [ testCase "an item-scoped archive failure: slot held until done, status names both steps" $ do
        let trace = batchTrace (batchEvents (outcomeOf [("a", Left "400")]) remindersOk)
        map fst trace @?= freedByLastOnly (map fst trace)
        snd (last trace) @?= Just "archive 0/1 (1 failed: 400) · reminders ok",
      testCase "a global archive failure: slot held until done, reminders reported skipped" $ do
        let trace = batchTrace (batchEvents (outcomeOf [("a", Left "503")]) remindersOk)
        map fst trace @?= freedByLastOnly (map fst trace)
        snd (last trace) @?= Just "archive 0/1 (aborted: 503) · reminders skipped: 503",
      testCase "a fully successful batch clears the status" $
        snd (last (batchTrace (batchEvents (outcomeOf [("a", Right True)]) remindersOk))) @?= Nothing
    ]
  where
    remindersOk = scriptedOutcome [((BookmarkItemId "r", batchTime), Right True)]

-- | A call that answers ok, except for the items it fails with an exception.
throwingOn :: [(Int, IO Bool)] -> Int -> IO Bool
throwingOn failures i = fromMaybe (pure True) (lookup i failures)

stepExceptionTests :: TestTree
stepExceptionTests =
  testGroup
    "exceptions in a batch step"
    [ testCase "a non-ClientError mid-step aborts it and keeps the successes so far" $ do
        o <- runCalls (throwingOn [(2, throwIO (userError "disk full"))]) [1, 2, 3]
        (o ^. soOk, o ^. soSkipped, o ^. soAborted) @?= ([1], [2, 3], Just "user error (disk full)"),
      testCase "a ClientError is scoped by classify: a 404 fails only that item" $ do
        o <- runCalls (throwingOn [(2, throwIO (failure 404))]) [1, 2, 3]
        (o ^. soOk, map fst (o ^. soFailed), o ^. soAborted) @?= ([1, 3], [2], Nothing),
      testCase "a step that throws before its first call skips every item" $ do
        o <- guardStep [1 :: Int, 2] (throwIO (userError "no log dir"))
        (o ^. soOk, o ^. soSkipped, o ^. soAborted) @?= ([], [1, 2], Just "user error (no log dir)"),
      testCase "the batch reports such a step as aborted, holds the slot, and skips the rest" $ do
        let trace = batchTrace (batchEvents (abortedBefore [BookmarkItemId "a"] "user error (no log dir)") remindersOk)
        map fst trace @?= freedByLastOnly (map fst trace)
        snd (last trace) @?= Just "archive 0/1 (aborted: user error (no log dir)) · reminders skipped: user error (no log dir)"
    ]
  where
    remindersOk = scriptedOutcome [((BookmarkItemId "r", batchTime), Right True)]

-- | The batch summary names an error the way D6 does ("2 failed: 404").
errorRenderingTests :: TestTree
errorRenderingTests =
  testGroup
    "batch error rendering"
    [ testCase "a failure response is its status code" $
        shortClientError (failure 404) @?= "404",
      testCase "a short x-error is appended" $
        shortClientError (failureWithXError 404 "Not found") @?= "404 Not found",
      testCase "a long x-error is left out" $
        shortClientError (failureWithXError 400 (BSC.replicate 80 'x')) @?= "400",
      testCase "a connection error is offline" $
        shortClientError (ConnectionError (toException (userError "Network.Socket.connect: does not exist"))) @?= "offline",
      testCase "decode and content-type failures are a bad response" $ do
        shortClientError (DecodeFailure "Error in $: key \"items\" not found" (response 200)) @?= "bad response"
        shortClientError (UnsupportedContentType "text/html" (response 200)) @?= "bad response"
        shortClientError (InvalidContentTypeHeader (response 200)) @?= "bad response",
      testCase "the batch summary uses the short form" $ do
        o <- runCalls (throwingOn [(2, throwIO (failure 404))]) [1, 2, 3]
        map snd (o ^. soFailed) @?= [CallFailed "404"]
    ]
