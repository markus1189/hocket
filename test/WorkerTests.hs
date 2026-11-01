{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module WorkerTests (workerTests) where

import CharacterizationTests (emptyState, item)
import ClassifyTests (failure, failureWithXError, response)
import Control.Exception (AsyncException (ThreadKilled), SomeException, displayException, evaluate, throwIO, toException, try)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.State.Strict (execState, modify, runState)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Foldable (for_)
import Data.Functor.Identity (runIdentity)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Network.Bookmark.App.Msg (Intent (Execute, Refresh), Msg (..), StepResult (..))
import Network.Bookmark.App.Update (update)
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
        fetchResult False (Left (Just "boom")) @?= FetchFailed (Just "boom"),
      testCase "a fetch posts one FetchCompleted with every page" $
        fetchResult True (Right [page1, page2]) @?= FetchCompleted True [page1, page2],
      testCase "an empty fetch still posts FetchCompleted" $
        fetchResult False (Right []) @?= FetchCompleted False [],
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

-- | Refresh, then the worker's one message, both through update: whether
-- the fetch slot is held after each.
fetchTrace :: Msg -> [Bool]
fetchTrace terminal = map asyncOpRunning [started, maybe started fst (update terminal started)]
  where
    Just (started, _) = update (Intent Refresh) emptyState

-- | Held through every event but the last, which frees it.
freedByLastOnly :: [Bool] -> [Bool]
freedByLastOnly trace = (True <$ drop 1 trace) <> [False]

fetchSlotTests :: TestTree
fetchSlotTests =
  testGroup
    "fetch slot"
    [ testCase "an empty fetch frees the slot (B1)" $
        fetchTrace (fetchResult False (Right [])) @?= [True, False],
      -- One message carries every page, so no page can free the slot early.
      testCase "a multi-page fetch holds the slot until its one message (B1b)" $
        fetchTrace (fetchResult True (Right [page1, page2])) @?= [True, False],
      testCase "a failed fetch frees the slot" $
        fetchTrace (fetchResult False (Left Nothing)) @?= [True, False]
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

-- | Execute, then the worker's messages, all through update: the slot and
-- the status after each message.
batchTrace :: [Msg] -> [(Bool, Maybe T.Text)]
batchTrace = map (\s -> (asyncOpRunning s, s ^. hsStatus)) . drop 1 . scanl deliver started
  where
    Just (started, _) = update (Intent Execute) batchState
    deliver s m = maybe s fst (update m s)

-- | What the batch worker posts for these step results, ending with its
-- one BatchFinished.
batchMsgs :: StepOutcome BookmarkItemId T.Text -> StepOutcome (BookmarkItemId, UTCTime) T.Text -> [Msg]
batchMsgs archive reminders = posted <> [BatchFinished summary]
  where
    (summary, posted) = runState (runBatch run [StepArchive, StepSetReminders]) []
    post m = modify (<> [m])
    run = \case
      StepArchive -> runStep post StepArchive Archived (pure archive)
      StepSetReminders -> runStep post StepSetReminders RemindersSetAt (pure reminders)
      StepRemoveReminders -> error "not planned"

batchSlotTests :: TestTree
batchSlotTests =
  testGroup
    "batch slot and status (B2)"
    [ testCase "an item-scoped archive failure: slot held until done, status names both steps" $ do
        let trace = batchTrace (batchMsgs (outcomeOf [("a", Left "400")]) remindersOk)
        map fst trace @?= freedByLastOnly (map fst trace)
        snd (last trace) @?= Just "archive 0/1 (1 failed: 400) · reminders ok",
      testCase "a global archive failure: slot held until done, reminders reported skipped" $ do
        let trace = batchTrace (batchMsgs (outcomeOf [("a", Left "503")]) remindersOk)
        map fst trace @?= freedByLastOnly (map fst trace)
        snd (last trace) @?= Just "archive 0/1 (aborted: 503) · reminders skipped: 503",
      testCase "a fully successful batch clears the status" $
        snd (last (batchTrace (batchMsgs (outcomeOf [("a", Right True)]) remindersOk))) @?= Nothing
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
      testCase "a step posts its computed outcome, even when a later call threw" $ do
        posted <- newIORef []
        let call bid = if bid == BookmarkItemId "b" then throwIO (userError "disk full") else pure True
        _ <- runStep (\e -> modifyIORef' posted (<> [e])) StepArchive Archived (runCalls call (map BookmarkItemId ["a", "b", "c"]))
        readIORef posted >>= \case
          [BatchStepStarted StepArchive, StepDone (Archived o)] ->
            (o ^. soOk, o ^. soSkipped, o ^. soAborted) @?= ([BookmarkItemId "a"], map BookmarkItemId ["b", "c"], Just "user error (disk full)")
          other -> assertFailure ("posted: " <> show other)
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
      testCase "an x-error header with invalid UTF-8 renders without throwing (B6)" $ do
        r <- try @SomeException (evaluate (maybe 0 T.length (errorMessageFromException (failureWithXError 404 (BS.pack [0xff, 0xfe, 0x41])))))
        either (assertFailure . ("threw: " <>) . takeWhile (/= '\n') . displayException) (@?= 3) r,
      testCase "an x-error with control characters is left out" $
        for_ ["a\rb", "a\tb", "a\ESCb"] $ \x -> do
          shortClientError (failureWithXError 404 x) @?= "404"
          fmap (T.isPrefixOf "Got status: ") (errorMessageFromException (failureWithXError 404 x)) @?= Just True,
      testCase "an x-error that is empty after stripping is left out" $
        for_ ["", "  "] $ \x -> do
          shortClientError (failureWithXError 404 x) @?= "404"
          fmap (T.isPrefixOf "Got status: ") (errorMessageFromException (failureWithXError 404 x)) @?= Just True,
      testCase "the batch summary uses the short form" $ do
        o <- runCalls (throwingOn [(2, throwIO (failure 404))]) [1, 2, 3]
        map snd (o ^. soFailed) @?= [CallFailed "404"]
    ]
