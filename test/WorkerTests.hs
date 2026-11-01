{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module WorkerTests (workerTests) where

import CharacterizationTests (emptyState, item)
import Control.Exception (AsyncException (ThreadKilled), throwIO, try)
import Control.Lens ((^.))
import Control.Monad.State.Strict (execState, modify)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Events
import Network.Bookmark.Batch
import Network.Bookmark.Types (BookmarkItemBatch (..), BookmarkItemId (..))
import Network.Bookmark.Ui.State
import Network.Bookmark.Ui.Sync
import Test.Tasty
import Test.Tasty.HUnit
import Worker

workerTests :: TestTree
workerTests = testGroup "async workers" [fetchEmissionTests, fetchSlotTests, stepEmissionTests]

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
outcomeOf :: [(T.Text, Either T.Text Bool)] -> StepOutcome BookmarkItemId T.Text
outcomeOf script = runIdentity (runItems scope (pure . flip lookupScript script) (map (BookmarkItemId . fst) script))
  where
    scope e = if "5" `T.isPrefixOf` e then Global else ItemScoped
    lookupScript (BookmarkItemId i) = fromMaybe (Right True) . lookup i

stepEmissionTests :: TestTree
stepEmissionTests =
  testGroup
    "batch step emission"
    [ testCase "a clean step posts its outcome and clears the status" $
        let o = outcomeOf [("a", Right True)] in stepEvents archivedItemsEvt o @?= [archivedItemsEvt o, setStatusEvt Nothing],
      testCase "an aborted step still posts its successes first (B5)" $ do
        let o = outcomeOf [("a", Right True), ("b", Left "503"), ("c", Right True)]
        o ^. soOk @?= [BookmarkItemId "a"]
        stepEvents archivedItemsEvt o @?= [archivedItemsEvt o, asyncActionFailedEvt (Just "503")],
      testCase "an item-scoped failure is named in the status" $ do
        stepEvents archivedItemsEvt (outcomeOf [("a", Left "404")]) !! 1 @?= setStatusEvt (Just "failed: 404")
        stepEvents archivedItemsEvt (outcomeOf [("a", Right False)]) !! 1 @?= setStatusEvt (Just "failed: result: false"),
      testCase "runWhile stops after the first step that answers False" $
        execState (runWhile [modify (<> [1 :: Int]) >> pure True, modify (<> [2]) >> pure False, modify (<> [3]) >> pure True]) []
          @?= [1, 2]
    ]
