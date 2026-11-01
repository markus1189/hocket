{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | stepEffect against the IO workers over fakePort: for the same effect,
-- the same messages, the same store and the same number of requests.
module Shell.SimulateTests (simulateTests) where

import Control.Lens ((.~), (^.))
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Time (UTCTime, addUTCTime)
import Gen (genBookmarkItemId, genFailRule, genFake, genUTCTime)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hocket.Adapter.Fake (FakeRaindrop, emptyFake, fakePort, frItems, frRules)
import Hocket.App.Batch (soAborted, soFailed)
import Hocket.App.Effect (Effect (..), Opener (..))
import Hocket.App.Msg (Msg (..), StepResult (..))
import Hocket.App.Plan (BatchPlan (..))
import Hocket.App.Sync (planSync)
import Hocket.Port (RaindropPort (..))
import Hocket.Shell.Simulate (stepEffect)
import Hocket.Shell.Worker (runBatch, runFetch)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testPropertyNamed)

simulateTests :: TestTree
simulateTests = testGroup "stepEffect" [equivalence, desktopTests]

t0 :: UTCTime
t0 = read "2026-11-01 12:00:00 UTC"

-- | The time of the n-th request: t0+1s, t0+2s, … as the fixture tests' port.
clock :: Int -> UTCTime
clock n = addUTCTime (fromIntegral (n + 1)) t0

equivalence :: TestTree
equivalence =
  testPropertyNamed "stepEffect posts and stores what the IO workers do over fakePort" "prop_stepEffect" . withTests 300 . property $ do
    store <- forAll ((frRules .~) <$> Gen.list (Range.linear 0 3) genFailRule <*> genFake)
    effect <- forAll (genEffect store)
    io <- evalIO (runIO effect store)
    cover 15 "a request failed" (any failed (fst io))
    cover 15 "a batch" (isBatch effect)
    cover 15 "a fetch" (not (isBatch effect))
    stepEffect clock effect (store, 0) === io
  where
    isBatch = \case StartBatch _ -> True; _ -> False

-- | The worker the TUI runs, in IO, over the IORef fake.
runIO :: Effect -> FakeRaindrop -> IO ([Msg], (FakeRaindrop, Int))
runIO effect store = do
  ref <- newIORef store
  ticks <- newIORef 0
  posted <- newIORef []
  let port = fakePort (atomicModifyIORef' ticks (\n -> (n + 1, clock n))) ref
      post m = modifyIORef' posted (<> [m])
  case effect of
    StartFetch plan -> runFetch (runRaindrop port) post plan
    StartBatch plan -> runBatch (runRaindrop port) post plan
    _ -> pure ()
  (,) <$> readIORef posted <*> ((,) <$> readIORef ref <*> readIORef ticks)

-- | A fetch with or without a last sync, or a batch over the store's ids
-- and some it does not have.
genEffect :: FakeRaindrop -> Gen Effect
genEffect store =
  Gen.choice
    [ StartFetch . planSync <$> Gen.maybe (fromIntegral <$> Gen.int (Range.linear 1500000000 1900000000)),
      StartBatch <$> (BatchPlan <$> ids <*> (traverse (\i -> (,) i <$> genUTCTime) =<< ids) <*> ids)
    ]
  where
    ids = (<>) <$> Gen.subsequence (Map.keys (store ^. frItems)) <*> Gen.list (Range.linear 0 2) genBookmarkItemId

failed :: Msg -> Bool
failed = \case
  FetchFailed _ -> True
  BatchFinished (Just _) -> True
  StepDone (Archived o) -> bad (o ^. soFailed) (o ^. soAborted)
  StepDone (RemindersSetAt o) -> bad (o ^. soFailed) (o ^. soAborted)
  StepDone (RemindersRemoved o) -> bad (o ^. soFailed) (o ^. soAborted)
  _ -> False
  where
    bad fs ab = not (null fs) || isJust ab

desktopTests :: TestTree
desktopTests =
  testCase "the browser, the clipboard and Halt post nothing and touch no store" $
    map (\e -> stepEffect clock e (store, 3)) [Halt, OpenUrl Firefox "https://example.org/", CopyText "https://example.org/"]
      @?= replicate 3 ([], (store, 3))
  where
    store = emptyFake 999
