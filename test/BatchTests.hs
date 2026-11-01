{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module BatchTests (batchTests) where

import CharacterizationTests (emptyState, item)
import Control.Lens
import Control.Monad.State.Strict (State, modify, runState)
import Data.List (findIndex, sort)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Text as T
import Gen (genFailureScript)
import Hedgehog (cover, forAll, property, withTests, (===))
import Network.Bookmark.Batch
import Network.Bookmark.Types (BookmarkItemId (..), PendingAction (..))
import Network.Bookmark.Ui.State
import Network.Bookmark.Ui.Sync (applyArchived)
import Network.Raindrop (classify)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog (testProperty)

batchTests :: TestTree
batchTests = testGroup "runItems (B5, D6)" [unitTests, outcomeProperty]

-- | Run a step against a scripted fake call; the outcome, and the items the
-- fake was called for, in order.
runScripted :: forall a e. (Eq a) => (e -> ErrorScope) -> [(a, Either e Bool)] -> (StepOutcome a e, [a])
runScripted scope script = runState (runItems scope call (map fst script)) [] & _2 %~ reverse
  where
    call :: a -> State [a] (Either e Bool)
    call x = modify (x :) >> pure (fromJust (lookup x script))

-- | Status codes standing in for errors: 5xx is global, the rest item-scoped.
statusScope :: Int -> ErrorScope
statusScope c = if c >= 500 then Global else ItemScoped

isCallFailed, isResultFalse :: Failure e -> Bool
isCallFailed = \case
  CallFailed _ -> True
  ResultFalse -> False
isResultFalse = not . isCallFailed

okAll :: [Int] -> [(Int, Either Int Bool)]
okAll = map (,Right True)

unitTests :: TestTree
unitTests =
  testGroup
    "scripted"
    [ testCase "item 2 of 5 gets a 404: 4 ok, 1 failed, every call made" $ do
        let (outcome, calls) = runScripted statusScope (okAll [1] <> [(2, Left 404)] <> okAll [3, 4, 5])
        (outcome ^. soOk, outcome ^. soFailed, outcome ^. soSkipped, outcome ^. soAborted)
          @?= ([1, 3, 4, 5], [(2, CallFailed 404)], [], Nothing)
        calls @?= [1 .. 5],
      testCase "item 2 of 5 gets a 503: 1 ok, 2-5 skipped, no call after item 2" $ do
        let (outcome, calls) = runScripted statusScope (okAll [1] <> [(2, Left 503)] <> okAll [3, 4, 5])
        (outcome ^. soOk, outcome ^. soFailed, outcome ^. soSkipped, outcome ^. soAborted)
          @?= ([1], [], [2, 3, 4, 5], Just 503)
        calls @?= [1, 2],
      testCase "result: false fails only that item" $ do
        let (outcome, calls) = runScripted statusScope [(1, Right False), (2, Right True)]
        (outcome ^. soOk, outcome ^. soFailed) @?= ([2], [(1, ResultFalse)])
        calls @?= [1, 2],
      testCase "one call for a group spreads its outcome over the group (archive)" $ do
        let group = [(["a", "b"], Right True)] :: [([T.Text], Either Int Bool)]
            spread r = concatOutcome (fst (runScripted statusScope [(["a", "b"], r)]))
        concatOutcome (fst (runScripted statusScope group)) ^. soOk @?= ["a", "b"]
        spread (Left 404) ^. soFailed @?= [("a", CallFailed 404), ("b", CallFailed 404)]
        (spread (Left 503) ^. soSkipped, spread (Left 503) ^. soAborted) @?= (["a", "b"], Just 503)
    ]

outcomeProperty :: TestTree
outcomeProperty =
  -- The rarest covered class measures ~5% (an item-scoped error), so a 1%
  -- floor over 1000 tests sits more than 5 standard deviations away.
  testProperty "runItems partitions the items, stops at the first global error, and only ok items lose their flag" $
    withTests 1000 $
      property $ do
        results <- forAll genFailureScript
        let items = [0 .. length results - 1]
            (outcome, calls) = runScripted classify (zip items results)
            firstGlobal = findIndex (either ((== Global) . classify) (const False)) results
            ok = outcome ^. soOk
        cover 1 "aborted" (isJust firstGlobal)
        cover 1 "ran to the end" (isNothing firstGlobal)
        cover 1 "an item-scoped error" (any (isCallFailed . snd) (outcome ^. soFailed))
        cover 1 "result: false" (any (isResultFalse . snd) (outcome ^. soFailed))
        sort (ok <> map fst (outcome ^. soFailed) <> outcome ^. soSkipped) === items
        outcome ^. soSkipped === maybe [] (`drop` items) firstGlobal
        calls === maybe items (\i -> take (i + 1) items) firstGlobal
        isJust (outcome ^. soAborted) === isJust firstGlobal
        let bid = BookmarkItemId . T.pack . show
            flagged = insertItems [item (T.pack (show i)) (-1) | i <- items] emptyState & setAllFlagsToArchive
            after = applyArchived (map bid ok) flagged
        Map.map fst (after ^. hsContents) === Map.fromList [(bid i, ToBeArchived) | i <- items, i `notElem` ok]
