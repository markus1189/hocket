{-# LANGUAGE OverloadedStrings #-}

module PlanTests (planTests) where

import Control.Lens ((&), (.~), (^.))
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Time (UTCTime)
import Gen (genModel)
import Hedgehog (forAllWith, property, (===))
import Network.Bookmark.App.Plan
import Network.Bookmark.Types
import Network.Bookmark.Ui.State
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

t7 :: UTCTime
t7 = read "2026-11-02 06:00:00 UTC"

item :: Text -> BookmarkItem
item i = BookmarkItem (BookmarkItemId i) "" "" "" "link" [] False t7 t7 "" "" 0 [] (-1) False Nothing

-- | Ids chosen so text order ("10" < "9") differs from numeric order.
withFlags :: [(Text, PendingAction)] -> HocketState
withFlags fs = initialState & hsContents .~ Map.fromList [(BookmarkItemId i, (a, item i)) | (i, a) <- fs]

ids :: [Text] -> [BookmarkItemId]
ids = map BookmarkItemId

planTests :: TestTree
planTests =
  testGroup
    "planBatch"
    [ testCase "nothing flagged is no plan" $
        planBatch (withFlags [("1", None)]) @?= Nothing,
      testCase "archive only, in id order" $
        planBatch (withFlags [("9", ToBeArchived), ("10", ToBeArchived), ("2", None)]) @?= Just (BatchPlan (ids ["10", "9"]) [] []),
      testCase "set reminders carry the flagged time" $
        planBatch (withFlags [("9", ToBeReminded t7), ("10", ToBeReminded t7)]) @?= Just (BatchPlan [] [(BookmarkItemId "10", t7), (BookmarkItemId "9", t7)] []),
      testCase "remove reminders only" $
        planBatch (withFlags [("3", ReminderToBeRemoved)]) @?= Just (BatchPlan [] [] (ids ["3"])),
      testCase "mixed flags split by type" $
        planBatch (withFlags [("1", ToBeArchived), ("2", ToBeReminded t7), ("3", ReminderToBeRemoved), ("4", None)])
          @?= Just (BatchPlan (ids ["1"]) [(BookmarkItemId "2", t7)] (ids ["3"])),
      testProperty "no plan exactly when no step has work; ids match the flags" $ property $ do
        s <- forAllWith (show . (^. hsContents)) genModel
        let flaggedAs p = [k | (k, (a, _)) <- Map.toList (s ^. hsContents), p a]
            plan = planBatch s
        isNothing plan === null (batchStepsWithWork s)
        fmap bpArchive plan === emptyToNothing (flaggedAs (== ToBeArchived)) plan
        fmap (map fst . bpSetReminders) plan === emptyToNothing (flaggedAs isToBeReminded) plan
        fmap bpRemoveReminders plan === emptyToNothing (flaggedAs (== ReminderToBeRemoved)) plan
    ]
  where
    emptyToNothing xs = fmap (const xs)
