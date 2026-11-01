{-# LANGUAGE OverloadedStrings #-}

module VisibleTests (visibleTests) where

import Control.Lens ((^.))
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as V
import Gen (genModel, genVideoFilterMode)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hocket.App.Model
import Hocket.Domain.Bookmark
import Hocket.Domain.Filter (fuzzyFilterMatch)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | Mostly queries that hit generated items (domains, the video tag),
-- some that rarely do.
genQuery :: Gen Text
genQuery = Gen.frequency [(2, pure ""), (3, Gen.element ["you", "git", "video", "ex org", "news"]), (1, Gen.text (Range.linear 1 4) Gen.alphaNum)]

data Inputs = Inputs Bool VideoFilterMode Text (Map.Map BookmarkItemId (PendingAction, BookmarkItem))

instance Show Inputs where
  show (Inputs f v q c) = show (f, v, q, c)

genInputs :: Gen Inputs
genInputs = Inputs <$> Gen.bool <*> genVideoFilterMode <*> genQuery <*> ((^. hsContents) <$> genModel)

passes :: Inputs -> BookmarkItem -> Bool
passes (Inputs showFuture video query _) item =
  (showFuture || isNothing (item ^. biReminder))
    && ( case video of
           NoVideoFilter -> True
           ShowOnlyVideos -> isVideoBookmark item
           HideVideos -> not (isVideoBookmark item)
       )
    && (query == "" || fuzzyFilterMatch query (bookmarkSearchText item))

run :: Inputs -> V.Vector BookmarkItem
run (Inputs f v q c) = visibleItems f v q c

ids :: V.Vector BookmarkItem -> Set.Set BookmarkItemId
ids = Set.fromList . map (^. biId) . V.toList

visibleTests :: TestTree
visibleTests =
  testGroup
    "visibleItems"
    [ testProperty "sorted by sort date, newest first" $ property $ do
        is <- forAll genInputs
        let dates = map (Down . getSortDate) (V.toList (run is))
        assert (and (zipWith (<=) dates (drop 1 dates))),
      testProperty "every visible item passes every active filter" $ property $ do
        is <- forAll genInputs
        assert (all (passes is) (run is)),
      testProperty "every item that passes all filters is visible" $ property $ do
        is@(Inputs _ _ _ c) <- forAll genInputs
        ids (run is) === Set.fromList [k | (k, (_, i)) <- Map.toList c, passes is i],
      testProperty "the filters commute: all at once is each one alone, intersected" $ property $ do
        Inputs f v q c <- forAll genInputs
        let alone = [visibleItems f NoVideoFilter "" c, visibleItems True v "" c, visibleItems True NoVideoFilter q c]
        ids (visibleItems f v q c) === foldr1 Set.intersection (map ids alone)
    ]
