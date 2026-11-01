{-# LANGUAGE OverloadedStrings #-}

module Core.NavTests (navTests) where

import qualified Brick.Widgets.List as L
import Control.Lens ((&), (.~), (^.))
import Data.Foldable (for_)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Vector as V
import Hedgehog (forAll, forAllWith, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hocket.App.Nav
import Hocket.Domain.Bookmark
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

t0 :: UTCTime
t0 = read "2026-11-01 10:00:00 UTC"

item :: Text -> BookmarkItem
item i = BookmarkItem (BookmarkItemId i) "" "" "" "link" [] False t0 t0 "" i 0 [] (-1) False Nothing

-- | Twenty items "a".."t"; "c" and "p" are flagged.
items :: V.Vector BookmarkItem
items = V.fromList [item (T.singleton c) | c <- ['a' .. 't']]

flags :: BookmarkItemId -> PendingAction
flags bid = fromMaybe None (Map.lookup bid (Map.fromList [(BookmarkItemId "c", ToBeArchived), (BookmarkItemId "p", ReminderToBeRemoved)]))

nav :: Int -> Nav -> Maybe Text -> Maybe Text
nav h n sel = (\(BookmarkItemId t) -> t) <$> navigate h n items flags (BookmarkItemId <$> sel)

navTests :: TestTree
navTests =
  testGroup
    "navigate"
    [ testCase "up and down move by one and clamp at the ends" $ do
        nav 5 Down (Just "b") @?= Just "c"
        nav 5 Up (Just "b") @?= Just "a"
        nav 5 Up (Just "a") @?= Just "a"
        nav 5 Down (Just "t") @?= Just "t",
      testCase "top and bottom" $ do
        nav 5 Top (Just "k") @?= Just "a"
        nav 5 Bottom (Just "k") @?= Just "t"
        nav 5 Top Nothing @?= Just "a"
        nav 5 Bottom Nothing @?= Just "t",
      testCase "from no selection: forward starts at the top, backward at the bottom (listMoveBy)" $ do
        nav 5 Down Nothing @?= Just "a"
        nav 5 Up Nothing @?= Just "t"
        nav 5 (Page 1) Nothing @?= Just "a"
        nav 5 (Page (-1)) Nothing @?= Just "t",
      testCase "a page is the viewport height, clamped" $ do
        nav 5 (Page 1) (Just "a") @?= Just "f"
        nav 5 (Page (-1)) (Just "f") @?= Just "a"
        nav 5 (Page 2) (Just "a") @?= Just "k"
        nav 5 (Page 1) (Just "r") @?= Just "t"
        nav 5 (Page (-1)) (Just "b") @?= Just "a",
      testCase "half pages round half to even, like brick's round" $ do
        nav 5 (HalfPage 1) (Just "a") @?= Just "c" -- round 2.5 = 2
        nav 7 (HalfPage 1) (Just "a") @?= Just "e" -- round 3.5 = 4
        nav 6 (HalfPage (-1)) (Just "j") @?= Just "g", -- 3
      testCase "J/K jump to the next and previous flagged item" $ do
        nav 5 NextFlagged (Just "a") @?= Just "c"
        nav 5 NextFlagged (Just "c") @?= Just "p"
        nav 5 PrevFlagged (Just "t") @?= Just "p"
        nav 5 PrevFlagged (Just "p") @?= Just "c",
      testCase "J/K without a further flagged item, or from no selection, stay put" $ do
        nav 5 NextFlagged (Just "p") @?= Just "p"
        nav 5 PrevFlagged (Just "c") @?= Just "c"
        nav 5 NextFlagged Nothing @?= Nothing,
      testCase "an empty list has no selection" $
        for_ [Up, Down, Top, Bottom, Page 1, HalfPage (-1), NextFlagged, PrevFlagged] $ \n ->
          (n, navigate 5 n V.empty flags (Just (BookmarkItemId "a"))) @?= (n, Nothing),
      testCase "a selected id that is gone counts as no selection" $
        nav 5 Down (Just "zz") @?= Just "a",
      testProperty "up, down, top and bottom agree with brick's own list moves" $ property $ do
        n <- forAll (Gen.int (Range.linear 0 8))
        sel <- forAll (Gen.maybe (Gen.int (Range.linear 0 (max 0 (n - 1)))))
        (move, brickMove) <- forAllWith (show . fst) (Gen.element [(Up, L.listMoveUp), (Down, L.listMoveDown), (Top, L.listMoveToBeginning), (Bottom, L.listMoveToEnd)])
        let v = V.take n items
            selId = (\i -> (v V.! i) ^. biId) <$> (if n == 0 then Nothing else sel)
            brickList = L.listMoveTo 0 (L.list () v 1) & L.listSelectedL .~ (if n == 0 then Nothing else sel)
            expected = (\i -> (v V.! i) ^. biId) <$> (brickMove brickList ^. L.listSelectedL)
        navigate 5 move v flags selId === expected
    ]
