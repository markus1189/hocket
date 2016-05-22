{-# LANGUAGE OverloadedStrings #-}
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.HUnit

import           Control.Lens (view,_2)
import           Control.Lens.Operators
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ord

import           Network.Pocket.Ui.State
import           Network.Pocket.Types

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

-- properties :: TestTree
-- properties = testGroup "Properties" [scProps, qcProps]

-- -- qcProps = testGroup "(checked by QuickCheck)"
--   [ QC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , QC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , QC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
--   ]

pocketItem1 :: PocketItem
pocketItem1 = PocketItem "excerpt"
                         True
                         "given title"
                         (URL "www.url.com")
                         No
                         No
                         True
                         True
                         (PocketItemId "1")
                         (PocketItemId "2")
                         "resolved title"
                         (URL "www.resolved-url.com")
                         5
                         Normal
                         1463904791
                         1463904791
                         1463904791
                         1463904791
                         42
                         []

pocketItem2 :: PocketItem
pocketItem2 = pocketItem1 {_timeUpdated = 1463906099, _givenTitle = "newer given title" }

unitTests = testGroup "HocketState"
  [ testCase "inserting items into the initial state" $
      length (insertItem pocketItem1 initialState ^. hsContents) @?= 1
  , testCase "inserting an item that is present overwrites if newer" $
      let s = insertItems [pocketItem1,pocketItem2] initialState
      in fmap (view (_2 . givenTitle)) (Map.lookup (view itemId pocketItem1) (view hsContents s)) @?= Just "newer given title"
  , testCase "inserting an item that is present overwrites if newer, insertion order does not matter" $
      let s = insertItems [pocketItem2,pocketItem1] initialState
      in fmap (view (_2 . givenTitle)) (Map.lookup (view itemId pocketItem1) (view hsContents s)) @?= Just "newer given title"
  ]
