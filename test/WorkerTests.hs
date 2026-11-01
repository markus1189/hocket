{-# LANGUAGE OverloadedStrings #-}

module WorkerTests (workerTests) where

import CharacterizationTests (item)
import Events
import Network.Bookmark.Types (BookmarkItemBatch (..))
import Test.Tasty
import Test.Tasty.HUnit
import Worker

workerTests :: TestTree
workerTests = testGroup "async workers" [fetchEmissionTests]

page1, page2 :: BookmarkItemBatch
page1 = BookmarkItemBatch 10 [item "a" (-1)] 1
page2 = BookmarkItemBatch 20 [item "b" (-1)] 1

fetchEmissionTests :: TestTree
fetchEmissionTests =
  testGroup
    "fetch emission"
    [ testCase "a failed fetch posts the failure" $
        fetchResultEvents False (Left (Just "boom")) @?= [asyncActionFailedEvt (Just "boom")],
      testCase "a fetch posts one FetchedItems per page" $
        fetchResultEvents True (Right [page1, page2])
          @?= [setStatusEvt Nothing, fetchedItemsEvt 10 [item "a" (-1)] True, fetchedItemsEvt 20 [item "b" (-1)] True],
      -- pins bug B1, flipped in Phase 1
      testCase "an empty fetch posts nothing that frees the slot" $
        fetchResultEvents False (Right []) @?= [setStatusEvt Nothing]
    ]
