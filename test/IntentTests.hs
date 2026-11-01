{-# LANGUAGE OverloadedStrings #-}

module IntentTests (intentTests) where

import Data.List (nub)
import Data.Time (utc)
import Gen (genAgentCmd)
import Hedgehog (assert, forAll, property)
import Network.Bookmark.Agent.Intent (toIntent)
import Network.Bookmark.Agent.Protocol
import Network.Bookmark.App.Msg
import Network.Bookmark.Types (BookmarkItemId (..))
import Network.Bookmark.Ui.State (VideoFilterMode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

now :: Now
now = Now (read "2026-11-01 10:00:00 UTC") utc

i1 :: BookmarkItemId
i1 = BookmarkItemId "1"

-- | One row per 'WriteCmd' constructor; the property below keeps it so.
rows :: [(WriteCmd, Intent)]
rows =
  [ (CmdSetFlag i1 FlagReminder, SetFlag i1 FlagReminder now),
    (CmdClearFlags, ClearAllFlags),
    (CmdFlagAllArchive, FlagAllArchive),
    (CmdExecute, Execute),
    (CmdRefresh, Refresh),
    (CmdSetFilter "q", SetFilter "q"),
    (CmdSetVideoFilter HideVideos, SetVideoFilter HideVideos),
    (CmdSetShowFutureReminders True, SetShowFutureReminders True),
    (CmdSelectItem i1, Select i1),
    (CmdOpenItem i1, Open (ById i1)),
    (CmdOpenAndFlag i1, OpenAndFlag i1),
    (CmdSetStatus "hi", SetStatus "agent: hi")
  ]

constructorName :: WriteCmd -> String
constructorName = takeWhile (/= ' ') . show

intentTests :: TestTree
intentTests =
  testGroup
    "toIntent"
    [ testCase "every write command maps like the shell's toEvent" $
        map (toIntent now . fst) rows @?= map snd rows,
      testCase "the table has one row per constructor" $
        length (nub (map (constructorName . fst) rows)) @?= length rows,
      testProperty "every generated write command has a row" $ property $ do
        cmd <- forAll genAgentCmd
        case cmd of
          AWrite w -> assert (constructorName w `elem` map (constructorName . fst) rows)
          _ -> pure ()
    ]
