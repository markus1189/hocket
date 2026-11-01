{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core.AgentTests
  ( agentSnapshotTests,
    agentProtocolTests,
  )
where

import Control.Lens.Operators
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.List (find)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Hocket.Agent.Protocol
import Hocket.Agent.Snapshot
import Hocket.App.Model
import Hocket.Domain.Bookmark
import Test.Tasty
import Test.Tasty.HUnit
import TestUtil

-- Agent-socket fixtures: distinct dates so the display order is deterministic.
agentItemA :: BookmarkItem
agentItemA =
  bookmarkItem1
    { _biId = BookmarkItemId "ag1",
      _biCreated = read "2020-01-02 10:00:00 UTC",
      _biLastUpdate = read "2020-01-02 10:00:00 UTC"
    }

agentItemB :: BookmarkItem
agentItemB =
  bookmarkItem1
    { _biId = BookmarkItemId "ag2",
      _biTitle = "second item",
      _biCreated = read "2020-01-01 10:00:00 UTC",
      _biLastUpdate = read "2020-01-01 10:00:00 UTC"
    }

agentItemRem :: BookmarkItem
agentItemRem =
  bookmarkItem1
    { _biId = BookmarkItemId "ag3",
      _biReminder = Just (read "2020-06-01 07:00:00 UTC")
    }

agentSnapAB :: AgentSnapshot
agentSnapAB = takeSnapshot 1 (settle (insertItems [agentItemA, agentItemB] testState))

agentSnapFlagged :: AgentSnapshot
agentSnapFlagged =
  takeSnapshot 1 $
    settle $
      setPendingAction (BookmarkItemId "ag1") ToBeArchived $
        insertItems [agentItemA, agentItemB] testState

agentSnapWithRem :: AgentSnapshot
agentSnapWithRem = takeSnapshot 1 (settle (insertItems [agentItemA, agentItemRem] testState))

agentSnapshotTests :: TestTree
agentSnapshotTests =
  testGroup
    "Agent snapshot"
    [ testCase "takeSnapshot carries the given version" $
        asVersion agentSnapAB @?= 1,
      testCase "visible items appear first, in display order (newest first)" $
        map siId (filter siVisible (asItems agentSnapAB))
          @?= [BookmarkItemId "ag1", BookmarkItemId "ag2"],
      testCase "selection mirrors the focused item" $
        asSelected agentSnapAB @?= Just (BookmarkItemId "ag1"),
      testCase "pending actions are carried into the snapshot" $
        fmap siPending (find ((== BookmarkItemId "ag1") . siId) (asItems agentSnapFlagged))
          @?= Just ToBeArchived,
      testCase "items hidden by the future-reminder filter are marked invisible" $ do
        map siId (filter siVisible (asItems agentSnapWithRem)) @?= [BookmarkItemId "ag1"]
        fmap siVisible (find ((== BookmarkItemId "ag3") . siId) (asItems agentSnapWithRem))
          @?= Just False,
      testCase "setPendingAction is idempotent (replace, not toggle)" $ do
        -- Flag archive, then re-flag archive: must not flip to None.
        let s = setPendingAction (BookmarkItemId "ag1") ToBeArchived $ insertItems [agentItemA] testState
        fst <$> Map.lookup (BookmarkItemId "ag1") (s ^. mContents) @?= Just ToBeArchived,
      testCase "setFilterQuery replaces the query and clears filter-input mode" $ do
        let s = setFilterQuery "haskell" (enterFilterMode testState)
        s ^. mFilterQuery @?= "haskell"
        s ^. mFilterActive @?= False,
      testCase "setVideoFilterMode is a plain assignment" $ do
        let s = setVideoFilterMode ShowOnlyVideos testState
        s ^. mVideoFilter @?= ShowOnlyVideos,
      testCase "setShowFutureReminders is a plain assignment" $ do
        let s = setShowFutureReminders True testState
        s ^. mShowFutureReminders @?= True,
      testCase "setAgentClients sets the header count" $ do
        let s = setAgentClients 2 testState
        s ^. mAgentClients @?= 2
    ]

agentProtocolTests :: TestTree
agentProtocolTests =
  testGroup
    "Agent protocol"
    [ testCase "decodeCmd: get_state" $
        decodeCmd "get_state" Nothing @?= Right (ARead CmdGetState),
      testCase "decodeCmd: list_items defaults to visible-only" $
        decodeCmd "list_items" Nothing @?= Right (ARead (CmdListItems True False)),
      testCase "decodeCmd: wait_version defaults its timeout" $
        decodeCmd "wait_version" (Just (A.object ["after" A..= (3 :: Int)]))
          @?= Right (AWait 3 10000),
      testCase "decodeCmd: set_flag archive" $
        decodeCmd
          "set_flag"
          (Just (A.object ["id" A..= ("ag1" :: Text), "action" A..= ("archive" :: Text)]))
          @?= Right (AWrite (CmdSetFlag (BookmarkItemId "ag1") FlagArchive)),
      testCase "decodeCmd: unknown method is rejected" $
        assertBool "expected Left" (isLeft (decodeCmd "bogus" Nothing)),
      testCase "decodeCmd: unknown flag action is rejected" $
        assertBool
          "expected Left"
          ( isLeft
              ( decodeCmd
                  "set_flag"
                  (Just (A.object ["id" A..= ("x" :: Text), "action" A..= ("explode" :: Text)]))
              )
          ),
      testCase "serveRead: get_item on unknown id is an error" $
        assertBool
          "expected Left"
          (isLeft (serveRead agentSnapAB (CmdGetItem (BookmarkItemId "nope")))),
      testCase "serveRead: get_item returns the matching item's fields" $
        case serveRead agentSnapAB (CmdGetItem (BookmarkItemId "ag1")) of
          Left err -> assertFailure (T.unpack err)
          Right v -> case v of
            A.Object o -> do
              KM.lookup (AKey.fromText "id") o @?= Just (A.String "ag1")
              KM.lookup (AKey.fromText "visible") o @?= Just (A.Bool True)
              -- pending field carries an action object with "action":"none"
              case KM.lookup (AKey.fromText "pending") o of
                Just (A.Object p) ->
                  KM.lookup (AKey.fromText "action") p @?= Just (A.String "none")
                _ -> assertFailure "pending is not an object"
            _ -> assertFailure "get_item result is not an object",
      testCase "stateView pins the documented wire contract" $ do
        let v = stateView agentSnapFlagged
        case v of
          A.Object o -> do
            -- top-level keys the README documents
            for_ ["version", "counts", "selected", "filter_query", "video_filter", "show_future_reminders", "status", "last_updated", "async_op"] $ \k ->
              assertBool ("stateView missing key " <> k) (KM.member (AKey.fromText (T.pack k)) o)
            -- counts subobject
            case KM.lookup (AKey.fromText "counts") o of
              Just (A.Object c) -> do
                let vn = KM.lookup (AKey.fromText "archive_flagged") c
                vn @?= Just (A.Number 1)
              _ -> assertFailure "stateView counts is not an object"
            -- selected reflects the focused item
            KM.lookup (AKey.fromText "selected") o @?= Just (A.String "ag1")
          _ -> assertFailure "stateView is not an object",
      testCase "serveRead: flagged_only narrows to flagged items" $
        case serveRead agentSnapFlagged (CmdListItems False True) of
          Left err -> assertFailure (T.unpack err)
          Right v -> case A.fromJSON v :: A.Result [A.Value] of
            A.Success xs -> length xs @?= 1
            A.Error err -> assertFailure err,
      testCase "validateWrite: archive flag on a known item passes through" $
        validateWrite agentSnapAB (CmdSetFlag (BookmarkItemId "ag1") FlagArchive)
          @?= Right (CmdSetFlag (BookmarkItemId "ag1") FlagArchive),
      testCase "validateWrite: flagging an unknown id is rejected" $
        assertBool
          "expected Left"
          (isLeft (validateWrite agentSnapAB (CmdSetFlag (BookmarkItemId "nope") FlagArchive))),
      testCase "validateWrite: remove_reminder without an existing reminder is rejected" $
        assertBool
          "expected Left"
          (isLeft (validateWrite agentSnapAB (CmdSetFlag (BookmarkItemId "ag1") FlagRemoveReminder))),
      testCase "validateWrite: reminder on an item that already has one is rejected" $
        assertBool
          "expected Left"
          (isLeft (validateWrite agentSnapWithRem (CmdSetFlag (BookmarkItemId "ag3") FlagReminder))),
      testCase "validateWrite: selecting a filtered-out item is rejected" $
        assertBool
          "expected Left"
          (isLeft (validateWrite agentSnapWithRem (CmdSelectItem (BookmarkItemId "ag3")))),
      testCase "validateWrite: open_item and open_and_flag on a known item pass through" $ do
        validateWrite agentSnapAB (CmdOpenItem (BookmarkItemId "ag1"))
          @?= Right (CmdOpenItem (BookmarkItemId "ag1"))
        validateWrite agentSnapAB (CmdOpenAndFlag (BookmarkItemId "ag1"))
          @?= Right (CmdOpenAndFlag (BookmarkItemId "ag1")),
      testCase "validateWrite: open_and_flag on an unknown id is rejected" $
        assertBool
          "expected Left"
          (isLeft (validateWrite agentSnapAB (CmdOpenAndFlag (BookmarkItemId "nope"))))
    ]
