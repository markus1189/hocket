{-# LANGUAGE OverloadedStrings #-}

-- | The agent wire pinned byte for byte (I7). The golden was recorded from
-- master's list-based takeSnapshot (before Phase 4 commit 6), so a match
-- shows the move to mSelected/mVisible left the wire unchanged.
module SnapshotGoldenTests (snapshotGoldenTests) where

import Control.Lens ((&), (.~), (?~), (^.))
import qualified Data.Aeson as A
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Vector as V
import Hocket.Agent.Protocol (stateView)
import Hocket.Agent.Snapshot (takeSnapshot)
import Hocket.App.Model
import Hocket.Domain.Bookmark
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

at :: String -> UTCTime
at = read

item :: Text -> Text -> [Text] -> String -> Maybe String -> BookmarkItem
item i link tags created rem' =
  BookmarkItem (BookmarkItemId i) link ("excerpt " <> i) ("note " <> i) "link" tags False (at created) (at created) "example.org" ("title " <> i) 0 [] (-1) (i == "2") (at <$> rem')

-- | Every flag, a reminder hidden by the future-reminder filter, a video
-- hidden by the video filter, the second visible item selected, a status.
state :: Model
state =
  let s0 =
        initialModel
          & insertItems
            [ item "1" "https://example.org/1" [] "2026-10-01 10:00:00 UTC" Nothing,
              item "2" "https://example.org/2" ["haskell"] "2026-10-02 10:00:00 UTC" Nothing,
              item "3" "https://example.org/3" [] "2026-10-03 10:00:00 UTC" Nothing,
              item "4" "https://example.org/4" [] "2026-10-04 10:00:00 UTC" (Just "2026-11-05 07:00:00 UTC"),
              item "5" "https://www.youtube.com/watch?v=x" ["video"] "2026-10-05 10:00:00 UTC" Nothing
            ]
          & setPendingAction (BookmarkItemId "1") ToBeArchived
          & setPendingAction (BookmarkItemId "3") (ToBeReminded (at "2026-11-02 07:00:00 UTC"))
          & setPendingAction (BookmarkItemId "4") ReminderToBeRemoved
          & setVideoFilterMode HideVideos
          & setShowFutureReminders False
          & mStatus ?~ "archive ok"
          & mLastUpdated ?~ 1790000000
          & settle
   in settle (s0 & mSelected ?~ ((s0 ^. mVisible) V.! 1 ^. biId))

snapshotGoldenTests :: TestTree
snapshotGoldenTests =
  testGroup
    "agent snapshot wire"
    [ goldenVsString "takeSnapshot JSON" "test/golden/snapshot.golden" (pure (A.encode (takeSnapshot 7 state) <> "\n")),
      goldenVsString "stateView JSON" "test/golden/state-view.golden" (pure (A.encode (stateView (takeSnapshot 7 state)) <> "\n"))
    ]
