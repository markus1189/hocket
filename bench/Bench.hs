{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.DeepSeq (NFData (..))
import Control.Lens (view, (^.))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (utc)
import Data.Time.Clock (addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.Bookmark.Agent.Intent (toIntent)
import Network.Bookmark.Agent.Protocol (FlagAction (..), WriteCmd (..), validateWrite)
import Network.Bookmark.Agent.Snapshot (asItems, siVisible, takeSnapshot)
import Network.Bookmark.App.Msg (Msg (Intent), Now (..))
import Network.Bookmark.App.Update (update)
import Network.Bookmark.Types
import Network.Bookmark.Ui.State
import Test.Tasty.Bench

-- Show forces every field of every item; good enough for one-off setup.
instance NFData HocketState where
  rnf s = rnf (show (s ^. hsContents))

vocab :: [Text]
vocab =
  [ "haskell",
    "lens",
    "tui",
    "brick",
    "nix",
    "flake",
    "rust",
    "compiler",
    "type",
    "monad",
    "async",
    "parser",
    "vector",
    "database",
    "postgres",
    "kernel",
    "linux",
    "emacs",
    "keyboard",
    "design",
    "review",
    "performance",
    "memory",
    "garbage",
    "collector",
    "streaming",
    "network",
    "socket",
    "protocol",
    "json",
    "api"
  ]

-- Deterministic LCG so runs are comparable without a random dependency.
pick :: Int -> Text
pick k = vocab !! (((k * 1103515245 + 12345) `mod` 2147483648) `mod` length vocab)

mkItem :: Int -> BookmarkItem
mkItem i =
  BookmarkItem
    { _biId = BookmarkItemId (T.pack (show i)),
      _biLink = if i `mod` 10 == 0 then "https://youtube.com/watch?v=" <> T.pack (show i) else "https://example.com/" <> T.pack (show i),
      _biExcerpt = T.unwords [pick (i * 31 + j) | j <- [1 .. 25]],
      _biNote = if i `mod` 5 == 0 then T.unwords [pick (i * 7 + j) | j <- [1 .. 5]] else "",
      _biType = "link",
      _biTags = [pick (i * 13 + j) | j <- [1 .. i `mod` 4]],
      _biRemoved = False,
      _biCreated = addUTCTime (fromIntegral i * 60) epoch,
      _biLastUpdate = addUTCTime (fromIntegral i * 60) epoch,
      _biDomain = "example.com",
      _biTitle = T.unwords [pick (i * 17 + j) | j <- [1 .. 6]],
      _biSort = i,
      _biHighlights = [],
      _biCollectionId = -1,
      _biImportant = False,
      _biReminder = if i `mod` 10 == 3 then Just (addUTCTime (fromIntegral i * 3600) epoch) else Nothing
    }
  where
    epoch = posixSecondsToUTCTime 1700000000

mkState :: Int -> HocketState
mkState n =
  settle $
    insertItems (map mkItem [1 .. n]) initialState

itemId :: Int -> BookmarkItemId
itemId = BookmarkItemId . T.pack . show

flagMany :: [Int] -> HocketState -> HocketState
flagMany is s0 = foldl' step s0 is
  where
    step st i = case validateWrite (takeSnapshot 0 st) (CmdSetFlag (itemId i) FlagArchive) of
      Left e -> error (T.unpack e)
      Right cmd -> applyWrite cmd st

-- | An agent write as the TUI applies it: update, then the settle that
-- follows every event.
applyWrite :: WriteCmd -> HocketState -> HocketState
applyWrite cmd = settle . fst . update (Intent (toIntent benchNow cmd))

benchNow :: Now
benchNow = Now (posixSecondsToUTCTime 1700000000) utc

sizes :: [Int]
sizes = [1000, 5000, 20000]

queries :: [(String, Text)]
queries = [("empty", ""), ("1 word", "haskell"), ("3 words", "haskell lens tui"), ("miss", "zzqx")]

visibleCount :: HocketState -> Int
visibleCount = length . view hsVisible

main :: IO ()
main =
  defaultMain
    [ bgroup
        (show n)
        [ bgroup
            "syncForRender"
            [ bench name $ nf (visibleCount . settle . setFilterQuery q) s
            | (name, q) <- queries
            ],
          bgroup
            "videoFilter"
            [ bench name $ nf (visibleCount . settle . setVideoFilterMode m) s
            | (name, m) <- [("only", ShowOnlyVideos), ("hide", HideVideos)]
            ],
          bgroup
            "agent"
            [ bench "validateWrite first" $ nf (either (const 0) (const 1 :: WriteCmd -> Int) . validateWrite (takeSnapshot 0 s)) (CmdSetFlag (itemId n) FlagArchive),
              bench "validateWrite last" $ nf (either (const 0) (const 1 :: WriteCmd -> Int) . validateWrite (takeSnapshot 0 s)) (CmdSetFlag (itemId 1) FlagArchive),
              -- What the server + TUI do per flag write: validate against the
              -- current snapshot, apply, re-render, re-snapshot.
              bench "flag 100 (validate+sync)" $ nf (visibleCount . flagMany [1 .. 100]) s,
              bench "flag 100 (sync only)" $ nf (visibleCount . foldl' (\st i -> applyWrite (CmdSetFlag (itemId i) FlagArchive) st) s) [1 .. 100]
            ],
          bench "hsNumItems" $ nf (\st -> let c = hsNumItems st in c ^. icNone + c ^. icFutureReminders) s,
          -- SnapshotItem fields are strict, so WHNF of each list cell forces the projection.
          bench "takeSnapshot" $ nf (foldl' (\a i -> a + fromEnum (siVisible i)) 0 . asItems . takeSnapshot 0) s
        ]
    | n <- sizes,
      let s = mkState n
    ]
