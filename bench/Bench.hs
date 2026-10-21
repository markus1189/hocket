{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import qualified Brick.Widgets.List as L
import Control.DeepSeq (NFData (..))
import Control.Lens (view, (^.))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.Bookmark.Agent.Snapshot (asItems, siVisible, takeSnapshot)
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
  syncForRender $
    insertItems (map mkItem [1 .. n]) (initialState (BookmarkCredentials (RaindropToken "") 0))

sizes :: [Int]
sizes = [1000, 5000, 20000]

queries :: [(String, Text)]
queries = [("empty", ""), ("1 word", "haskell"), ("3 words", "haskell lens tui"), ("miss", "zzqx")]

visibleCount :: HocketState -> Int
visibleCount = length . view (itemList . L.listElementsL)

main :: IO ()
main =
  defaultMain
    [ bgroup
        (show n)
        [ bgroup
            "syncForRender"
            [ bench name $ nf (visibleCount . syncForRender . setFilterQuery q) s
            | (name, q) <- queries
            ],
          bench "hsNumItems" $ nf (\st -> let c = hsNumItems st in c ^. icNone + c ^. icFutureReminders) s,
          -- SnapshotItem fields are strict, so WHNF of each list cell forces the projection.
          bench "takeSnapshot" $ nf (foldl' (\a i -> a + fromEnum (siVisible i)) 0 . asItems . takeSnapshot 0) s
        ]
    | n <- sizes,
      let s = mkState n
    ]
