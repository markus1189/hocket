{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core.FilterTests
  ( sanitizeForDisplayTests,
    fuzzyMatchTests,
    fuzzyFilterMatchTests,
    filterTuningTests,
    filterStateTests,
  )
where

import Brick.Widgets.Edit (applyEdit, editContentsL)
import qualified Brick.Widgets.List as L
import Control.Lens (to, view)
import Control.Lens.Operators
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import Data.Text.Zipper.Generic.Words (deletePrevWord)
import qualified Data.Vector as V
import Hocket.App.Model
import Hocket.Domain.Bookmark
import Hocket.Domain.Filter (fuzzyFilterMatch, fuzzyMatch, sanitizeForDisplay)
import Hocket.Shell.Tui.Reconcile
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import TestUtil

typeInFilter :: Text -> UiState -> UiState
typeInFilter t = uiEditor %~ applyEdit (Z.insertMany t)

sanitizeForDisplayTests :: TestTree
sanitizeForDisplayTests =
  testGroup
    "sanitizeForDisplay"
    [ testCase "preserves plain ASCII" $
        sanitizeForDisplay "Hello, World!" @?= "Hello, World!",
      testCase "preserves Latin diacritics" $
        sanitizeForDisplay "Café résumé naïve" @?= "Café résumé naïve",
      testCase "preserves CJK characters" $
        sanitizeForDisplay "漢字テスト" @?= "漢字テスト",
      testCase "replaces supplementary-plane emoji with two spaces" $
        sanitizeForDisplay "Beach day \x1F30A\&\x1F3D6" @?= "Beach day     ",
      testCase "replaces BMP dingbat with two spaces" $
        sanitizeForDisplay "\x270C" @?= "  ",
      testCase "replaces star-struck emoji with two spaces" $
        sanitizeForDisplay "\x1F929" @?= "  ",
      testCase "replaces newline with space" $
        sanitizeForDisplay "line1\nline2" @?= "line1 line2",
      testCase "replaces carriage return and tab with space" $
        sanitizeForDisplay "a\rb\tc" @?= "a b c",
      testCase "replaces C0 control chars with space" $
        sanitizeForDisplay "x\ESCy\BELz" @?= "x y z",
      testCase "replaces DEL and C1 control chars with space" $
        sanitizeForDisplay "a\DELb\x80\&c\x9F\&d" @?= "a b c d",
      testCase "leaves empty text unchanged" $
        sanitizeForDisplay "" @?= "",
      testCase "preserves mixed Unicode and ASCII" $
        sanitizeForDisplay "Foo — Bar – Baz «quoted»" @?= "Foo — Bar – Baz «quoted»",
      testCase "strips ZWJ between emojis (each base becomes two spaces)" $
        sanitizeForDisplay "\x1F468\&\x200D\&\x1F469" @?= "    ",
      testCase "strips Fitzpatrick modifier; base dingbat becomes two spaces" $
        sanitizeForDisplay "\x270C\&\x1F3FC" @?= "  ",
      testCase "strips variation selector; base dingbat becomes two spaces" $
        sanitizeForDisplay "\x270C\&\xFE0F" @?= "  ",
      testCase "real-world Carport title aligns: every codepoint is Vty-narrow" $
        sanitizeForDisplay "EINFACH und SCHNELL zum eigenem Carport \x1F929 Die Anleitung \x270C\&\x1F3FC"
          @?= "EINFACH und SCHNELL zum eigenem Carport    Die Anleitung   "
    ]

fuzzyMatchTests :: TestTree
fuzzyMatchTests =
  testGroup
    "fuzzyMatch"
    [ testCase "empty needle matches non-empty haystack" $
        fuzzyMatch "" "anything" @?= True,
      testCase "empty needle matches empty haystack" $
        fuzzyMatch "" "" @?= True,
      testCase "exact substring matches" $
        fuzzyMatch "hask" "haskell" @?= True,
      testCase "subsequence with gaps matches" $
        fuzzyMatch "hkl" "haskell" @?= True,
      testCase "case-insensitive: upper needle, lower haystack" $
        fuzzyMatch "HSK" "haskell" @?= True,
      testCase "case-insensitive: lower needle, upper haystack" $
        fuzzyMatch "hsk" "HASKELL" @?= True,
      testCase "out-of-order fails" $
        fuzzyMatch "lha" "haskell" @?= False,
      testCase "needle longer than match fails" $
        fuzzyMatch "haskellx" "haskell" @?= False
    ]

fuzzyFilterMatchTests :: TestTree
fuzzyFilterMatchTests =
  testGroup
    "fuzzyFilterMatch"
    [ -- The key regression: a term may not span word boundaries. "api" is a
      -- subsequence of "apple pie" (a,p in "apple"; i in "pie") but of no
      -- single word, so the stricter matcher rejects it.
      testCase "term does not match across word boundaries" $
        fuzzyFilterMatch "api" "apple pie" @?= False,
      testCase "the same string still subsequence-matches the whole blob" $
        fuzzyMatch "api" "apple pie" @?= True,
      -- Intra-word gaps are still tolerated.
      testCase "intra-word subsequence still matches" $
        fuzzyFilterMatch "hkl" "haskell" @?= True,
      -- Space-separated terms are ANDed; each must match some word.
      testCase "all terms must match (success)" $
        fuzzyFilterMatch "hask lens" "haskell lens tutorial" @?= True,
      testCase "all terms must match (one term absent fails)" $
        fuzzyFilterMatch "lens zzz" "haskell lens tutorial" @?= False,
      -- Empty / whitespace-only queries match everything.
      testCase "empty query matches anything" $
        fuzzyFilterMatch "" "anything" @?= True,
      testCase "whitespace-only query matches anything" $
        fuzzyFilterMatch "   " "anything" @?= True
    ]

filterTestBookmark :: BookmarkItem
filterTestBookmark =
  bookmarkItem1
    { _biId = BookmarkItemId "filter-1",
      _biTitle = "uniquetitle",
      _biDomain = "uniquedomain.example",
      _biExcerpt = "uniqueexcerpt",
      _biNote = "uniquenote",
      _biTags = ["zzqtag"]
    }

filterVideoBookmark :: BookmarkItem
filterVideoBookmark =
  bookmarkItem1
    { _biId = BookmarkItemId "filter-video",
      _biTitle = "uniquevideo",
      _biDomain = "youtube.com",
      _biLink = "https://youtube.com/watch",
      _biExcerpt = "",
      _biNote = "",
      _biTags = []
    }

filterOtherBookmark :: BookmarkItem
filterOtherBookmark =
  bookmarkItem1
    { _biId = BookmarkItemId "filter-other",
      _biTitle = "somethingelse",
      _biDomain = "other.example",
      _biExcerpt = "",
      _biNote = "",
      _biTags = []
    }

-- | A 10-item fixture for exercising and tuning the live fuzzy filter.
--
-- Each item has distinct text across title/domain/excerpt/note and a distinct
-- creation date that ascends with the id (t01 oldest .. t10 newest). Because
-- 'settle' sorts descending by 'getSortDate' (reminder if present, else
-- '_biCreated'), the default render order is newest-first: t10, t09, .. t01.
--
-- All reminders are Nothing, so every item is visible under the default state
-- and sorts purely by '_biCreated'. Tuning hooks:
--   * Matching: edit the title/domain/excerpt/note fields or the query. The
--     match target is title+domain+excerpt+note (tags are excluded).
--   * Ordering: edit '_biCreated'. To make an item sort by a reminder instead,
--     set '_biReminder' AND run with @mShowFutureReminders .~ True@, otherwise
--     'hasFutureReminder' hides it.
tuningBookmarks :: [BookmarkItem]
tuningBookmarks =
  [ mk "t01" "2024-01-15" "Haskell lens tutorial" "school.dev" "learn optics and lenses" "read later",
    mk "t02" "2024-02-15" "Functional programming in Scala" "blog.scala.org" "monads and functors" "",
    mk "t03" "2024-03-15" "Brick TUI library guide" "hackage.haskell.org" "terminal user interfaces" "for hocket",
    mk "t04" "2024-04-15" "Async concurrency patterns" "stackoverflow.com" "threads and channels" "",
    mk "t05" "2024-05-15" "Parsing JSON with aeson" "hackage.haskell.org" "decode and encode records" "api work",
    mk "t06" "2024-06-15" "Dhall configuration language" "dhall-lang.org" "typed config files" "config.dhall",
    mk "t07" "2024-07-15" "Raindrop API reference" "developer.raindrop.io" "bookmark rest endpoints" "integration",
    mk "t08" "2024-08-15" "Fuzzy finding algorithms" "junegunn.github.io" "subsequence matching like fzf" "filter feature",
    mk "t09" "2024-09-15" "Nix flakes explained" "nix.dev" "reproducible builds" "flake update",
    mk "t10" "2024-10-15" "GHC optimization tips" "well-typed.com" "strictness and inlining" "performance"
  ]
  where
    mk :: Text -> String -> Text -> Text -> Text -> Text -> BookmarkItem
    mk i created title domain excerpt note =
      bookmarkItem1
        { _biId = BookmarkItemId i,
          _biCreated = read (created <> " 00:00:00 UTC"),
          _biLastUpdate = read (created <> " 00:00:00 UTC"),
          _biTitle = title,
          _biDomain = domain,
          _biExcerpt = excerpt,
          _biNote = note,
          _biLink = "https://" <> domain,
          _biTags = [],
          _biReminder = Nothing
        }

-- | Run a query through the real render pipeline and return the ordered items.
runTuning :: Text -> [BookmarkItem]
runTuning q =
  let base = insertItems tuningBookmarks testState
      filtered = settle (base & mFilterQuery .~ q)
   in V.toList (view mVisible filtered)

-- | True iff the list is in non-increasing order (the render invariant).
isDescending :: (Ord a) => [a] -> Bool
isDescending xs = and (zipWith (>=) xs (drop 1 xs))

-- | Queries whose ordered results are snapshotted into the golden file.
-- Add a query here to start tracking it, then regenerate with @--accept@.
tuningQueries :: [Text]
tuningQueries = ["", "haskell", "config", "fuzzy", "api", "nix", "zzzznomatch"]

-- | A stable, human-readable snapshot of "query -> ordered (id, title)" over
-- the tuning fixture, for the golden test. Tune the fixture or the matcher,
-- run @cabal test --test-options=--accept@, and eyeball the golden diff.
renderTuning :: String
renderTuning = intercalate "\n\n" (map renderQuery tuningQueries) <> "\n"
  where
    renderQuery q =
      "query: "
        <> show q
        <> "\n"
        <> case runTuning q of
          [] -> "  (no matches)"
          items -> intercalate "\n" (map renderItem items)
    renderItem bi =
      "  " <> T.unpack (unId (_biId bi)) <> "  " <> T.unpack (_biTitle bi)
    unId (BookmarkItemId t) = t

filterTuningTests :: TestTree
filterTuningTests =
  testGroup
    "Live filter tuning fixture"
    [ -- Golden snapshot of matching + ordering. Regenerate after tuning with:
      --   cabal test --test-options=--accept
      goldenVsString
        "ordered filter results per query"
        "test/golden/tuning-filter.golden"
        (pure (LBS.pack renderTuning)),
      -- Render invariant: whatever matches, the result is always date-desc.
      testCase "survivors are always in descending created order" $
        isDescending (map _biCreated (runTuning "e")) @?= True
    ]

filterStateTests :: TestTree
filterStateTests =
  testGroup
    "Live filter state"
    [ testCase "bookmarkSearchText includes title" $
        fuzzyMatch "uniquetitle" (bookmarkSearchText filterTestBookmark) @?= True,
      testCase "bookmarkSearchText includes domain" $
        fuzzyMatch "uniquedomain" (bookmarkSearchText filterTestBookmark) @?= True,
      testCase "bookmarkSearchText includes excerpt" $
        fuzzyMatch "uniqueexcerpt" (bookmarkSearchText filterTestBookmark) @?= True,
      testCase "bookmarkSearchText includes note" $
        fuzzyMatch "uniquenote" (bookmarkSearchText filterTestBookmark) @?= True,
      testCase "bookmarkSearchText excludes tags" $
        fuzzyMatch "zzqtag" (bookmarkSearchText filterTestBookmark) @?= False,
      testCase "enterFilterMode sets mFilterActive" $
        view mFilterActive (enterFilterMode testState) @?= True,
      testCase "typing after a set query appends at the end" $
        editorQuery (typeInFilter "x" (reconcile (testState & mFilterQuery .~ "ab") initialUi)) @?= "abx",
      testCase "ctrl-w deletes the previous word" $
        editorQuery (reconcile (testState & mFilterQuery .~ "foo bar") initialUi & uiEditor %~ applyEdit deletePrevWord) @?= "foo ",
      testCase "the editor is written only when it differs from the query" $ do
        -- Mid-typing the two agree, so the cursor stays off the end, where
        -- the keys put it.
        let typing = typeInFilter "abc" (reconcile testState initialUi) & uiEditor %~ applyEdit (Z.moveLeft . Z.moveLeft)
            typed = testState & mFilterQuery .~ "abc"
            cursor = view (uiEditor . editContentsL . to Z.cursorPosition)
        (editorQuery (reconcile typed typing), cursor (reconcile typed typing)) @?= ("abc", (0, 1))
        let written = reconcile (typed & mFilterQuery .~ "agent") typing
        (editorQuery written, cursor written) @?= ("agent", (0, 5)),
      testCase "reconcile shows what is visible, with the selection on mSelected" $ do
        let model = settle (insertItems [filterTestBookmark, filterOtherBookmark] testState) & mSelected ?~ BookmarkItemId "filter-other"
            shown = reconcile model initialUi
        V.toList (V.map _biId (shown ^. uiList . L.listElementsL)) @?= V.toList (V.map _biId (model ^. mVisible))
        fmap (_biId . snd) (L.listSelectedElement (shown ^. uiList)) @?= Just (BookmarkItemId "filter-other"),
      testCase "cancelFilter clears query and exits editing" $
        let s = cancelFilter (enterFilterMode testState & mFilterQuery .~ "x")
         in (view mFilterActive s, view mFilterQuery s) @?= (False, ""),
      testCase "lockFilter exits editing but preserves query" $
        let s = lockFilter (enterFilterMode testState & mFilterQuery .~ "x")
         in (view mFilterActive s, view mFilterQuery s) @?= (False, "x"),
      testCase "settle applies fuzzy text filter" $
        let base = insertItems [filterTestBookmark, filterOtherBookmark] testState
            filtered = settle (base & mFilterQuery .~ "uniquetitle")
            elems = view mVisible filtered
         in V.toList (V.map _biId elems) @?= [BookmarkItemId "filter-1"],
      testCase "text filter composes with video filter" $
        let base = insertItems [filterTestBookmark, filterVideoBookmark] testState
            filtered =
              settle
                ( base
                    & mVideoFilter .~ ShowOnlyVideos
                    & mFilterQuery .~ "unique"
                )
            elems = view mVisible filtered
         in V.toList (V.map _biId elems) @?= [BookmarkItemId "filter-video"]
    ]
