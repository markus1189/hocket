{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Brick.Widgets.List as L
import Control.Lens (ix, view, _1, _2)
import Control.Lens.Operators
import qualified Data.Aeson as A
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (find, head, intercalate, last)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (DiffTime, UTCTime (..))
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.Vector as V
import Network.Bookmark.Types
import Network.Bookmark.Ui.State
import Network.Bookmark.Ui.Widgets (fuzzyFilterMatch, fuzzyMatch, sanitizeForDisplay)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [hocketStateTests, raindropParsingTests, dateTimeParsingTests, jsonRoundtripTests, sanitizeForDisplayTests, fuzzyMatchTests, fuzzyFilterMatchTests, filterStateTests, filterTuningTests]

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

bookmarkItem1 :: BookmarkItem
bookmarkItem1 =
  BookmarkItem
    (BookmarkItemId "1")
    "www.url.com"
    "excerpt"
    "note"
    "link"
    []
    False
    (read "2016-05-22 12:33:11 UTC")
    (read "2016-05-22 12:33:11 UTC")
    "url.com"
    "given title"
    0
    []
    0
    False
    Nothing

bookmarkItem2 :: BookmarkItem
bookmarkItem2 = bookmarkItem1 {_biLastUpdate = read "2016-05-22 12:54:59 UTC", _biTitle = "newer title"}

bookmarkItem1_same_ts_diff_title :: BookmarkItem
bookmarkItem1_same_ts_diff_title = bookmarkItem1 {_biTitle = "title for same ts item"}

testState = initialState (BookmarkCredentials (RaindropToken "") 0)

hocketStateTests =
  testGroup
    "HocketState insertItem/insertItems"
    [ testCase "insertItem: new item gets None status and correct data" $
        let s = insertItem bookmarkItem1 testState
         in Map.lookup (_biId bookmarkItem1) (view hsContents s) @?= Just (None, bookmarkItem1),
      testCase "insertItem: updates with newer item" $
        let stateWithOldItem = insertItem bookmarkItem1 testState
            stateWithOldItemPending = stateWithOldItem & hsContents . ix (_biId bookmarkItem1) . _1 .~ ToBeArchived
            stateAfterUpdate = insertItem bookmarkItem2 stateWithOldItemPending
         in Map.lookup (_biId bookmarkItem1) (view hsContents stateAfterUpdate) @?= Just (ToBeArchived, bookmarkItem2),
      testCase "insertItem: older item does not overwrite newer; status and data preserved" $
        let stateWithNewerItem = insertItem bookmarkItem2 testState
            stateWithNewerItemPending = stateWithNewerItem & hsContents . ix (_biId bookmarkItem2) . _1 .~ ToBeArchived
            stateAfterAttemptedUpdate = insertItem bookmarkItem1 stateWithNewerItemPending
         in Map.lookup (_biId bookmarkItem2) (view hsContents stateAfterAttemptedUpdate) @?= Just (ToBeArchived, bookmarkItem2),
      testCase "insertItem: item with same timestamp does overwrite; status preserved, data updated" $
        let stateWithOriginalItem = insertItem bookmarkItem1 testState
            stateWithOriginalItemPending = stateWithOriginalItem & hsContents . ix (_biId bookmarkItem1) . _1 .~ ToBeArchived
            stateAfterAttemptedUpdate = insertItem bookmarkItem1_same_ts_diff_title stateWithOriginalItemPending
         in Map.lookup (_biId bookmarkItem1) (view hsContents stateAfterAttemptedUpdate) @?= Just (ToBeArchived, bookmarkItem1_same_ts_diff_title),
      testCase "insertItems: inserting an item that is present overwrites if newer" $
        let s = insertItems [bookmarkItem1, bookmarkItem2] testState
         in fmap (view (_2 . biTitle)) (Map.lookup (_biId bookmarkItem1) (view hsContents s)) @?= Just "newer title",
      testCase "insertItems: inserting an item that is present overwrites if newer, insertion order does not matter" $
        let s = insertItems [bookmarkItem2, bookmarkItem1] testState
         in fmap (view (_2 . biTitle)) (Map.lookup (_biId bookmarkItem1) (view hsContents s)) @?= Just "newer title",
      testCase "insertItems: basic insertion count" $
        length (insertItems [bookmarkItem1] testState ^. hsContents) @?= 1
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
-- 'syncForRender' sorts descending by 'getSortDate' (reminder if present, else
-- '_biCreated'), the default render order is newest-first: t10, t09, .. t01.
--
-- All reminders are Nothing, so every item is visible under the default state
-- and sorts purely by '_biCreated'. Tuning hooks:
--   * Matching: edit the title/domain/excerpt/note fields or the query. The
--     match target is title+domain+excerpt+note (tags are excluded).
--   * Ordering: edit '_biCreated'. To make an item sort by a reminder instead,
--     set '_biReminder' AND run with @hsShowFutureReminders .~ True@, otherwise
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
      filtered = syncForRender (base & hsFilterQuery .~ q)
   in V.toList (view (itemList . L.listElementsL) filtered)

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
      testCase "enterFilterMode sets hsFilterActive" $
        view hsFilterActive (enterFilterMode testState) @?= True,
      testCase "backspace undoes appendFilterChar" $
        view hsFilterQuery (backspaceFilter (appendFilterChar 'x' testState))
          @?= view hsFilterQuery testState,
      testCase "cancelFilter clears query and exits editing" $
        let s = cancelFilter (appendFilterChar 'x' (enterFilterMode testState))
         in (view hsFilterActive s, view hsFilterQuery s) @?= (False, ""),
      testCase "lockFilter exits editing but preserves query" $
        let s = lockFilter (appendFilterChar 'x' (enterFilterMode testState))
         in (view hsFilterActive s, view hsFilterQuery s) @?= (False, "x"),
      testCase "syncForRender applies fuzzy text filter" $
        let base = insertItems [filterTestBookmark, filterOtherBookmark] testState
            filtered = syncForRender (base & hsFilterQuery .~ "uniquetitle")
            elems = view (itemList . L.listElementsL) filtered
         in V.toList (V.map _biId elems) @?= [BookmarkItemId "filter-1"],
      testCase "text filter composes with video filter" $
        let base = insertItems [filterTestBookmark, filterVideoBookmark] testState
            filtered =
              syncForRender
                ( base
                    & hsVideoFilter .~ ShowOnlyVideos
                    & hsFilterQuery .~ "unique"
                )
            elems = view (itemList . L.listElementsL) filtered
         in V.toList (V.map _biId elems) @?= [BookmarkItemId "filter-video"]
    ]

raindropParsingTests :: TestTree
raindropParsingTests =
  testGroup
    "Raindrop JSON Parsing"
    [ testCase "parsing list of BookmarkItems from JSON" $ do
        jsonLBS <- LBS.readFile "test/raindrop-items1.json"
        let decodedValue = A.eitherDecode jsonLBS :: Either String A.Value

        case decodedValue of
          Left err -> assertFailure $ "JSON decoding failed: " ++ err
          Right val ->
            case parseMaybe (A.withObject "ApiResponse" (A..: "items")) val of
              Nothing -> assertFailure "Could not extract 'items' array or parse it into [BookmarkItem]"
              Just (items :: [BookmarkItem]) -> do
                assertEqual "Number of parsed items" 18 (length items)
                assertEqual "First item ID" (BookmarkItemId "1085603154") (_biId $ head items)
                assertEqual "Last item ID" (BookmarkItemId "1070362199") (_biId $ last items)

                -- Verify a specific item's title (Google item)
                let googleItemId = BookmarkItemId "1077326683"
                let maybeGoogleItem = find (\item -> _biId item == googleItemId) items
                case maybeGoogleItem of
                  Nothing -> assertFailure $ "Item with ID " ++ show googleItemId ++ " not found."
                  Just googleItem -> assertEqual "Google item title" ("Google" :: Text) (_biTitle googleItem)

                -- Verify another specific item's excerpt (Stiftung Warentest item)
                let stiftungTestItemId = BookmarkItemId "1085443954"
                let maybeStiftungTestItem = find (\item -> _biId item == stiftungTestItemId) items
                case maybeStiftungTestItem of
                  Nothing -> assertFailure $ "Item with ID " ++ show stiftungTestItemId ++ " not found."
                  Just stiftungTestItem -> assertEqual "Stiftung Warentest item excerpt" ("Stiftung Warentest: Testberichte zu Elektronik, Haushalt und Gesundheit sowie Finanzen, Versicherung und Steuern" :: Text) (_biExcerpt stiftungTestItem)

                -- Verify specific item has reminder date
                let reminderItemId = BookmarkItemId "1076430230"
                let maybeReminderItem = find (\item -> _biId item == reminderItemId) items
                case maybeReminderItem of
                  Nothing -> assertFailure $ "Item with ID " ++ show reminderItemId ++ " not found."
                  Just reminderItem -> do
                    case _biReminder reminderItem of
                      Nothing -> assertFailure "Expected item to have a reminder, but found Nothing"
                      Just reminderTime -> assertEqual "Reminder date should match expected value" (read "2025-07-11 14:48:19.217 UTC") reminderTime
    ]

dateTimeParsingTests :: TestTree
dateTimeParsingTests = testGroup "Date/Time Parsing" [testParseUTCTimeString]

testParseUTCTimeString :: TestTree
testParseUTCTimeString = testCase "parsing ISO8601 UTCTime string '2025-05-25T15:47:27.230Z'" $ do
  let timeString = "2025-05-25T15:47:27.230Z"
  let mParsedTime = iso8601ParseM timeString :: Maybe UTCTime

  case mParsedTime of
    Nothing -> assertFailure $ "Failed to parse ZonedTime string: " ++ timeString
    Just (UTCTime day diffTime) -> do
      let (year, month, dayOfMonth) = toGregorian day
      assertEqual "Year component" 2025 year
      assertEqual "Month component" 5 month
      assertEqual "Day component" 25 dayOfMonth

      let expectedDiffTimeAsRational = 56847230 % 1000
      assertEqual "Time component (DiffTime as Rational)" expectedDiffTimeAsRational (toRational diffTime)

jsonRoundtripTests :: TestTree
jsonRoundtripTests =
  testGroup
    "JSON Roundtrip"
    [ testBookmarkItemRoundtrip,
      testBookmarkItemEdgeCases
    ]

testBookmarkItemRoundtrip :: TestTree
testBookmarkItemRoundtrip = testCase "BookmarkItem JSON encode/decode roundtrip" $ do
  let original = bookmarkItem1
  let encoded = A.encode original
  let decoded = A.eitherDecode encoded :: Either String BookmarkItem

  case decoded of
    Left err -> assertFailure $ "JSON roundtrip failed: " ++ err
    Right item -> assertEqual "Roundtrip should preserve original item" original item

testBookmarkItemEdgeCases :: TestTree
testBookmarkItemEdgeCases = testCase "BookmarkItem edge cases" $ do
  let itemWithEmptyFields =
        BookmarkItem
          (BookmarkItemId "999")
          "" -- empty link
          "" -- empty excerpt
          "" -- empty note
          "link"
          [] -- empty tags
          True
          (read "2016-05-22 12:33:11 UTC")
          (read "2016-05-22 12:54:59 UTC")
          "" -- empty domain
          "" -- empty title
          0
          [] -- empty highlights
          0
          False
          Nothing -- no reminder
  let encoded = A.encode itemWithEmptyFields
  let decoded = A.eitherDecode encoded :: Either String BookmarkItem

  case decoded of
    Left err -> assertFailure $ "Edge case roundtrip failed: " ++ err
    Right item -> assertEqual "Empty fields should roundtrip correctly" itemWithEmptyFields item
