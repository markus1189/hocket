{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import AgentServer (AgentEnv (..), runAgentServer)
import Brick.BChan (BChan, newBChan, readBChan, writeBChan)
import qualified Brick.Widgets.List as L
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO)
import Control.Exception (IOException, try)
import Control.Monad (void)
import Control.Lens (ix, view, _1, _2)
import Control.Lens.Operators
import qualified Data.Aeson as A
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either (isLeft)
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (for_)
import Data.List (find, head, intercalate, isInfixOf, last)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (DiffTime, UTCTime (..))
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.Vector as V
import Events
import Network.Bookmark.Agent.Protocol
import Network.Bookmark.Agent.Snapshot
import Network.Bookmark.Types
import Network.Bookmark.Ui.State
import Network.Bookmark.Ui.Widgets (fuzzyFilterMatch, fuzzyMatch, sanitizeForDisplay)
import Network.Socket
  ( Family (AF_UNIX),
    SockAddr (SockAddrUnix),
    Socket,
    SocketType (Stream),
    close,
    connect,
    defaultProtocol,
    socket,
    socketToHandle,
  )
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import System.IO
  ( BufferMode (LineBuffering),
    Handle,
    IOMode (ReadWriteMode),
    hFlush,
    hSetBuffering,
  )
import System.Posix.Process (getProcessID)
import System.Posix.Time (epochTime)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [xKeyBatchConcurrencyTests, hocketStateTests, raindropParsingTests, dateTimeParsingTests, jsonRoundtripTests, sanitizeForDisplayTests, fuzzyMatchTests, fuzzyFilterMatchTests, filterStateTests, filterTuningTests, agentSnapshotTests, agentProtocolTests, agentServerIntegrationTests]

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

-- Fixtures for the 'X'-batch regression test: three items, one per bucket.
batchReminderTime :: UTCTime
batchReminderTime = read "2016-05-23 07:00:00 UTC"

batchArmArchive :: BookmarkItem
batchArmArchive = bookmarkItem1 {_biId = BookmarkItemId "bx1"}

batchArmSetRem :: BookmarkItem
batchArmSetRem = bookmarkItem1 {_biId = BookmarkItemId "bx2", _biReminder = Nothing}

batchArmRmvRem :: BookmarkItem
batchArmRmvRem = bookmarkItem1 {_biId = BookmarkItemId "bx3", _biReminder = Just (read "2016-05-22 12:33:11 UTC")}

-- State with pending work in all three 'X' buckets at once.
armedBatchState :: HocketState
armedBatchState =
  insertItems [batchArmArchive, batchArmSetRem, batchArmRmvRem] testState
    & togglePendingAction (_biId batchArmArchive)
    & togglePendingActionToReminder (_biId batchArmSetRem) batchReminderTime
    & togglePendingActionToReminder (_biId batchArmRmvRem) batchReminderTime

-- One 'X' press must cover every pending bucket via a single slot-owning batch,
-- and a second press/fetch is refused while it runs. These assertions fail if
-- the three ops are ever uncoordinated again or a bucket is silently dropped.
xKeyBatchConcurrencyTests :: TestTree
xKeyBatchConcurrencyTests =
  testGroup
    "X-key batch (regression)"
    [ testCase "one X press plans every non-empty bucket" $ do
        batchStepsWithWork testState @?= []
        batchStepsWithWork justArchive @?= [StepArchive]
        batchStepsWithWork justSetRem @?= [StepSetReminders]
        batchStepsWithWork justRmvRem @?= [StepRemoveReminders]
        batchStepsWithWork armedBatchState @?= [StepArchive, StepSetReminders, StepRemoveReminders],
      testCase "batch holds the single slot; a double X is refused, completion frees it" $ do
        asyncOpRunning armedBatchState @?= False
        let Just fired = tryAcquireAsyncOp OpExecuteBatch armedBatchState
        asyncOpRunning fired @?= True
        ( case tryAcquireAsyncOp OpExecuteBatch fired of
            Nothing -> pure ()
            Just _ -> assertFailure "a second X started while a batch owns the slot"
          )
        ( case tryAcquireAsyncOp OpFetchItems fired of
            Nothing -> pure ()
            Just _ -> assertFailure "fetch started while the batch owns the slot"
          )
        let done = completeAsyncOp fired
        asyncOpRunning done @?= False
        case tryAcquireAsyncOp OpExecuteBatch done of
          Just inFlight -> asyncOpRunning inFlight @?= True
          Nothing -> assertFailure "batch refused after slot freed"
    ]
  where
    justArchive = insertItems [batchArmArchive] testState & togglePendingAction (_biId batchArmArchive)
    justSetRem = insertItems [batchArmSetRem] testState & togglePendingActionToReminder (_biId batchArmSetRem) batchReminderTime
    justRmvRem = insertItems [batchArmRmvRem] testState & togglePendingActionToReminder (_biId batchArmRmvRem) batchReminderTime

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
agentSnapAB = takeSnapshot 1 (syncForRender (insertItems [agentItemA, agentItemB] testState))

agentSnapFlagged :: AgentSnapshot
agentSnapFlagged =
  takeSnapshot 1 $
    syncForRender $
      setPendingAction (BookmarkItemId "ag1") ToBeArchived $
        insertItems [agentItemA, agentItemB] testState

agentSnapWithRem :: AgentSnapshot
agentSnapWithRem = takeSnapshot 1 (syncForRender (insertItems [agentItemA, agentItemRem] testState))

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
        fst <$> Map.lookup (BookmarkItemId "ag1") (s ^. hsContents) @?= Just ToBeArchived,
      testCase "setFilterQuery replaces the query and clears filter-input mode" $ do
        let s = setFilterQuery "haskell" (enterFilterMode testState)
        s ^. hsFilterQuery @?= "haskell"
        s ^. hsFilterActive @?= False,
      testCase "setVideoFilterMode is a plain assignment" $ do
        let s = setVideoFilterMode ShowOnlyVideos testState
        s ^. hsVideoFilter @?= ShowOnlyVideos,
      testCase "setShowFutureReminders is a plain assignment" $ do
        let s = setShowFutureReminders True testState
        s ^. hsShowFutureReminders @?= True,
      testCase "setAgentClients sets the header count" $ do
        let s = setAgentClients 2 testState
        s ^. hsAgentClients @?= 2,
      testCase "credentials never appear in the encoded snapshot" $
        let s =
              syncForRender
                ( insertItems
                    [agentItemA]
                    (initialState (BookmarkCredentials (RaindropToken "super-secret-token") 0))
                )
            encoded = LBS.unpack (A.encode (takeSnapshot 1 s))
         in assertBool
              "raindrop token must not appear in snapshot JSON"
              (not ("super-secret-token" `isInfixOf` encoded))
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
          (isLeft (validateWrite agentSnapWithRem (CmdSelectItem (BookmarkItemId "ag3"))))
    ]

-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Agent control socket: end-to-end over a real Unix-domain socket. These
-- exercise the IO shell (AgentServer.hs) that the pure protocol tests cannot
-- reach: real AF_UNIX connects, reads answered from the snapshot TVar,
-- validated write-injection into the event BChan, and wait_version
-- long-polling that must unblock when the snapshot version advances.
-- ---------------------------------------------------------------------------

-- Small helper: a BookmarkItem with a stable id/title.
agentSockItem :: Text -> Text -> BookmarkItem
agentSockItem i t =
  bookmarkItem1
    { _biId = BookmarkItemId i,
      _biTitle = t,
      _biCreated = read "2020-01-01 10:00:00 UTC",
      _biLastUpdate = read "2020-01-01 10:00:00 UTC"
    }

-- | Build the (pure) snapshot projection the server will serve, given a
-- version. Mirrors what appHandleEvent does each frame.
buildTestSnapshot :: Int -> AgentSnapshot
buildTestSnapshot v =
  takeSnapshot v $
    syncForRender $
      insertItems
        [ agentSockItem "in-1" "first bookmark",
          agentSockItem "in-2" "second bookmark"
        ]
        testState

-- | Connect a client socket to the server; returns the IO handle.
connectAgentClient :: FilePath -> IO Handle
connectAgentClient path = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock (SockAddrUnix path)
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h LineBuffering
  pure h

-- | Send one newline-delimited request and read the newline-delimited reply.
sendRequest :: Handle -> BSC.ByteString -> IO A.Value
sendRequest h line = do
  LBS.hPutStr h (LBS.fromStrict line <> "\n")
  hFlush h
  BSC.hGetLine h >>= \resp ->
    case A.eitherDecodeStrict resp of
      Left e -> assertFailure ("bad response json: " ++ show e)
      Right v -> pure v

-- | Observe the next, non-announcement UiCommand injected into the event
-- channel (blocking). Skips SetAgentClients connect/disconnect bookkeeping.
readAgentInjected :: BChan HocketEvent -> IO UiCommand
readAgentInjected ch = do
  ev <- readBChan ch
  case ev of
    HocketUi (SetAgentClients _) -> readAgentInjected ch
    HocketUi u -> pure u
    _ -> readAgentInjected ch

-- | Poll-connect to a socket until a listener accepts (server fully bound),
-- retrying a bounded number of times with a short sleep. Guards against the
-- race where a test launches a server in the background and immediately
-- probes it before the server has actually bound its path.
waitUntilReachable :: Int -> FilePath -> IO ()
waitUntilReachable attempts path
  | attempts <= 0 = assertFailure ("server never became reachable at " ++ path)
  | otherwise =
      reachable path >>= \ok ->
        if ok
          then pure ()
          else threadDelay 20000 >> waitUntilReachable (attempts - 1) path
  where
    reachable :: FilePath -> IO Bool
    reachable p = do
      sock <- socket AF_UNIX Stream defaultProtocol
      r <- try @IOException (connect sock (SockAddrUnix p) >> socketToHandle sock ReadWriteMode >> pure ())
      close sock
      pure (not (isLeft r))

-- | Run the agent server on a throwaway socket with a snapshot TVar we own,
-- handing both that TVar and the event BChan to the action so the test can
-- bump versions and observe injected writes.
withAgentServer ::
  (TVar AgentSnapshot -> BChan HocketEvent -> FilePath -> IO a) ->
  IO a
withAgentServer action = do
  tmp <- getTemporaryDirectory
  e <- epochTime
  pid <- getProcessID
  let path = tmp </> ("hocket-test-" ++ show pid ++ "-" ++ show e ++ ".sock")
  snapVar <- newTVarIO emptySnapshot
  atomically (modifyTVar' snapVar (const (buildTestSnapshot 1)))
  events <- newBChan 10
  let env =
        AgentEnv
          { aeSnapshot = snapVar,
            aeInject = \evt -> do
              void (writeBChan events evt)
              pure True,
            aeReminderTime = pure (read "2020-01-01 07:00:00 UTC" :: UTCTime)
          }
  withAsync (runAgentServer path env) $ \_ -> action snapVar events path

-- | Drain (up to n) injected events from the BChan, returning the UiCommands.
drainUI :: BChan HocketEvent -> Int -> IO [UiCommand]
drainUI ch n = go n []
  where
    go 0 acc = pure (reverse acc)
    go k acc = do
      ev <- readBChan ch
      case ev of
        HocketUi u -> go (k - 1) (u : acc)
        _ -> go (k - 1) acc

agentServerIntegrationTests :: TestTree
agentServerIntegrationTests =
  testGroup
    "Agent server (socket)"
    [ testCase "get_state returns the snapshot view with a version" $
        withAgentServer $ \snapVar events path -> do
          -- seed the snapshot the test owns; the server reads from the same TVar
          atomically (modifyTVar' snapVar (const (buildTestSnapshot 7)))
          h <- connectAgentClient path
          resp <- sendRequest h "{\"id\":1,\"method\":\"get_state\"}"
          case resp of
            A.Object o ->
              case KM.lookup (AKey.fromText "result") o of
                Just (A.Object r) -> do
                  KM.lookup (AKey.fromText "version") r @?= Just (A.Number 7)
                  -- counts subobject reflects the two seeded items
                  case KM.lookup (AKey.fromText "counts") r of
                    Just (A.Object c) ->
                      KM.lookup (AKey.fromText "visible") c @?= Just (A.Number 2)
                    _ -> assertFailure "no counts object"
                _ -> assertFailure "no result object"
            _ -> assertFailure "response not an object",
      testCase "list_items returns the snapshot items" $
        withAgentServer $ \snapVar events path -> do
          atomically (modifyTVar' snapVar (const (buildTestSnapshot 3)))
          h <- connectAgentClient path
          resp <- sendRequest h "{\"id\":2,\"method\":\"list_items\"}"
          case resp of
            A.Object o ->
              case KM.lookup (AKey.fromText "result") o of
                Just (A.Array items) -> length items @?= 2
                _ -> assertFailure "list_items result is not an array"
            _ -> assertFailure "response not an object",
      testCase "a validated write is injected into the event BChan" $
        withAgentServer $ \snapVar events path -> do
          atomically (modifyTVar' snapVar (const (buildTestSnapshot 9)))
          h <- connectAgentClient path
          -- set_filter is a validated write that translates to SetFilterQuery.
          resp <-
            sendRequest
              h
              "{\"id\":4,\"method\":\"set_filter\",\"params\":{\"query\":\"haskell\"}}"
          -- response should be ok, result carrying "injected":true
          case resp of
            A.Object o -> do
              KM.lookup (AKey.fromText "ok") o @?= Just (A.Bool True)
              case KM.lookup (AKey.fromText "result") o of
                Just (A.Object r) ->
                  KM.lookup (AKey.fromText "injected") r @?= Just (A.Bool True)
                _ -> assertFailure "set_filter result missing"
            _ -> assertFailure "set_filter response not an object"
          -- The injected event must be a SetFilterQuery "haskell".
          injected <- readAgentInjected events
          injected @?= SetFilterQuery "haskell",
      testCase "wait_version unblocks once the snapshot version advances" $
        withAgentServer $ \snapVar events path -> do
          atomically (modifyTVar' snapVar (const (buildTestSnapshot 4)))
          h <- connectAgentClient path
          -- Bump the version on a delay so the client has to genuinely wait.
          _ <- forkIO $ do
            threadDelay 300000
            atomically (modifyTVar' snapVar (const (buildTestSnapshot 6)))
          resp <-
            sendRequest
              h
              "{\"id\":3,\"method\":\"wait_version\",\"params\":{\"after\":4,\"timeout_ms\":5000}}"
          case resp of
            A.Object o ->
              case KM.lookup (AKey.fromText "result") o of
                Just (A.Object r) ->
                  KM.lookup (AKey.fromText "version") r @?= Just (A.Number 6)
                _ -> assertFailure "wait_version result missing"
            _ -> assertFailure "wait_version response not object",
      testCase "a second server on the same path fails instead of hijacking" $
        withAgentServer $ \snapVar events path -> do
          -- First server is live on `path`. Attempt to start a second one on
          -- the same path; the probe-then-reclaim must NOT unlink the live
          -- socket, and the second bind must fail with a genuine error
          -- rather than stealing ownership.
          snapVar2 <- newTVarIO emptySnapshot
          events2 <- newBChan 10
          let env2 =
                AgentEnv
                  { aeSnapshot = snapVar2,
                    aeInject = \evt -> do
                      void (writeBChan events2 evt)
                      pure True,
                    aeReminderTime = pure (read "2020-01-01 07:00:00 UTC" :: UTCTime)
                  }
          -- Ensure server 1 has actually bound the path before we probe, so
          -- server 2's bind can genuinely be rejected rather than racing
          -- ahead and winning the path (which would hang us in accept).
          waitUntilReachable 50 path
          result <- try @IOException (runAgentServer path env2)
          assertBool
            "expected the second server's bind to fail (address already in use)"
            (isLeft result)
          -- The original server must still be reachable after the attempt.
          h <- connectAgentClient path
          resp <- sendRequest h "{\"id\":99,\"method\":\"get_state\"}"
          case resp of
            A.Object o ->
              KM.lookup (AKey.fromText "ok") o @?= Just (A.Bool True)
            _ -> assertFailure "original server unreachable after collision attempt"
    ]
