{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import BatchTests (batchTests)
import Brick.BChan (BChan, newBChan, readBChan, writeBChan)
import Brick.Widgets.Edit (applyEdit, editContentsL)
import qualified Brick.Widgets.List as L
import CharacterizationTests (characterizationTests)
import ClassifyTests (classifyTests)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, cancel, poll, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO)
import Control.Exception (IOException, SomeException, displayException, finally, throwIO, try)
import Control.Lens (ix, to, view, _1, _2)
import Control.Lens.Operators
import Control.Monad (void)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, head, intercalate, isInfixOf, last)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import Data.Text.Zipper.Generic.Words (deletePrevWord)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (DiffTime, UTCTime (..))
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time.LocalTime (utc)
import qualified Data.Vector as V
import FakeTests (fakeTests)
import FixtureTests (fixtureTests)
import HeadlessTests (headlessTests)
import Hocket.Adapter.Raindrop.Api (ApiResult (..), BatchMove (..), Created (..), Items (..), NewRaindrop (..), RaindropPatch (..))
import Hocket.Agent.Protocol
import Hocket.Agent.Snapshot
import Hocket.App.Model
import qualified Hocket.App.Msg as I
import Hocket.Domain.Bookmark
import Hocket.Domain.Filter (fuzzyFilterMatch, fuzzyMatch, sanitizeForDisplay)
import Hocket.Shell.Agent.Client (callAgent, isChannelFullError)
import Hocket.Shell.Agent.Server (AgentEnv (..), BoundSocket, boundSocketId, cleanupAgentSocket, newBoundSocket, runAgentServer, socketDirIsOurs)
import Hocket.Shell.Signals (Terminated (..), exitOnTerminated, unwindOnTermination)
import Hocket.Shell.Tui.Reconcile
import IntentTests (intentTests)
import KeyIntentTests (keyIntentTests)
import NavTests (navTests)
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
import PlanTests (planTests)
import Properties (propertyTests)
import ReminderTests (reminderTests)
import SMFixtureTests (smFixtureTests)
import SnapshotGoldenTests (snapshotGoldenTests)
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getTemporaryDirectory, pathIsSymbolicLink, removeDirectoryRecursive, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO
  ( BufferMode (LineBuffering),
    Handle,
    IOMode (ReadWriteMode),
    hClose,
    hFlush,
    hSetBuffering,
  )
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (createSymbolicLink, deviceID, fileID, fileMode, getFileStatus, getSymbolicLinkStatus, isSocket, setFileMode)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Handler (Default), installHandler, raiseSignal, sigHUP, sigTERM)
import System.Posix.Time (epochTime)
import System.Posix.Types (DeviceID, FileID)
import System.Timeout (timeout)
import SystemTests (systemTests)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import UpdateTests (updateTests)
import VisibleTests (visibleTests)
import WorkerTests (workerTests)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [smFixtureTests, snapshotGoldenTests, xKeyBatchConcurrencyTests, propertyTests, visibleTests, navTests, intentTests, keyIntentTests, planTests, systemTests, fakeTests, fixtureTests, reminderTests, hocketStateTests, raindropParsingTests, raindropApiBodyTests, classifyTests, characterizationTests, updateTests, workerTests, headlessTests, batchTests, dateTimeParsingTests, jsonRoundtripTests, sanitizeForDisplayTests, fuzzyMatchTests, fuzzyFilterMatchTests, filterStateTests, filterTuningTests, agentSnapshotTests, agentProtocolTests, agentServerIntegrationTests]

-- Expected bodies are what the pre-servant wreq client sent.
raindropApiBodyTests :: TestTree
raindropApiBodyTests =
  testGroup
    "Raindrop API bodies"
    [ testCase "move to collection" $
        A.toJSON (MoveTo 7) @?= A.object ["collection" A..= A.object ["$id" A..= (7 :: Int)]],
      testCase "set reminder keeps millisecond Z format" $
        A.toJSON (SetReminderAt (UTCTime (read "2026-10-06") 18000))
          @?= A.object ["reminder" A..= A.object ["date" A..= ("2026-10-06T05:00:00.000Z" :: Text)]],
      testCase "clear reminder sends null" $
        A.toJSON ClearReminder @?= A.object ["reminder" A..= A.Null],
      testCase "batch move keeps ids as strings" $
        A.toJSON (BatchMove ["1", "2"] 7)
          @?= A.object ["ids" A..= (["1", "2"] :: [Text]), "collection" A..= A.object ["$id" A..= (7 :: Int)]],
      testCase "new raindrop" $
        A.toJSON (NewRaindrop "https://example.com" "-1" ["t"])
          @?= A.object ["link" A..= ("https://example.com" :: Text), "collection" A..= ("-1" :: Text), "tags" A..= (["t"] :: [Text]), "pleaseParse" A..= A.object []],
      testCase "created id is decoded" $
        (\(Created i) -> i) <$> A.decode "{\"result\":true,\"item\":{\"_id\":123}}" @?= Just 123,
      testCase "result false is a value, missing result is a decode failure" $ do
        (\(ApiResult b) -> b) <$> A.decode "{\"result\":false}" @?= Just False
        isLeft (A.eitherDecode @ApiResult "{}") @? "expected decode failure",
      testCase "highlight objects decode to their text" $
        view biHighlights <$> parseMaybe A.parseJSON (itemWithHighlights [A.object ["text" A..= ("quoted" :: Text), "color" A..= ("yellow" :: Text)]])
          @?= Just ["quoted"],
      testCase "list response decodes count and items" $ do
        jsonLBS <- LBS.readFile "test/raindrop-items1.json"
        case A.eitherDecode jsonLBS of
          Left e -> assertFailure e
          Right (Items count items) -> (count, length items) @?= (18, 18)
    ]

-- | The first item of the list fixture, with its highlights replaced.
itemWithHighlights :: [A.Value] -> A.Value
itemWithHighlights hs = case A.decode @A.Value (LBS.pack fixtureItem) of
  Just (A.Object o) -> A.Object (KM.insert "highlights" (A.toJSON hs) o)
  _ -> A.Null
  where
    fixtureItem = "{\"_id\":1,\"link\":\"https://example.com\",\"excerpt\":\"\",\"note\":\"\",\"type\":\"link\",\"tags\":[],\"removed\":false,\"created\":\"2026-01-01T00:00:00.000Z\",\"lastUpdate\":\"2026-01-01T00:00:00.000Z\",\"domain\":\"example.com\",\"title\":\"t\",\"sort\":0,\"collection\":{\"$id\":-1},\"important\":false}"

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

testState = initialModel

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
armedBatchState :: Model
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
    "Model insertItem/insertItems"
    [ testCase "insertItem: new item gets None status and correct data" $
        let s = insertItem bookmarkItem1 testState
         in Map.lookup (_biId bookmarkItem1) (view mContents s) @?= Just (None, bookmarkItem1),
      testCase "insertItem: updates with newer item" $
        let stateWithOldItem = insertItem bookmarkItem1 testState
            stateWithOldItemPending = stateWithOldItem & mContents . ix (_biId bookmarkItem1) . _1 .~ ToBeArchived
            stateAfterUpdate = insertItem bookmarkItem2 stateWithOldItemPending
         in Map.lookup (_biId bookmarkItem1) (view mContents stateAfterUpdate) @?= Just (ToBeArchived, bookmarkItem2),
      testCase "insertItem: older item does not overwrite newer; status and data preserved" $
        let stateWithNewerItem = insertItem bookmarkItem2 testState
            stateWithNewerItemPending = stateWithNewerItem & mContents . ix (_biId bookmarkItem2) . _1 .~ ToBeArchived
            stateAfterAttemptedUpdate = insertItem bookmarkItem1 stateWithNewerItemPending
         in Map.lookup (_biId bookmarkItem2) (view mContents stateAfterAttemptedUpdate) @?= Just (ToBeArchived, bookmarkItem2),
      testCase "insertItem: item with same timestamp does overwrite; status preserved, data updated" $
        let stateWithOriginalItem = insertItem bookmarkItem1 testState
            stateWithOriginalItemPending = stateWithOriginalItem & mContents . ix (_biId bookmarkItem1) . _1 .~ ToBeArchived
            stateAfterAttemptedUpdate = insertItem bookmarkItem1_same_ts_diff_title stateWithOriginalItemPending
         in Map.lookup (_biId bookmarkItem1) (view mContents stateAfterAttemptedUpdate) @?= Just (ToBeArchived, bookmarkItem1_same_ts_diff_title),
      testCase "insertItems: inserting an item that is present overwrites if newer" $
        let s = insertItems [bookmarkItem1, bookmarkItem2] testState
         in fmap (view (_2 . biTitle)) (Map.lookup (_biId bookmarkItem1) (view mContents s)) @?= Just "newer title",
      testCase "insertItems: inserting an item that is present overwrites if newer, insertion order does not matter" $
        let s = insertItems [bookmarkItem2, bookmarkItem1] testState
         in fmap (view (_2 . biTitle)) (Map.lookup (_biId bookmarkItem1) (view mContents s)) @?= Just "newer title",
      testCase "insertItems: basic insertion count" $
        length (insertItems [bookmarkItem1] testState ^. mContents) @?= 1
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
    settle $
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

-- | Observe the next, non-announcement event injected into the event
-- channel (blocking). Skips SetAgentClients connect/disconnect bookkeeping.
readAgentInjected :: BChan I.Msg -> IO I.Msg
readAgentInjected ch = do
  ev <- readBChan ch
  case ev of
    I.AgentClients _ -> readAgentInjected ch
    _ -> pure ev

-- | What the agent server's clock reads in these tests.
testNow :: I.Now
testNow = I.Now (read "2020-01-01 06:00:00 UTC") utc

-- | Poll-connect to a socket until a listener accepts (server fully bound),
-- retrying a bounded number of times with a short sleep. Guards against the
-- race where a test launches a server in the background and immediately
-- probes it before the server has actually bound its path.
waitUntilReachable :: Int -> FilePath -> IO ()
waitUntilReachable attempts0 path = go attempts0 []
  where
    -- On failure, the causes of the last attempts, newest first.
    go attempts errs
      | attempts <= 0 = assertFailure ("server never became reachable at " ++ path ++ "; last attempts: " ++ intercalate " | " (take 5 errs))
      | otherwise =
          reachable path
            >>= maybe (pure ()) (\e -> threadDelay 20000 >> go (attempts - 1) (e : errs))
    reachable :: FilePath -> IO (Maybe String)
    reachable p = do
      sock <- socket AF_UNIX Stream defaultProtocol
      -- Close the probe, or the server holds it as a client until GC.
      r <- try @IOException (connect sock (SockAddrUnix p) >> socketToHandle sock ReadWriteMode >>= hClose)
      close sock
      pure (either (Just . show) (const Nothing) r)

-- | Run the agent server on a throwaway socket with a snapshot TVar we own,
-- handing both that TVar and the event BChan to the action so the test can
-- bump versions and observe injected writes.
withAgentServer ::
  (TVar AgentSnapshot -> BChan I.Msg -> FilePath -> IO a) ->
  IO a
withAgentServer action = withSystemTempDirectory "hk" $ \dir -> do
  let path = dir </> "s.sock"
  snapVar <- newTVarIO emptySnapshot
  atomically (modifyTVar' snapVar (const (buildTestSnapshot 1)))
  events <- newBChan 10
  let env =
        AgentEnv
          { aeSnapshot = snapVar,
            aeInject = \evt -> do
              void (writeBChan events evt)
              pure True,
            aeNow = pure testNow
          }
  -- The readiness probe is a real client: the server announces it with two
  -- AgentClients messages (+1, then -1 from its own thread, so possibly
  -- after the test's first events). readAgentInjected skips them; any test asserting
  -- exact BChan contents must account for them.
  bound <- newBoundSocket
  withAsync (runAgentServer bound path env) $ \_ -> do
    waitUntilReachable 100 path
    action snapVar events path

-- | The TUI's socket lifecycle: decide ownership, serve until reachable,
-- stop, clean up. Only ever pointed at a fresh temp root.
-- The extra action runs while the server is up.
serveThenCleanup :: FilePath -> IO () -> IO ()
serveThenCleanup path whileUp = do
  ours <- socketDirIsOurs path
  bound <- newBoundSocket
  withAsync (quietEnv >>= runAgentServer bound path) $ \_ -> waitUntilReachable 100 path >> whileUp
  cleanupAgentSocket bound ours path

-- | A real TVar, not undefined: 'AgentEnv' is strict, and the accept loop
-- forces it on the first connection; an undefined field killed the server.
quietEnv :: IO AgentEnv
quietEnv = (\v -> AgentEnv {aeSnapshot = v, aeInject = const (pure True), aeNow = pure testNow}) <$> newTVarIO emptySnapshot

-- | Start a server on a path that holds something else; under the old
-- reclaim it unlinked that and served forever, hence the timeout.
startOnOccupied :: FilePath -> IO (Maybe (Either IOException ()))
startOnOccupied path = do
  bound <- newBoundSocket
  timeout 2000000 (try @IOException (quietEnv >>= runAgentServer bound path))

-- | What sits at a path, for failure messages: absent, or type and dev+inode.
describePath :: FilePath -> IO String
describePath path = do
  r <- try @IOException (getSymbolicLinkStatus path)
  pure $ case r of
    Left e -> "nothing (" <> show e <> ")"
    Right st -> (if isSocket st then "socket " else "non-socket ") <> show (deviceID st, fileID st)

holdsSocket :: FilePath -> Maybe (DeviceID, FileID) -> IO Bool
holdsSocket path expected = do
  r <- try @IOException (getSymbolicLinkStatus path)
  pure $ case (r, expected) of
    (Right st, Just i) -> isSocket st && (deviceID st, fileID st) == i
    _ -> False

-- | 'waitUntilReachable' with the standard budget, failing with what the
-- path holds and what each instance recorded.
reachableOr :: String -> FilePath -> [(String, BoundSocket)] -> IO ()
reachableOr what path instances = do
  r <- try @SomeException (waitUntilReachable 100 path)
  case r of
    Right () -> pure ()
    Left e -> do
      at' <- describePath path
      ids <- traverse (\(n, b) -> (\i -> n <> " = " <> show i) <$> boundSocketId b) instances
      assertFailure (what <> "; path holds " <> at' <> "; " <> intercalate ", " ids <> "; " <> unwords (lines (displayException e)))

failCountPosts :: I.Msg -> IO ()
failCountPosts (I.AgentClients _) = throwIO (userError "post failed")
failCountPosts _ = pure ()

-- | The newest AgentClients count posted so far.
latestCount :: IORef [I.Msg] -> IO (Maybe Int)
latestCount ref = listToMaybe . mapMaybe count <$> readIORef ref
  where
    count (I.AgentClients n) = Just n
    count _ = Nothing

-- | Polls every 20 ms; whether the condition held within the budget.
waitFor :: Int -> IO Bool -> IO Bool
waitFor n cond = cond >>= \ok -> if ok || n <= 0 then pure ok else threadDelay 20000 >> waitFor (n - 1) cond

reportsFailure :: I.Msg -> Bool
reportsFailure (I.Intent (I.SetStatus t)) = "inject broke" `T.isInfixOf` t
reportsFailure _ = False

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
          injected @?= I.Intent (I.SetFilter "haskell"),
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
      testCase "the shipped client reads state over the socket" $
        withAgentServer $ \snapVar _events path -> do
          atomically (modifyTVar' snapVar (const (buildTestSnapshot 11)))
          res <- callAgent path (ARead CmdGetState)
          case res of
            Left err -> assertFailure (T.unpack err)
            Right (A.Object o) -> do
              KM.lookup (AKey.fromText "ok") o @?= Just (A.Bool True)
              case KM.lookup (AKey.fromText "result") o of
                Just (A.Object r) ->
                  KM.lookup (AKey.fromText "version") r @?= Just (A.Number 11)
                _ -> assertFailure "no result object"
            Right _ -> assertFailure "client response not an object",
      testCase "the shipped client injects a validated write" $
        withAgentServer $ \snapVar events path -> do
          atomically (modifyTVar' snapVar (const (buildTestSnapshot 12)))
          res <- callAgent path (AWrite (CmdSetFlag (BookmarkItemId "in-1") FlagArchive))
          case res of
            Left err -> assertFailure (T.unpack err)
            Right (A.Object o) ->
              KM.lookup (AKey.fromText "ok") o @?= Just (A.Bool True)
            Right _ -> assertFailure "client response not an object"
          injected <- readAgentInjected events
          injected @?= I.Intent (I.SetFlag (BookmarkItemId "in-1") FlagArchive testNow),
      testCase "the shipped client reports an unreachable socket" $ do
        res <- callAgent "/nonexistent/hocket-agent-absent.sock" (ARead CmdGetState)
        assertBool "expected a transport error" (isLeft res),
      testCase "isChannelFullError recognizes the channel-cap reply" $ do
        let chan v = A.Object (KM.fromList [(AKey.fromText "ok", A.Bool v), (AKey.fromText "error", A.String "event channel full, retry")])
        assertBool "should match the server's channel-full reply" $ isChannelFullError (chan False)
        assertBool "should reject an ok:true reply" $ not (isChannelFullError (chan True))
        assertBool "should reject a genuine validation error" $ not (isChannelFullError (A.Object (KM.fromList [(AKey.fromText "ok", A.Bool False), (AKey.fromText "error", A.String "bad request: unknown item")])))
        assertBool "should reject a non-object and a non-string error" $ not (isChannelFullError (A.String "event channel full, retry")),
      testCase "isChannelFullError ignores an error absent or different message" $ do
        assertBool "error absent" $ not (isChannelFullError (A.Object (KM.fromList [(AKey.fromText "ok", A.Bool False)])))
        assertBool "different message does not match" $ not (isChannelFullError (A.Object (KM.fromList [(AKey.fromText "ok", A.Bool False), (AKey.fromText "error", A.String "something else")]))),
      testCase "binding into a pre-existing directory leaves its mode alone" $ do
        -- An explicit --agent-socket-path may point into a directory we did
        -- not create (/tmp, $HOME). Tightening it to 0700 would either fail
        -- outright (killing the server) or silently lock down a directory
        -- that is none of our business.
        tmp <- getTemporaryDirectory
        pid <- getProcessID
        e <- epochTime
        let dir = tmp </> ("hocket-mode-" ++ show pid ++ "-" ++ show e)
            path = dir </> "control.sock"
        createDirectoryIfMissing True dir
        setFileMode dir 0o755
        snapVar <- newTVarIO emptySnapshot
        atomically (modifyTVar' snapVar (const (buildTestSnapshot 1)))
        events <- newBChan 10
        let env =
              AgentEnv
                { aeSnapshot = snapVar,
                  aeInject = \evt -> do
                    void (writeBChan events evt)
                    pure True,
                  aeNow = pure testNow
                }
        bound <- newBoundSocket
        withAsync (runAgentServer bound path env) $ \_ -> do
          waitUntilReachable 50 path
          st <- getFileStatus dir
          (fileMode st `mod` 0o1000) @?= 0o755
          -- and the server is genuinely serving from that directory
          res <- callAgent path (ARead CmdGetState)
          assertBool "expected a response from the server" (not (isLeft res))
        removeDirectoryRecursive dir,
      testCase "cleanup keeps a pre-existing directory and its other files (B7)" $
        withSystemTempDirectory "hk" $ \root -> do
          let dir = root </> "shared"
          createDirectory dir
          writeFile (dir </> "keep") "x"
          serveThenCleanup (dir </> "s.sock") (pure ())
          doesFileExist (dir </> "keep") >>= (@?= True)
          doesDirectoryExist dir >>= (@?= True),
      testCase "cleanup keeps a pre-existing empty directory (B7)" $
        withSystemTempDirectory "hk" $ \root -> do
          let dir = root </> "empty"
          createDirectory dir
          serveThenCleanup (dir </> "s.sock") (pure ())
          doesDirectoryExist dir >>= (@?= True),
      testCase "cleanup removes a directory the server created" $
        withSystemTempDirectory "hk" $ \root -> do
          let dir = root </> "ours"
          serveThenCleanup (dir </> "s.sock") (pure ())
          doesDirectoryExist dir >>= (@?= False),
      testCase "cleanup keeps a created directory that gained other files (B7b)" $
        withSystemTempDirectory "hk" $ \root -> do
          let dir = root </> "ours"
          serveThenCleanup (dir </> "s.sock") (writeFile (dir </> "other") "x")
          doesFileExist (dir </> "other") >>= (@?= True)
          doesDirectoryExist dir >>= (@?= True),
      testCase "a regular file at the socket path is refused and survives (B7b)" $
        withSystemTempDirectory "hk" $ \root -> do
          let path = root </> "notes.txt"
          writeFile path "precious"
          r <- startOnOccupied path
          fmap (either (const "failed") (const "ran")) r @?= Just "failed"
          readFile path >>= (@?= "precious"),
      testCase "a symlink at the socket path is not followed or removed (B7b)" $
        withSystemTempDirectory "hk" $ \root -> do
          let target = root </> "target"
              path = root </> "link"
          writeFile target "precious"
          createSymbolicLink target path
          r <- startOnOccupied path
          fmap (either (const "failed") (const "ran")) r @?= Just "failed"
          pathIsSymbolicLink path >>= (@?= True)
          readFile target >>= (@?= "precious"),
      testCase "cleanup leaves a socket another instance bound at the same path (B7b)" $
        withSystemTempDirectory "hk" $ \root -> do
          let path = root </> "s.sock"
          bound1 <- newBoundSocket
          withAsync (quietEnv >>= runAgentServer bound1 path) $ \a1 -> do
            reachableOr "instance 1 never became reachable" path [("bound1", bound1)]
            removeFile path
            bound2 <- newBoundSocket
            withAsync (quietEnv >>= runAgentServer bound2 path) $ \a2 -> do
              reachableOr "instance 2 never became reachable" path [("bound1", bound1), ("bound2", bound2)]
              cleanupAgentSocket bound1 False path
              -- Deterministic: the path still holds instance 2's socket.
              id2 <- boundSocketId bound2
              after <- describePath path
              assertBool ("cleanup with bound1 changed the path: " <> after <> ", bound2 = " <> show id2) =<< holdsSocket path id2
              r <- try @SomeException (reachableOr "instance 2 unreachable after cleanup" path [("bound1", bound1), ("bound2", bound2)])
              case r of
                Right () -> pure ()
                Left e -> do
                  s1 <- poll a1
                  s2 <- poll a2
                  assertFailure (unwords (lines (displayException e)) <> "; server 1: " <> maybe "running" show s1 <> "; server 2: " <> maybe "running" show s2),
      testCase "a losing second instance does not unlink the live socket (B7b)" $
        withAgentServer $ \_ _ path -> do
          bound2 <- newBoundSocket
          r <- try @IOException (quietEnv >>= runAgentServer bound2 path)
          assertBool "second bind should fail" (isLeft r)
          cleanupAgentSocket bound2 False path
          waitUntilReachable 1 path,
      testCase "SIGTERM and SIGHUP unwind so finally runs (B8)" $
        for_ [sigTERM, sigHUP] $ \sig -> do
          ran <- newIORef False
          unwindOnTermination
          r <- try @Terminated ((raiseSignal sig >> threadDelay 5000000) `finally` writeIORef ran True)
          for_ [sigTERM, sigHUP] $ \s -> installHandler s Default Nothing
          r @?= Left (Terminated sig)
          readIORef ran >>= (@?= True),
      testCase "a termination signal exits with 128 + the signal number" $
        for_ [(sigTERM, 143), (sigHUP, 129)] $ \(sig, code) -> do
          r <- try @ExitCode (exitOnTerminated (throwIO (Terminated sig) :: IO ()))
          r @?= Left (ExitFailure code),
      testCase "cancel stops the server even while posting the client count blocks" $ do
        -- Blocking in the count post itself, and in the report of a failed one.
        for_ [const (pure ()), failCountPosts] $ \beforeBlocking ->
          withSystemTempDirectory "hk" $ \root -> do
            let path = root </> "s.sock"
            gate <- newEmptyMVar
            snapVar <- newTVarIO (buildTestSnapshot 1)
            let env = AgentEnv {aeSnapshot = snapVar, aeInject = \m -> beforeBlocking m >> readMVar gate, aeNow = pure testNow}
            bound <- newBoundSocket
            a <- async (runAgentServer bound path env)
            waitUntilReachable 100 path
            threadDelay 100000
            stopped <- timeout 2000000 (cancel a)
            putMVar gate True -- keeps the gate reachable: no BlockedIndefinitelyOnMVar
            stopped @?= Just (),
      testCase "a count post dropped by a full channel is posted again" $
        withSystemTempDirectory "hk" $ \root -> do
          let path = root </> "s.sock"
          posted <- newIORef []
          full <- newIORef True
          snapVar <- newTVarIO (buildTestSnapshot 1)
          let inject m = readIORef full >>= \f -> if f then pure False else atomicModifyIORef' posted (\ms -> (m : ms, True))
              env = AgentEnv {aeSnapshot = snapVar, aeInject = inject, aeNow = pure testNow}
          bound <- newBoundSocket
          withAsync (runAgentServer bound path env) $ \_ -> do
            waitUntilReachable 100 path
            h <- connectAgentClient path
            _ <- sendRequest h "{\"id\":1,\"method\":\"get_state\"}"
            threadDelay 100000
            writeIORef full False -- the TUI drained its queue; the client stays
            healed <- waitFor 100 ((== Just 1) <$> latestCount posted)
            hClose h
            healed @?= True,
      testCase "a permanently full channel is retried with a backoff, not a spin" $
        withSystemTempDirectory "hk" $ \root -> do
          let path = root </> "s.sock"
          attempts <- newIORef (0 :: Int)
          snapVar <- newTVarIO (buildTestSnapshot 1)
          let inject m = case m of
                I.AgentClients _ -> atomicModifyIORef' attempts (\n -> (n + 1, False))
                _ -> pure True
              env = AgentEnv {aeSnapshot = snapVar, aeInject = inject, aeNow = pure testNow}
          bound <- newBoundSocket
          withAsync (runAgentServer bound path env) $ \_ -> do
            waitUntilReachable 100 path
            h <- connectAgentClient path
            _ <- sendRequest h "{\"id\":1,\"method\":\"get_state\"}"
            writeIORef attempts 0
            threadDelay 1000000
            n <- readIORef attempts
            hClose h
            -- 50 ms backoff: about 20 a second; a spin makes thousands.
            assertBool ("attempts in 1 s: " <> show n) (n >= 1 && n <= 60),
      testCase "a persistent post failure is reported once, not on every retry" $
        withSystemTempDirectory "hk" $ \root -> do
          let path = root </> "s.sock"
          reports <- newIORef (0 :: Int)
          snapVar <- newTVarIO (buildTestSnapshot 1)
          let inject m = case m of
                I.AgentClients _ -> throwIO (userError "post failed")
                _ -> atomicModifyIORef' reports (\n -> (n + 1, True))
              env = AgentEnv {aeSnapshot = snapVar, aeInject = inject, aeNow = pure testNow}
          bound <- newBoundSocket
          withAsync (runAgentServer bound path env) $ \_ -> do
            waitUntilReachable 100 path
            h <- connectAgentClient path
            _ <- sendRequest h "{\"id\":1,\"method\":\"get_state\"}"
            -- About 10 failed retries at 50 ms; one streak, one report.
            threadDelay 500000
            n <- readIORef reports
            hClose h
            n @?= 1,
      testCase "the client count returns to 0 when clients leave" $
        withSystemTempDirectory "hk" $ \root -> do
          let path = root </> "s.sock"
          posted <- newIORef []
          snapVar <- newTVarIO (buildTestSnapshot 1)
          let env = AgentEnv {aeSnapshot = snapVar, aeInject = \m -> atomicModifyIORef' posted (\ms -> (m : ms, True)), aeNow = pure testNow}
          bound <- newBoundSocket
          withAsync (runAgentServer bound path env) $ \_ -> do
            waitUntilReachable 100 path
            h <- connectAgentClient path
            _ <- sendRequest h "{\"id\":1,\"method\":\"get_state\"}"
            hClose h
            lastCount <- waitFor 100 ((== Just 0) <$> latestCount posted)
            lastCount @?= True,
      testCase "a failing announce neither stops the accept loop nor goes unreported" $
        withSystemTempDirectory "hk" $ \root -> do
          let path = root </> "s.sock"
          injected <- newIORef []
          calls <- newIORef (0 :: Int)
          snapVar <- newTVarIO (buildTestSnapshot 1)
          let inject m = do
                n <- atomicModifyIORef' calls (\c -> (c + 1, c))
                if n == 0 then throwIO (userError "inject broke") else atomicModifyIORef' injected (\ms -> (m : ms, True))
              env = AgentEnv {aeSnapshot = snapVar, aeInject = inject, aeNow = pure testNow}
          bound <- newBoundSocket
          withAsync (runAgentServer bound path env) $ \_ -> do
            waitUntilReachable 100 path
            h <- connectAgentClient path
            resp <- sendRequest h "{\"id\":1,\"method\":\"get_state\"}"
            case resp of
              A.Object o -> KM.lookup (AKey.fromText "ok") o @?= Just (A.Bool True)
              _ -> assertFailure "no answer after a failed announce"
            -- The poster thread reports asynchronously.
            reported <- waitFor 100 (any reportsFailure <$> readIORef injected)
            ms <- readIORef injected
            assertBool ("the failure was not reported: " <> show ms) reported,
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
                    aeNow = pure testNow
                  }
          -- Ensure server 1 has actually bound the path before we probe, so
          -- server 2's bind can genuinely be rejected rather than racing
          -- ahead and winning the path (which would hang us in accept).
          waitUntilReachable 50 path
          bound2 <- newBoundSocket
          result <- try @IOException (runAgentServer bound2 path env2)
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
