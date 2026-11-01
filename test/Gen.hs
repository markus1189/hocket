{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Hedgehog generators shared by every property and state-machine test (D9).
module Gen
  ( genBookmarkItem,
    genBookmarkItemId,
    genPendingAction,
    genNow,
    genUTCTime,
    genTimeZone,
    genModel,
    genClientError,
    clientErrorRows,
    genClientErrorRow,
    genFailureScript,
    genAgentCmd,
    genAgentSnapshot,
    genVideoFilterMode,
    genAsyncOp,
  )
where

import Control.Exception (toException)
import Control.Lens ((&), (.~), (^.))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day (ModifiedJulianDay), fromGregorian, toModifiedJulianDay)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.LocalTime (TimeZone, minutesToTimeZone)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.Bookmark.Agent.Protocol
import Network.Bookmark.Agent.Snapshot
import Network.Bookmark.Types
import Network.Bookmark.Ui.State
import Network.HTTP.Media (MediaType, (//))
import Network.HTTP.Types (Status, http11, mkStatus)
import Network.Raindrop (ErrorScope (..))
import Servant.Client.Core (BaseUrl (..), ClientError (..), RequestF (..), ResponseF (..), Scheme (Https), defaultRequest)
import System.IO.Error (userError)

-- | Millisecond precision: that is what Raindrop sends, and coarser than any
-- encoder we round-trip through.
genUTCTime :: Gen UTCTime
genUTCTime = do
  day <- ModifiedJulianDay <$> Gen.integral (Range.linear (toModifiedJulianDay (fromGregorian 2015 1 1)) (toModifiedJulianDay (fromGregorian 2030 12 31)))
  ms <- Gen.integral (Range.linear 0 (86400 * 1000 - 1))
  pure (UTCTime day (secondsToDiffTime ms / 1000))

-- | Every real offset is a multiple of 15 minutes within −12:00..+14:00,
-- which covers +05:30 and +05:45.
genTimeZone :: Gen TimeZone
genTimeZone = minutesToTimeZone . (* 15) <$> Gen.int (Range.linearFrom 0 (-48) 56)

genNow :: Gen UTCTime
genNow = genUTCTime

-- | Ids are numeric because the Raindrop JSON carries them as integers.
genBookmarkItemId :: Gen BookmarkItemId
genBookmarkItemId = BookmarkItemId . T.pack . show <$> Gen.int (Range.linear 1 1000000)

genWord :: Gen Text
genWord = Gen.text (Range.linear 1 10) Gen.alphaNum

genDomain :: Gen Text
genDomain = Gen.element ["youtube.com", "youtu.be", "github.com", "example.org", "news.ycombinator.com", "blog.example.de"]

genBookmarkItem :: Gen BookmarkItem
genBookmarkItem = do
  bid <- genBookmarkItemId
  domain <- genDomain
  path <- genWord
  let link = "https://" <> domain <> "/" <> path
  created <- genUTCTime
  lastUpdate <- genUTCTime
  tags <- Gen.list (Range.linear 0 4) (Gen.frequency [(1, pure "video"), (4, genWord)])
  BookmarkItem bid link
    <$> Gen.text (Range.linear 0 40) Gen.unicode
    <*> Gen.text (Range.linear 0 20) Gen.unicode
    <*> Gen.element ["link", "article", "video", "image"]
    <*> pure tags
    <*> Gen.bool
    <*> pure created
    <*> pure (max created lastUpdate)
    <*> pure domain
    <*> Gen.text (Range.linear 0 30) Gen.unicode
    <*> Gen.int (Range.linear (-1000) 1000)
    <*> Gen.list (Range.linear 0 3) (Gen.text (Range.linear 1 20) Gen.unicode)
    <*> Gen.element [-1, 0, 4711, 4712]
    <*> Gen.bool
    <*> Gen.maybe genUTCTime

genPendingAction :: Gen PendingAction
genPendingAction = Gen.choice [pure None, pure ToBeArchived, ToBeReminded <$> genUTCTime, pure ReminderToBeRemoved]

-- | A flag the TUI could have produced for this item: reminders are only set
-- on items without one and only removed from items that have one.
genConsistentAction :: BookmarkItem -> Gen PendingAction
genConsistentAction item
  | isJust (item ^. biReminder) = Gen.element [None, ToBeArchived, ReminderToBeRemoved]
  | otherwise = Gen.choice [pure None, pure ToBeArchived, ToBeReminded <$> genUTCTime]

genContents :: Gen (Map BookmarkItemId (PendingAction, BookmarkItem))
genContents = do
  items <- Gen.list (Range.linear 0 30) genBookmarkItem
  Map.fromList <$> traverse (\i -> (\a -> (i ^. biId, (a, i))) <$> genConsistentAction i) items

-- | Today's model is 'HocketState'; only the fields the pure core reads are
-- generated, the brick list stays as 'initialState' leaves it. The archive
-- collection is one 'genBookmarkItem' also assigns, so some items already
-- sit in it.
genModel :: Gen HocketState
genModel = do
  contents <- genContents
  lastUpdated <- Gen.maybe genPOSIXTime
  status <- Gen.maybe genWord
  futureRem <- Gen.bool
  video <- genVideoFilterMode
  pure $
    initialState (BookmarkCredentials (RaindropToken "") 4711)
      & hsContents .~ contents
      & hsLastUpdated .~ lastUpdated
      & hsStatus .~ status
      & hsShowFutureReminders .~ futureRem
      & hsVideoFilter .~ video

genPOSIXTime :: Gen POSIXTime
genPOSIXTime = (/ 1000) . fromInteger <$> Gen.integral (Range.linear 1400000000000 1900000000000)

genVideoFilterMode :: Gen VideoFilterMode
genVideoFilterMode = Gen.element [NoVideoFilter, ShowOnlyVideos, HideVideos]

genAsyncOp :: Gen AsyncOp
genAsyncOp = Gen.element [OpFetchItems, OpExecuteBatch]

genResponse :: Status -> Gen (ResponseF LBS.ByteString)
genResponse status = do
  body <- LBS.pack . T.unpack <$> Gen.text (Range.linear 0 30) Gen.alphaNum
  pure (Response status mempty http11 body)

genStatus :: [Int] -> Gen Status
genStatus codes = (`mkStatus` "") <$> Gen.element codes

genMediaType :: Gen MediaType
genMediaType = Gen.element ["text" // "html", "text" // "plain", "application" // "xml"]

-- | The D6 table (plan §5, rulings in §11): one generator per row, each
-- named and scoped by hand so a property can check 'classify' against it
-- and cover every row.
clientErrorRows :: [(String, ErrorScope, Gen ClientError)]
clientErrorRows =
  [ ("other 4xx", ItemScoped, failure [400, 404, 405, 409, 410, 422, 499]),
    ("401/403", Global, failure [401, 403]),
    ("408", Global, failure [408]),
    ("429", Global, failure [429]),
    ("5xx", Global, failure [500, 502, 503, 504, 599]),
    ("1xx/3xx", Global, failure [100, 301, 302, 304]),
    ("connection", Global, pure (ConnectionError (toException (userError "connection refused")))),
    ("decode", Global, DecodeFailure "Error in $: key \"items\" not found" <$> (genStatus [200] >>= genResponse)),
    ("content type", Global, UnsupportedContentType <$> genMediaType <*> (genStatus [200] >>= genResponse)),
    ("content-type header", Global, InvalidContentTypeHeader <$> (genStatus [200] >>= genResponse))
  ]
  where
    failure codes = FailureResponse req <$> (genStatus codes >>= genResponse)
    req = defaultRequest {requestBody = Nothing, requestPath = (BaseUrl Https "api.raindrop.io" 443 "/rest/v1", "")}

-- | A uniformly chosen D6 row: its name, the error, and its scope.
genClientErrorRow :: Gen (String, ClientError, ErrorScope)
genClientErrorRow = Gen.choice [(name,,scope) <$> gen | (name, scope, gen) <- clientErrorRows]

genClientError :: Gen ClientError
genClientError = (\(_, err, _) -> err) <$> genClientErrorRow

-- | Per-call outcomes for a scripted fake: 'Right False' is Raindrop's
-- @result: false@ answer.
genFailureScript :: Gen [Either ClientError Bool]
genFailureScript = Gen.list (Range.linear 0 20) (Gen.frequency [(6, pure (Right True)), (1, pure (Right False)), (3, Left <$> genClientError)])

genFlagAction :: Gen FlagAction
genFlagAction = Gen.element [FlagArchive, FlagReminder, FlagRemoveReminder, FlagNone]

genAgentCmd :: Gen AgentCmd
genAgentCmd =
  Gen.choice
    [ pure (ARead CmdGetState),
      ARead <$> (CmdListItems <$> Gen.bool <*> Gen.bool),
      ARead . CmdGetItem <$> genBookmarkItemId,
      AWait <$> Gen.int (Range.linear 0 100000) <*> Gen.int (Range.linear 0 60000),
      AWrite <$> (CmdSetFlag <$> genBookmarkItemId <*> genFlagAction),
      pure (AWrite CmdClearFlags),
      pure (AWrite CmdFlagAllArchive),
      pure (AWrite CmdExecute),
      pure (AWrite CmdRefresh),
      AWrite . CmdSetFilter <$> Gen.text (Range.linear 0 20) Gen.unicode,
      AWrite . CmdSetVideoFilter <$> genVideoFilterMode,
      AWrite . CmdSetShowFutureReminders <$> Gen.bool,
      AWrite . CmdSelectItem <$> genBookmarkItemId,
      AWrite . CmdOpenItem <$> genBookmarkItemId,
      AWrite . CmdOpenAndFlag <$> genBookmarkItemId,
      AWrite . CmdSetStatus <$> Gen.text (Range.linear 0 20) Gen.unicode
    ]

genSnapshotItem :: Gen SnapshotItem
genSnapshotItem =
  SnapshotItem
    <$> genBookmarkItemId
    <*> Gen.text (Range.linear 0 30) Gen.unicode
    <*> Gen.text (Range.linear 0 30) Gen.unicode
    <*> Gen.list (Range.linear 0 4) genWord
    <*> Gen.text (Range.linear 0 20) Gen.unicode
    <*> Gen.text (Range.linear 0 20) Gen.unicode
    <*> genUTCTime
    <*> Gen.maybe genUTCTime
    <*> Gen.bool
    <*> genPendingAction
    <*> Gen.bool

genAgentSnapshot :: Gen AgentSnapshot
genAgentSnapshot =
  AgentSnapshot
    <$> Gen.int (Range.linear 0 100000)
    <*> Gen.list (Range.linear 0 10) genSnapshotItem
    <*> Gen.maybe genBookmarkItemId
    <*> Gen.text (Range.linear 0 20) Gen.unicode
    <*> genVideoFilterMode
    <*> Gen.bool
    <*> Gen.maybe (Gen.text (Range.linear 0 20) Gen.unicode)
    <*> Gen.maybe genPOSIXTime
    <*> Gen.maybe genAsyncOp
