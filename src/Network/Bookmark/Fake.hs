{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The fixture backend's pure core (plan D8): an in-memory Raindrop that
-- answers 'BookmarkRequest's. The shell wraps it in an 'IORef'; Phase 6
-- reuses it as the state-machine model.
--
-- Unchecked assumptions about the real API, for the E2E run to confirm:
--
-- 1. @lastUpdate:>DATE@ compares the item's UTC day, strictly. Raindrop may
--    mean another boundary or time zone; the strict reading hides less, so
--    a missing safety margin in 'planSync' shows up here first.
-- 2. The default sort is @-created@ (ties by id), recalled from Raindrop's
--    docs. Paging only needs a stable order, so a wrong guess changes page
--    contents, not which items a full fetch returns.
-- 3. The batch move silently skips ids Raindrop does not have, as bulk
--    endpoints usually do. Failing instead would make one item deleted
--    elsewhere abort the whole archive step, a failure no test could pin
--    to real behaviour.
--
-- Deliberate semantics:
--
-- 4. No retry: the fake answers at the 'BookmarkRequest' level, above
--    'Network.Raindrop.raindrop's retry (plan §11 #16), so a 503 fails
--    once and 'isTransient' is never exercised.
-- 5. @STEP:N@ counts calls of that step since the fixture started, failed
--    ones included; a second @X@ keeps counting, so @set-reminders:2=404@
--    fires only once. The TUI sends reminder items in 'Map' order of the
--    'Text' id (@"10" < "9"@), so E2E seeds should use same-width ids.
-- 6. A search other than @lastUpdate:>@ is a 400, so unmodelled queries
--    fail loudly.
-- 7. @result: false@, decode and content-type failures cannot be scripted
--    ('FailStatus' is HTTP status or connection); Phase 1 pins them purely.
module Network.Bookmark.Fake
  ( FakeRaindrop,
    frItems,
    frArchive,
    frPageSize,
    frRules,
    frCalls,
    FakeStep (..),
    FailStatus (..),
    FailRule (..),
    emptyFake,
    seedFake,
    encodeFixtureDump,
    serveFake,
    parseFixtureFail,
  )
where

import Control.Exception (toException)
import Control.Lens
import Data.Aeson (eitherDecode)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.List (find, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Time (Day, UTCTime (utctDay), defaultTimeLocale, parseTimeM)
import Network.Bookmark.Types
import Network.HTTP.Types (http11, mkStatus)
import Network.Raindrop.Api (Items (..))
import Numeric.Natural (Natural)
import Servant.Client.Core (BaseUrl (..), ClientError (..), RequestF (..), ResponseF (..), Scheme (Https), defaultRequest)

-- | The @--fixture-fail@ step names; 'AddBookmark' belongs to none.
data FakeStep = StepFetch | StepArchive | StepSetReminders | StepRemoveReminders
  deriving (Show, Eq, Ord, Enum, Bounded)

data FailStatus = FailHttp !Int | FailConn deriving (Show, Eq)

-- | @Nothing@ for the call number fails every call of the step.
data FailRule = FailRule
  { frStep :: !FakeStep,
    frNth :: !(Maybe Int),
    frStatus :: !FailStatus
  }
  deriving (Show, Eq)

data FakeRaindrop = FakeRaindrop
  { _frItems :: !(Map BookmarkItemId BookmarkItem),
    _frArchive :: !Int,
    _frPageSize :: !Natural,
    _frRules :: ![FailRule],
    -- | Calls made per step, failed ones included: what @STEP:N@ counts.
    _frCalls :: !(Map FakeStep Int)
  }
  deriving (Show, Eq)

makeLenses ''FakeRaindrop

-- | The real client asks for 50 items per page.
emptyFake :: Int -> FakeRaindrop
emptyFake archive = FakeRaindrop Map.empty archive 50 [] Map.empty

-- | Seed from a Raindrop list response (@items@, @count@).
seedFake :: Int -> LBS.ByteString -> Either String FakeRaindrop
seedFake archive body = do
  Items _ items <- eitherDecode body
  pure (emptyFake archive & frItems .~ Map.fromList [(i ^. biId, i) | i <- items])

-- | The store's items as a Raindrop list response, so a dump can seed the
-- next run through 'seedFake'.
encodeFixtureDump :: FakeRaindrop -> LBS.ByteString
encodeFixtureDump fr =
  A.encode (A.object ["result" A..= True, "items" A..= items, "count" A..= length items])
  where
    items = Map.elems (fr ^. frItems)

stepOf :: BookmarkRequest a -> Maybe FakeStep
stepOf = \case
  AddBookmark {} -> Nothing
  ArchiveBookmark {} -> Just StepArchive
  BatchArchiveBookmarks {} -> Just StepArchive
  SetReminder {} -> Just StepSetReminders
  RemoveReminder {} -> Just StepRemoveReminders
  RetrieveBookmarks {} -> Just StepFetch

-- | Answer one request at time @now@. A call matched by a failure rule
-- returns its error and changes nothing but the call count.
serveFake :: UTCTime -> BookmarkRequest a -> FakeRaindrop -> (Either ClientError a, FakeRaindrop)
serveFake now req fr = case stepOf req of
  Nothing -> serve now req fr
  Just st ->
    let n = maybe 1 (+ 1) (fr ^. frCalls . at st)
        counted = fr & frCalls . at st ?~ n
        matches r = frStep r == st && maybe True (== n) (frNth r)
     in case find matches (fr ^. frRules) of
          Just r -> (Left (statusError (frStatus r)), counted)
          Nothing -> serve now req counted

serve :: UTCTime -> BookmarkRequest a -> FakeRaindrop -> (Either ClientError a, FakeRaindrop)
serve now req fr = case req of
  AddBookmark link mColl tags -> case maybe (Just (-1)) readInt mColl of
    Nothing -> (Left (httpError 400), fr)
    Just coll ->
      let bid = BookmarkItemId (T.pack (show (nextId (fr ^. frItems))))
          item = BookmarkItem bid link "" "" "link" tags False now now "" link 0 [] coll False Nothing
       in (Right (Just bid), fr & frItems . at bid ?~ item)
  ArchiveBookmark bid -> patch bid (biCollectionId .~ fr ^. frArchive)
  BatchArchiveBookmarks bids ->
    -- Unknown ids are skipped: assumption 3.
    (Right True, foldr (\bid -> frItems . ix bid %~ bump . (biCollectionId .~ fr ^. frArchive)) fr bids)
  SetReminder bid t -> patch bid (biReminder ?~ t)
  RemoveReminder bid -> patch bid (biReminder .~ Nothing)
  RetrieveBookmarks page (RaindropCollectionId cid) search ->
    case (,) <$> readInt cid <*> traverse parseSearch search of
      Nothing -> (Left (httpError 400), fr)
      Just (coll, since) ->
        let hits =
              sortOn (\i -> (Down (i ^. biCreated), i ^. biId)) $
                filter (\i -> inCollection coll i && maybe True (\d -> utctDay (i ^. biLastUpdate) > d) since) (Map.elems (fr ^. frItems))
            size = fromIntegral (fr ^. frPageSize)
         in (Right (fromIntegral (length hits), take size (drop (fromIntegral page * size) hits)), fr)
  where
    bump = biLastUpdate .~ now
    patch :: BookmarkItemId -> (BookmarkItem -> BookmarkItem) -> (Either ClientError Bool, FakeRaindrop)
    patch bid f
      | isJust (fr ^. frItems . at bid) = (Right True, fr & frItems . ix bid %~ bump . f)
      | otherwise = (Left (httpError 404), fr)
    inCollection coll i = coll == 0 || i ^. biCollectionId == coll

-- | @lastUpdate:>YYYY-MM-DD@, the only search the client sends ('planSync').
-- Assumption 1 decides the comparison; anything else is item 6.
parseSearch :: Text -> Maybe Day
parseSearch s = T.stripPrefix "lastUpdate:>" s >>= parseTimeM False defaultTimeLocale "%Y-%m-%d" . T.unpack

nextId :: Map BookmarkItemId BookmarkItem -> Int
nextId items = 1 + maximum (0 : [n | BookmarkItemId t <- Map.keys items, Just n <- [readInt t]])

readInt :: Text -> Maybe Int
readInt t = case TR.signed TR.decimal t of
  Right (n, rest) | T.null rest -> Just n
  _ -> Nothing

statusError :: FailStatus -> ClientError
statusError (FailHttp c) = httpError c
statusError FailConn = ConnectionError (toException (userError "fixture: connection refused"))

-- | Shaped like the D6 generators' errors, so 'classify' scopes it the same.
httpError :: Int -> ClientError
httpError c = FailureResponse req (Response (mkStatus c "") mempty http11 "")
  where
    req = defaultRequest {requestBody = Nothing, requestPath = (BaseUrl Https "api.raindrop.io" 443 "/rest/v1", "")}

-- | @STEP[:N]=STATUS@, e.g. @archive=503@, @set-reminders:2=404@, @fetch=conn@.
parseFixtureFail :: Text -> Either Text FailRule
parseFixtureFail spec = case T.splitOn "=" spec of
  [lhs, rhs] -> do
    let (stepTxt, nthTxt) = T.breakOn ":" lhs
    st <- maybe (Left ("unknown step: " <> stepTxt)) Right (lookup stepTxt steps)
    nth <- case T.stripPrefix ":" nthTxt of
      Nothing -> Right Nothing
      Just t -> case readInt t of
        Just n | n >= 1 -> Right (Just n)
        _ -> Left ("call number must be a positive integer: " <> t)
    status <- case rhs of
      "conn" -> Right FailConn
      _ -> case readInt rhs of
        Just c | c >= 100 && c <= 599 && (c < 200 || c >= 300) -> Right (FailHttp c)
        _ -> Left ("status must be a non-2xx HTTP code or conn: " <> rhs)
    pure (FailRule st nth status)
  _ -> Left ("expected STEP[:N]=STATUS: " <> spec)
  where
    steps = [("fetch", StepFetch), ("archive", StepArchive), ("set-reminders", StepSetReminders), ("remove-reminders", StepRemoveReminders)]
