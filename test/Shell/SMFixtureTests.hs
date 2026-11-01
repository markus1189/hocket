{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | SM-fixture (plan I9): the IORef-backed 'fakePort' behaves exactly like
-- the pure 'serveFake' it wraps. The model is the pure store plus the clock
-- ticks; no symbolic Vars, because the model reproduces every id the SUT
-- returns (AddBookmark included), so later commands draw ids from it.
module Shell.SMFixtureTests (smFixtureTests) where

import Control.Exception (try)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, utctDay)
import Gen (genFailRule, genFake, genUTCTime)
import Hedgehog hiding (classify)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hocket.Adapter.Fake
import Hocket.Adapter.Raindrop (ErrorScope, classify)
import Hocket.Domain.Bookmark
import Hocket.Port (RaindropPort (..))
import Servant.Client.Core (ClientError)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testPropertyNamed)

t0 :: UTCTime
t0 = read "2026-11-01 00:00:00 UTC"

-- | The store, and how many times the clock has been read so far.
data FixtureModel (v :: Type -> Type) = FixtureModel !FakeRaindrop !Int

-- | Any request, with what is needed to compare its answers.
data SomeRequest = forall a. (Show a) => SomeRequest (BookmarkRequest a)

instance Show SomeRequest where
  show (SomeRequest r) = case r of
    AddBookmark l c ts -> "AddBookmark " <> show l <> " " <> show c <> " " <> show ts
    ArchiveBookmark i -> "ArchiveBookmark " <> show i
    BatchArchiveBookmarks is -> "BatchArchiveBookmarks " <> show is
    SetReminder i t -> "SetReminder " <> show i <> " " <> show t
    RemoveReminder i -> "RemoveReminder " <> show i
    RetrieveBookmarks p c s -> "RetrieveBookmarks " <> show p <> " " <> show c <> " " <> show s

newtype Call (v :: Type -> Type) = Call SomeRequest
  deriving (Show)

instance FunctorB Call where bmap _ (Call r) = Call r

instance TraversableB Call where btraverse _ (Call r) = pure (Call r)

-- | An answer as comparable data: the value, or the error's scope and text.
type Outcome = Either (ErrorScope, String) String

outcome :: (Show a) => Either ClientError a -> Outcome
outcome = either (\e -> Left (classify e, show e)) (Right . show)

-- | What the pure fake answers at the model's next clock reading.
predict :: SomeRequest -> FixtureModel v -> (Outcome, FixtureModel v)
predict (SomeRequest req) (FixtureModel fr ticks) =
  let now = addUTCTime (fromIntegral (ticks + 1)) t0
      (res, fr') = serveFake now req fr
   in (outcome res, FixtureModel fr' (ticks + 1))

genRequest :: FakeRaindrop -> Gen SomeRequest
genRequest fr =
  Gen.choice
    [ SomeRequest <$> (RetrieveBookmarks <$> Gen.integral (Range.linear 0 3) <*> genColl <*> Gen.maybe genSearch),
      SomeRequest . BatchArchiveBookmarks <$> Gen.subsequence (BookmarkItemId "999999" : known),
      SomeRequest . ArchiveBookmark <$> genId,
      SomeRequest <$> (SetReminder <$> genId <*> genUTCTime),
      SomeRequest . RemoveReminder <$> genId,
      SomeRequest <$> (AddBookmark . ("https://example.org/" <>) <$> Gen.text (Range.linear 1 6) Gen.alphaNum <*> Gen.element [Nothing, Just "-1", Just "4711"] <*> pure ["t"])
    ]
  where
    known = Map.keys (fr ^. frItems)
    genId = Gen.frequency [(5, if null known then pure (BookmarkItemId "1") else Gen.element known), (1, pure (BookmarkItemId "999999"))]
    genColl = RaindropCollectionId <$> Gen.element ["-1", "0", "4711", "99"]
    days = map (utctDay . (^. biLastUpdate)) (Map.elems (fr ^. frItems))
    genSearch = do
      d <- if null days then utctDay <$> genUTCTime else Gen.choice [Gen.element days, utctDay <$> genUTCTime]
      pure ("lastUpdate:>" <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d" d))

-- | What a sequence exercised, for 'cover': an error after earlier counted
-- calls, a fetch whose search day is an item's day, an empty fetch answer.
data Seen = Seen {seenLateError :: !Bool, seenBoundaryDay :: !Bool, seenEmptyPage :: !Bool}

noneSeen :: Seen
noneSeen = Seen False False False

-- | Classified from the store before the call and the typed answer.
observe :: FakeRaindrop -> BookmarkRequest a -> Either ClientError a -> Seen -> Seen
observe before req r seen =
  Seen
    { seenLateError = seenLateError seen || (isLeft' r && any (> 0) (before ^. frCalls)),
      seenBoundaryDay = seenBoundaryDay seen || boundary,
      seenEmptyPage = seenEmptyPage seen || emptyPage
    }
  where
    isLeft' = either (const True) (const False)
    days = map (utctDay . (^. biLastUpdate)) (Map.elems (before ^. frItems))
    (boundary, emptyPage) = case (req, r) of
      (RetrieveBookmarks _ _ search, _) ->
        ( maybe False (\t -> any (\d -> t == "lastUpdate:>" <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d" d)) days) search,
          either (const False) (null . snd) r
        )
      _ -> (False, False)

call :: RaindropPort -> IORef FakeRaindrop -> IORef Seen -> Command Gen (PropertyT IO) FixtureModel
call port ref seen =
  Command
    (\(FixtureModel fr _) -> Just (Call <$> genRequest fr))
    ( \(Call (SomeRequest req)) -> do
        before <- liftIO (readIORef ref)
        r <- liftIO (try (runRaindrop port req))
        liftIO (atomicModifyIORef' seen (\s -> (observe before req r s, ())))
        store <- liftIO (readIORef ref)
        pure (outcome r, store)
    )
    [ Update (\m (Call r) _ -> snd (predict r m)),
      Ensure $ \before (FixtureModel expectedStore _) (Call r) (got, store) -> do
        fst (predict r before) === got
        expectedStore === store
    ]

smFixture :: Property
smFixture = withTests 200 . property $ do
  rules <- forAll (Gen.list (Range.linear 0 3) genFailRule)
  seed <- forAll genFake
  let start = seed & frRules .~ rules
  ref <- liftIO (newIORef start)
  ticks <- liftIO (newIORef (0 :: Int))
  let clock = atomicModifyIORef' ticks (\n -> (n + 1, addUTCTime (fromIntegral (n + 1)) t0))
      port = fakePort clock ref
  seen <- liftIO (newIORef noneSeen)
  actions <- forAll (Gen.sequential (Range.linear 1 40) (FixtureModel start 0) [call port ref seen])
  -- Shrinking reruns from here: every run starts from the seed.
  liftIO (writeIORef ref start >> writeIORef ticks 0 >> writeIORef seen noneSeen)
  executeSequential (FixtureModel start 0) actions
  -- QS measured about 51%, 47% and 55% per sequence.
  Seen lateError boundaryDay emptyPage <- liftIO (readIORef seen)
  cover 35 "an error after earlier counted calls" lateError
  cover 30 "a fetch at an item's boundary day" boundaryDay
  cover 40 "a page past the end / empty answer" emptyPage

smFixtureTests :: TestTree
smFixtureTests = testPropertyNamed "SM-fixture: fakePort behaves like serveFake (I9)" "smFixture" smFixture
