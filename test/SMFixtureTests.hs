{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | SM-fixture (plan I9): the IORef-backed 'fakePort' behaves exactly like
-- the pure 'serveFake' it wraps. The model is the pure store plus the clock
-- ticks; no symbolic Vars, because the model reproduces every id the SUT
-- returns (AddBookmark included), so later commands draw ids from it.
module SMFixtureTests (smFixtureTests) where

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
import Network.Bookmark.Fake
import Network.Bookmark.Port (RaindropPort (..))
import Network.Bookmark.Types
import Network.Raindrop (ErrorScope, classify)
import Servant.Client.Core (ClientError)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testPropertyNamed)

t0 :: UTCTime
t0 = read "2026-11-01 00:00:00 UTC"

-- | The store, and how many times the clock has been read so far.
data Model (v :: Type -> Type) = Model !FakeRaindrop !Int

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
predict :: SomeRequest -> Model v -> (Outcome, Model v)
predict (SomeRequest req) (Model fr ticks) =
  let now = addUTCTime (fromIntegral (ticks + 1)) t0
      (res, fr') = serveFake now req fr
   in (outcome res, Model fr' (ticks + 1))

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

call :: RaindropPort -> IORef FakeRaindrop -> Command Gen (PropertyT IO) Model
call port ref =
  Command
    (\(Model fr _) -> Just (Call <$> genRequest fr))
    ( \(Call (SomeRequest req)) -> do
        r <- liftIO (try (runRaindrop port req))
        store <- liftIO (readIORef ref)
        pure (outcome r, store)
    )
    [ Update (\m (Call r) _ -> snd (predict r m)),
      Ensure $ \before (Model expectedStore _) (Call r) (got, store) -> do
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
  actions <- forAll (Gen.sequential (Range.linear 1 40) (Model start 0) [call port ref])
  -- Shrinking reruns from here: every run starts from the seed.
  liftIO (writeIORef ref start >> writeIORef ticks 0)
  executeSequential (Model start 0) actions

smFixtureTests :: TestTree
smFixtureTests = testPropertyNamed "SM-fixture: fakePort behaves like serveFake (I9)" "smFixture" smFixture
