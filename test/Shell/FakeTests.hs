{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Shell.FakeTests (fakeTests) where

import Control.Exception (try)
import Control.Lens
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (..), addUTCTime, defaultTimeLocale, formatTime)
import Gen (genFake, genUTCTime)
import Hedgehog (Gen, Property, annotateShow, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hocket.Adapter.Fake
import Hocket.Adapter.Raindrop (ErrorScope (..), classify)
import Hocket.Domain.Bookmark
import Hocket.Port (RaindropPort (..))
import Servant.Client.Core (ClientError (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

archive :: Int
archive = 4711

t0, t1 :: UTCTime
t0 = read "2026-10-01 12:00:00 UTC"
t1 = read "2026-10-02 08:00:00 UTC"

item :: Text -> Int -> UTCTime -> BookmarkItem
item i coll lu = BookmarkItem (BookmarkItemId i) ("https://x/" <> i) "" "" "link" [] False lu lu "x" i 0 [] coll False Nothing

-- | Unsorted 1–3, archived 4, elsewhere 5; 3 and 5 updated on 2026-10-02.
store :: FakeRaindrop
store =
  emptyFake archive
    & frPageSize .~ 2
    & frItems
      .~ Map.fromList
        [ (i ^. biId, i)
        | i <-
            [ item "1" (-1) t0,
              item "2" (-1) t0,
              item "3" (-1) t1,
              item "4" archive t0,
              item "5" 99 t1
            ]
        ]

now :: UTCTime
now = read "2026-11-01 09:00:00 UTC"

serve :: BookmarkRequest a -> FakeRaindrop -> (Either ClientError a, FakeRaindrop)
serve = serveFake now

fetch :: Integer -> Text -> Maybe Text -> FakeRaindrop -> Either ClientError (Integer, [Text])
fetch page coll search fr =
  bimap toInteger (map (^. biId . _BookmarkItemId))
    <$> fst (serve (RetrieveBookmarks (fromInteger page) (RaindropCollectionId coll) search) fr)

itemAt :: Text -> FakeRaindrop -> Maybe BookmarkItem
itemAt i fr = fr ^. frItems . at (BookmarkItemId i)

scopeOf :: Either ClientError a -> Maybe ErrorScope
scopeOf = either (Just . classify) (const Nothing)

isConn :: Either ClientError a -> Bool
isConn (Left (ConnectionError _)) = True
isConn _ = False

withRule :: Text -> FakeRaindrop -> FakeRaindrop
withRule spec fr = either (error . T.unpack) (\r -> fr & frRules %~ (<> [r])) (parseFixtureFail spec)

fakeTests :: TestTree
fakeTests = testGroup "Fake Raindrop" [requestTests, failureTests, parserTests, seedTests, emptySeedTest, dumpTest, portTests, fakeProperties]

requestTests :: TestTree
requestTests =
  testGroup
    "requests"
    [ testCase "fetch -1 is Unsorted, paged, count is the total" $ do
        fetch 0 "-1" Nothing store @?= Right (3, ["3", "1"])
        fetch 1 "-1" Nothing store @?= Right (3, ["2"]),
      testCase "fetch past the last page is empty, count unchanged" $
        fetch 2 "-1" Nothing store @?= Right (3, []),
      testCase "fetch 0 is every collection" $
        fmap (sort . concatMap snd) (sequence [fetch p "0" Nothing store | p <- [0, 1, 2]]) @?= Right ["1", "2", "3", "4", "5"],
      testCase "fetch of one collection" $
        fetch 0 "4711" Nothing store @?= Right (1, ["4"]),
      testCase "lastUpdate:> keeps items updated on a later day" $
        fetch 0 "0" (Just "lastUpdate:>2026-10-01") store @?= Right (2, ["3", "5"]),
      testCase "lastUpdate:> the same day excludes it" $
        fetch 0 "0" (Just "lastUpdate:>2026-10-02") store @?= Right (0, []),
      testCase "an unsupported search is a 400" $
        scopeOf (fst (serve (RetrieveBookmarks 0 (RaindropCollectionId "0") (Just "#tag")) store)) @?= Just ItemScoped,
      testCase "archive one moves it and bumps lastUpdate" $ do
        let (r, fr) = serve (ArchiveBookmark (BookmarkItemId "1")) store
        r @?= Right True
        fmap (\i -> (i ^. biCollectionId, i ^. biLastUpdate)) (itemAt "1" fr) @?= Just (archive, now),
      testCase "archive of an unknown id is an item-scoped 404" $ do
        let (r, fr) = serve (ArchiveBookmark (BookmarkItemId "404")) store
        scopeOf r @?= Just ItemScoped
        (fr ^. frItems) @?= (store ^. frItems),
      testCase "batch archive moves the known ids and ignores unknown ones" $ do
        let (r, fr) = serve (BatchArchiveBookmarks (map BookmarkItemId ["1", "3", "404"])) store
        r @?= Right True
        map (fmap (^. biCollectionId) . (`itemAt` fr)) ["1", "2", "3"] @?= [Just archive, Just (-1), Just archive]
        Map.size (fr ^. frItems) @?= 5,
      testCase "set reminder patches and bumps" $ do
        let (r, fr) = serve (SetReminder (BookmarkItemId "2") t1) store
        r @?= Right True
        fmap (\i -> (i ^. biReminder, i ^. biLastUpdate)) (itemAt "2" fr) @?= Just (Just t1, now),
      testCase "remove reminder clears and bumps" $ do
        let withRem = store & frItems . ix (BookmarkItemId "2") . biReminder ?~ t1
            (r, fr) = serve (RemoveReminder (BookmarkItemId "2")) withRem
        r @?= Right True
        fmap (\i -> (i ^. biReminder, i ^. biLastUpdate)) (itemAt "2" fr) @?= Just (Nothing, now),
      testCase "reminder calls on unknown ids are 404s" $ do
        scopeOf (fst (serve (SetReminder (BookmarkItemId "404") t1) store)) @?= Just ItemScoped
        scopeOf (fst (serve (RemoveReminder (BookmarkItemId "404")) store)) @?= Just ItemScoped,
      testCase "add puts a new id into Unsorted" $ do
        let (r, fr) = serve (AddBookmark "https://new" Nothing ["t"]) store
        r @?= Right (Just (BookmarkItemId "6"))
        fmap (\i -> (i ^. biCollectionId, i ^. biLink, i ^. biCreated)) (itemAt "6" fr) @?= Just (-1, "https://new", now)
    ]

failureTests :: TestTree
failureTests =
  testGroup
    "failure rules"
    [ testCase "archive=503 fails every archive call, globally" $ do
        let fr = withRule "archive=503" store
            (r1, fr1) = serve (ArchiveBookmark (BookmarkItemId "1")) fr
            (r2, _) = serve (BatchArchiveBookmarks [BookmarkItemId "2"]) fr1
        map scopeOf [r1, r2] @?= [Just Global, Just Global]
        (fr1 ^. frItems) @?= (store ^. frItems),
      testCase "set-reminders:2=404 fails only the second call" $ do
        let fr = withRule "set-reminders:2=404" store
            (r1, fr1) = serve (SetReminder (BookmarkItemId "1") t1) fr
            (r2, fr2) = serve (SetReminder (BookmarkItemId "2") t1) fr1
            (r3, _) = serve (SetReminder (BookmarkItemId "3") t1) fr2
        map scopeOf [r1, r2, r3] @?= [Nothing, Just ItemScoped, Nothing]
        fmap (^. biReminder) (itemAt "2" fr2) @?= Just Nothing,
      testCase "fetch=conn is a connection error" $
        isConn (fst (serve (RetrieveBookmarks 0 (RaindropCollectionId "-1") Nothing) (withRule "fetch=conn" store))) @?= True,
      testCase "remove-reminders:1=429 is global and other steps are unaffected" $ do
        let fr = withRule "remove-reminders:1=429" store
        scopeOf (fst (serve (RemoveReminder (BookmarkItemId "1")) fr)) @?= Just Global
        scopeOf (fst (serve (SetReminder (BookmarkItemId "1") t1) fr)) @?= Nothing,
      testCase "add has no step and never fails" $
        isLeft (fst (serve (AddBookmark "https://new" Nothing []) (foldr withRule store ["fetch=500", "archive=500", "set-reminders=500", "remove-reminders=500"]))) @?= False
    ]

parserTests :: TestTree
parserTests =
  testGroup
    "parseFixtureFail"
    [ testCase "valid specs" $
        map
          parseFixtureFail
          ["archive=503", "set-reminders:2=404", "fetch=conn", "remove-reminders:10=429"]
          @?= map
            Right
            [ FailRule StepArchive Nothing (FailHttp 503),
              FailRule StepSetReminders (Just 2) (FailHttp 404),
              FailRule StepFetch Nothing FailConn,
              FailRule StepRemoveReminders (Just 10) (FailHttp 429)
            ],
      testCase "invalid specs are rejected" $
        mapM_
          (\s -> either (const (pure ())) (\r -> assertFailure (show s <> " parsed as " <> show r)) (parseFixtureFail s))
          ["add=500", "archive=200", "archive=299", "archive:0=500", "archive:x=500", "archive=99", "archive=600", "archive=oops", "archive", "archive=1=2", ""]
    ]

seedTests :: TestTree
seedTests =
  testCase "seed from a Raindrop list response" $ do
    body <- LBS.readFile "test/raindrop-items1.json"
    case seedFake archive body of
      Left e -> assertFailure e
      Right fr -> do
        Map.size (fr ^. frItems) @?= 18
        all ((== (-1)) . (^. biCollectionId)) (fr ^. frItems) @?= True

dumpTest :: TestTree
dumpTest =
  testCase "a dump of the seed reads back to the same items" $ do
    body <- LBS.readFile "test/raindrop-items1.json"
    case seedFake archive body of
      Left e -> assertFailure e
      Right fr -> do
        Map.size (fr ^. frItems) @?= 18
        fmap (^. frItems) (seedFake archive (encodeFixtureDump fr)) @?= Right (fr ^. frItems)

-- | A port over a fresh copy of 'store' whose clock ticks t0+1s, t0+2s, …
newPort :: FakeRaindrop -> IO (RaindropPort, IORef FakeRaindrop)
newPort fr = do
  ref <- newIORef fr
  ticks <- newIORef (0 :: Int)
  let clock = atomicModifyIORef' ticks (\n -> (n + 1, addUTCTime (fromIntegral (n + 1)) t0))
  pure (fakePort clock ref, ref)

-- | The port answers and stores exactly what 'serveFake' does at tick 1.
samePort :: (Eq a, Show a) => BookmarkRequest a -> Assertion
samePort req = do
  (port, ref) <- newPort store
  let (expected, fr') = serveFake (addUTCTime 1 t0) req store
  v <- either (assertFailure . show) pure expected
  runRaindrop port req >>= (@?= v)
  readIORef ref >>= (@?= fr')

portTests :: TestTree
portTests =
  testGroup
    "fakePort"
    [ testCase "every request answers like serveFake" $ do
        samePort (AddBookmark "https://new" Nothing [])
        samePort (ArchiveBookmark (BookmarkItemId "1"))
        samePort (BatchArchiveBookmarks (map BookmarkItemId ["1", "2"]))
        samePort (SetReminder (BookmarkItemId "2") t1)
        samePort (RemoveReminder (BookmarkItemId "2"))
        samePort (RetrieveBookmarks 0 (RaindropCollectionId "0") Nothing),
      testCase "a failure rule throws a ClientError with its scope; only frCalls changes" $ do
        let failing = withRule "set-reminders=503" store
        (port, ref) <- newPort failing
        r <- try @ClientError (runRaindrop port (SetReminder (BookmarkItemId "1") t1))
        either (Just . classify) (const Nothing) r @?= Just Global
        readIORef ref >>= (@?= (failing & frCalls . at StepSetReminders ?~ 1)),
      testCase "an item error throws an item-scoped ClientError" $ do
        (port, _) <- newPort store
        r <- try @ClientError (runRaindrop port (ArchiveBookmark (BookmarkItemId "404")))
        either (Just . classify) (const Nothing) r @?= Just ItemScoped,
      testCase "writes are stamped with the logical clock and see each other" $ do
        (port, ref) <- newPort store
        _ <- runRaindrop port (SetReminder (BookmarkItemId "1") t1)
        _ <- runRaindrop port (ArchiveBookmark (BookmarkItemId "1"))
        fr <- readIORef ref
        fmap (\i -> (i ^. biReminder, i ^. biCollectionId, i ^. biLastUpdate)) (itemAt "1" fr)
          @?= Just (Just t1, archive, addUTCTime 2 t0)
    ]

-- | The B1 E2E seed: an empty account.
emptySeedTest :: TestTree
emptySeedTest =
  testCase "seed from an empty list response" $
    case seedFake archive "{\"items\":[],\"count\":0}" of
      Left e -> assertFailure e
      Right fr -> do
        Map.null (fr ^. frItems) @?= True
        fetch 0 "-1" Nothing fr @?= Right (0, [])

-- | Page from 0 until the first empty page, like the real fetch loop.
fetchAll :: Text -> Maybe Text -> FakeRaindrop -> Either ClientError [BookmarkItem]
fetchAll coll search fr = go 0
  where
    go p = case fst (serveFake now (RetrieveBookmarks p (RaindropCollectionId coll) search) fr) of
      Left e -> Left e
      Right (_, []) -> Right []
      Right (_, is) -> (is <>) <$> go (p + 1)

ids :: [BookmarkItem] -> [BookmarkItemId]
ids = sort . map (^. biId)

genDay :: Gen Day
genDay = utctDay <$> genUTCTime

fakeProperties :: TestTree
fakeProperties =
  testGroup
    "properties"
    [ testProperty "archiving moves exactly the requested ids" $ property $ do
        fr <- forAll genFake
        req <- forAll (Gen.subsequence (Map.keys (fr ^. frItems)))
        at' <- forAll genUTCTime
        let (r, fr') = serveFake at' (BatchArchiveBookmarks req) fr
            expected = Map.mapWithKey (\k i -> if k `elem` req then i & biCollectionId .~ archive & biLastUpdate .~ at' else i) (fr ^. frItems)
        r === Right True
        fr' ^. frItems === expected,
      testProperty "lastUpdate:> returns exactly the items updated on a later day" $ property $ do
        fr <- forAll genFake
        let days = map (utctDay . view biLastUpdate) (Map.elems (fr ^. frItems))
        -- Item days hit the boundary, which random days almost never do.
        d <- forAll (if null days then genDay else Gen.choice [genDay, Gen.element days])
        let search = "lastUpdate:>" <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d" d)
        fmap ids (fetchAll "0" (Just search) fr) === Right (ids (filter (\i -> utctDay (i ^. biLastUpdate) > d) (Map.elems (fr ^. frItems)))),
      testProperty "paging returns every item exactly once" $ property $ do
        fr <- forAll genFake
        fmap ids (fetchAll "0" Nothing fr) === Right (Map.keys (fr ^. frItems)),
      testProperty "a failed call leaves the store unchanged but counted" failedCallUnchanged,
      testProperty "a dump seeds the same items" $ property $ do
        fr <- forAll genFake
        fmap (^. frItems) (seedFake archive (encodeFixtureDump fr)) === Right (fr ^. frItems)
    ]

failedCallUnchanged :: Property
failedCallUnchanged = property $ do
  fr <- forAll genFake
  status <- forAll (Gen.choice [pure FailConn, FailHttp <$> Gen.element [400, 404, 429, 500, 503]])
  bid <- forAll (Gen.element (BookmarkItemId "1" : Map.keys (fr ^. frItems)))
  t <- forAll genUTCTime
  which <- forAll (Gen.element [minBound .. maxBound])
  let failing = fr & frRules .~ [FailRule which Nothing status]
      check :: BookmarkRequest a -> (Bool, FakeRaindrop)
      check req = let (r, fr') = serveFake (addUTCTime 1 t) req failing in (isLeft r, fr')
      (failed, after) = case which of
        StepFetch -> check (RetrieveBookmarks 0 (RaindropCollectionId "0") Nothing)
        StepArchive -> check (BatchArchiveBookmarks [bid])
        StepSetReminders -> check (SetReminder bid t)
        StepRemoveReminders -> check (RemoveReminder bid)
  annotateShow (after ^. frCalls)
  assert failed
  after === (failing & frCalls . at which ?~ 1)
