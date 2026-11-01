{-# LANGUAGE OverloadedStrings #-}

module HeadlessTests (headlessTests) where

import Brick.BChan (readBChan)
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, check, modifyTVar', readTVarIO, writeTVar)
import Control.Lens (ix, (^.), (^?))
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Time (UTCTime, utc)
import Headless
import Interpret (Env (..), injectMsg, newEnv, postMsg, tookMsg)
import Network.Bookmark.Agent.Protocol (FlagAction (FlagArchive))
import Network.Bookmark.Agent.Snapshot (AgentSnapshot (..), SnapshotItem (..))
import Network.Bookmark.App.Msg (Intent (..), Msg (..), Now (..), Target (..))
import Network.Bookmark.Fake (FakeRaindrop, fakePort, frItems, seedFake)
import Network.Bookmark.Port (RaindropPort (..), SystemPort (..))
import Network.Bookmark.Types (BookmarkItemId (..), URL (..), biCollectionId)
import Network.Bookmark.Ui.State (AsyncOp (OpFetchItems), hsStatus, initialState)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

headlessTests :: TestTree
headlessTests = testGroup "headless loop" [idleTests, injectTests, busyTests, loopTests]

-- | A message in hand is not idle, though nothing is pending and no slot is
-- taken: the browser blocks inside the step.
busyTests :: TestTree
busyTests =
  testCase "the loop is not idle while it runs a step" $ do
    entered <- newEmptyMVar
    release <- newEmptyMVar
    store <- seedStore
    let blocking = SystemPort (\_ _ -> putMVar entered () >> takeMVar release) (\_ -> pure ())
    env <- newEnv (fakePort (pure t0) store) blocking (pure (Now t0 utc))
    loop <- newLoop
    withAsync (runHeadless env loop initialState) $ \running -> do
      waitIdle env loop
      postMsg env (Intent (Open (ById (BookmarkItemId "1001"))))
      takeMVar entered
      atomically (loopIdle env loop) >>= (@?= False)
      putMVar release ()
      waitIdle env loop
      postMsg env (Intent Quit)
      timeout 5000000 (wait running) >>= maybe (assertFailure "Quit did not end the loop") (const (pure ()))

seedStore :: IO (IORef FakeRaindrop)
seedStore = LBS.readFile "test/e2e/seed-basic.json" >>= either fail newIORef . seedFake 999

waitIdle :: Env -> Loop -> IO ()
waitIdle env loop = timeout 5000000 (atomically (loopIdle env loop >>= check)) >>= maybe (assertFailure "the loop never went idle") pure

-- | A write the full channel refused is not counted as pending.
injectTests :: TestTree
injectTests =
  testCase "a refused injection leaves the pending count as it was" $ do
    (env, _) <- testEnv (RaindropPort (\_ -> fail "no Raindrop here"))
    accepted <- traverse (const (injectMsg env (Intent Refresh))) [1 .. 11 :: Int]
    (length (filter id accepted), last accepted) @?= (10, False)
    readTVarIO (envPending env) >>= (@?= 10)

t0 :: UTCTime
t0 = read "2026-11-01 12:00:00 UTC"

-- | An Env over this port whose desktop records its calls.
testEnv :: RaindropPort -> IO (Env, IORef [String])
testEnv port = do
  calls <- newIORef []
  let record c = modifyIORef' calls (<> [c])
      system = SystemPort (\program (URL url) -> record (program <> " " <> url)) (record . ("copy " <>))
  env <- newEnv port system (pure (Now t0 utc))
  pure (env, calls)

idleTests :: TestTree
idleTests =
  testCase "idle takes an empty queue, no message in hand and a free slot" $ do
    (env, _) <- testEnv (RaindropPort (\_ -> fail "no Raindrop here"))
    loop <- newLoop
    let idle = atomically (loopIdle env loop)
    idle >>= (@?= False) -- in hand: the startup refresh
    atomically (writeTVar (loopBusy loop) False)
    idle >>= (@?= True)
    -- A worker's terminal message, posted but not yet taken.
    postMsg env (FetchCompleted True [])
    idle >>= (@?= False)
    _ <- readBChan (envEvents env)
    idle >>= (@?= False) -- taken, not yet counted off
    atomically (tookMsg env)
    idle >>= (@?= True)
    atomically (modifyTVar' (loopSnapshot loop) (\snap -> snap {asAsyncOp = Just OpFetchItems}))
    idle >>= (@?= False)

loopTests :: TestTree
loopTests =
  testCase "the TUI's path without brick: refresh, flag, execute, open, quit" $ do
    store <- seedStore
    (env, calls) <- testEnv (fakePort (pure t0) store)
    loop <- newLoop
    let post = postMsg env . Intent
        settled = waitIdle env loop
        visible = map siId . filter siVisible . asItems <$> readTVarIO (loopSnapshot loop)
        a = BookmarkItemId "1001"
    withAsync (runHeadless env loop initialState) $ \running -> do
      settled
      visible >>= (@?= map BookmarkItemId ["1001", "1002", "1004"])
      post (SetFlag a FlagArchive (Now t0 utc))
      post Execute
      settled
      fr <- readIORef store
      fr ^? frItems . ix a . biCollectionId @?= Just 999
      visible >>= (@?= map BookmarkItemId ["1002", "1004"])
      post (Open (ById (BookmarkItemId "1002")))
      post Quit
      final <- timeout 5000000 (wait running) >>= maybe (assertFailure "Quit did not end the loop") pure
      readIORef calls >>= (@?= ["firefox https://example.com/1002"])
      final ^. hsStatus @?= Nothing
