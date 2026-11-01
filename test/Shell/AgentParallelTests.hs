{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | SM-agent (plan I10): two agents write to one headless hocket over the
-- real socket at once, and some order of their calls explains every answer.
-- No write is lost or applied twice (the flags equal the model's after any
-- linearisation), a write lands or is answered "channel full" and retried,
-- versions never go back, and the slot is free once the loop is idle.
--
-- Every write here is valid in every state: the items have no reminders and
-- nothing executes, so no write can be rejected by a race with an archive.
-- After a write, the harness waits until the channel is drained, so a later
-- read sees it: the socket only promises that a write was injected.
module Shell.AgentParallelTests (agentParallelTests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar)
import qualified Control.Concurrent.STM as STM
import Control.Lens ((&), (.~))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Control (control)
import Data.Aeson (Value, withObject, (.:))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, utc)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hocket.Adapter.Fake (fakePort, frItems, seedFake)
import Hocket.Agent.Protocol (AgentCmd (..), FlagAction (..), ReadCmd (..), WriteCmd (..))
import Hocket.App.Model (initialModel)
import Hocket.App.Msg (Now (..))
import Hocket.Domain.Bookmark (BookmarkItemId (..), biReminder)
import Hocket.Port (SystemPort (..))
import Hocket.Shell.Agent.Client (callAgent, isChannelFullError)
import Hocket.Shell.Agent.Server (withAgentServer)
import Hocket.Shell.Headless (Loop (..), loopIdle, runHeadless)
import Hocket.Shell.Interpret (Env (..), newEnv)
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testPropertyNamed)

agentParallelTests :: TestTree
agentParallelTests = testPropertyNamed "SM-agent: concurrent agents on one socket (I10)" "smAgent" smAgent

t0 :: UTCTime
t0 = read "2026-11-01 12:00:00 UTC"

ids :: [Text]
ids = ["1001", "1002", "1003", "1004"]

-- | A running headless hocket with its socket.
data Sut = Sut {sutSocket :: FilePath, sutEnv :: Env, sutLoop :: Loop}

-- | seed-basic without its one reminder, so every flag is a valid write.
withSut :: (Sut -> IO a) -> IO a
withSut act = withSystemTempDirectory "hocket-sm" $ \dir -> do
  body <- LBS.readFile "test/e2e/seed-basic.json"
  seed <- either fail pure (seedFake 999 body)
  store <- newIORef (seed & frItems . traverse . biReminder .~ Nothing)
  ticks <- newIORef (0 :: Int)
  let clock = atomicModifyIORef' ticks (\n -> (n + 1, addUTCTime (fromIntegral (n + 1)) t0))
  env <- newEnv (fakePort clock store) (SystemPort (\_ _ -> pure ()) (\_ -> pure ())) (pure (Now t0 utc))
  createDirectory (dir </> "sock")
  let path = dir </> "sock" </> "s.sock"
  withAgentServer env path $ \snapVar -> do
    loop <- Loop snapVar <$> newTVarIO True
    withAsync (runHeadless env loop initialModel) $ \_ -> do
      -- The server binds in its own thread; the loop starts with a refresh.
      up <- timeout 10000000 (untilRight (callAgent path (ARead CmdGetState)))
      idle <- timeout 10000000 (atomically (loopIdle env loop >>= STM.check))
      maybe (fail "no socket or no idle loop within 10 s") (const (act (Sut path env loop))) (up >> idle)
  where
    untilRight call = call >>= either (const (threadDelay 20000 >> untilRight call)) (const (pure ()))

-- | The flag of every item, by id, and the last version an answer showed.
data AgentModel (v :: Type -> Type) = AgentModel
  { amFlags :: !(Map Text Text),
    amVersion :: !(Maybe (Var Int v))
  }

initialAgentModel :: AgentModel v
initialAgentModel = AgentModel (Map.fromList [(i, "none") | i <- ids]) Nothing

data Op
  = OpSetFlag !Text !Text
  | OpClearFlags
  | OpSetFilter !Text
  | OpRefresh
  deriving (Show)

newtype Write (v :: Type -> Type) = Write Op deriving (Show)

instance FunctorB Write where bmap _ (Write o) = Write o

instance TraversableB Write where btraverse _ (Write o) = pure (Write o)

data Query (v :: Type -> Type) = GetFlags | GetState deriving (Show)

instance FunctorB Query where
  bmap _ = \case GetFlags -> GetFlags; GetState -> GetState

instance TraversableB Query where
  btraverse _ = \case GetFlags -> pure GetFlags; GetState -> pure GetState

sutOf :: IORef (Maybe Sut) -> IO Sut
sutOf ref = readIORef ref >>= maybe (fail "SM-agent: no system under test") pure

-- | A write until it is accepted, then until the loop has applied it.
write :: Sut -> Op -> IO (Maybe Bool)
write sut op = do
  r <- callAgent (sutSocket sut) (AWrite (toCmd op))
  case r of
    Right v | isChannelFullError v -> threadDelay 2000 >> write sut op
    Right v -> do
      drained <- timeout 10000000 . atomically $ do
        pending <- readTVar (envPending (sutEnv sut))
        busy <- readTVar (loopBusy (sutLoop sut))
        STM.check (pending == 0 && not busy)
      pure (okOf v <* drained)
    Left _ -> pure Nothing
  where
    toCmd = \case
      OpSetFlag i a -> CmdSetFlag (BookmarkItemId i) (flagOf a)
      OpClearFlags -> CmdClearFlags
      OpSetFilter q -> CmdSetFilter q
      OpRefresh -> CmdRefresh
    flagOf = \case "archive" -> FlagArchive; "reminder" -> FlagReminder; _ -> FlagNone

okOf :: Value -> Maybe Bool
okOf = parseMaybe (withObject "response" (.: "ok"))

-- | The flag writes get commands of their own, so that they are common: a
-- lost write only shows when a read follows it.
setFlags :: IORef (Maybe Sut) -> Command Gen (TestT IO) AgentModel
setFlags ref = writes ref (OpSetFlag <$> Gen.element ids <*> Gen.element ["archive", "reminder", "none"])

clearFlags :: IORef (Maybe Sut) -> Command Gen (TestT IO) AgentModel
clearFlags ref = writes ref (pure OpClearFlags)

otherWrites :: IORef (Maybe Sut) -> Command Gen (TestT IO) AgentModel
otherWrites ref = writes ref (Gen.choice [OpSetFilter <$> Gen.element ["", "1001", "e2e", "zzzz"], pure OpRefresh])

writes :: IORef (Maybe Sut) -> Gen Op -> Command Gen (TestT IO) AgentModel
writes ref gen =
  Command
    (\_ -> Just (Write <$> gen))
    (\(Write op) -> evalIO (sutOf ref >>= (`write` op)))
    [ Update $ \m (Write op) _ -> case op of
        OpSetFlag i a -> m {amFlags = Map.insert i a (amFlags m)}
        OpClearFlags -> m {amFlags = "none" <$ amFlags m}
        _ -> m,
      Ensure $ \_ _ _ landed -> landed === Just True
    ]

-- | Every item's flag, all items, visible or not.
getFlags :: IORef (Maybe Sut) -> Command Gen (TestT IO) AgentModel
getFlags ref =
  Command
    (\_ -> Just (pure GetFlags))
    ( \_ -> do
        r <- evalIO (sutOf ref >>= \sut -> callAgent (sutSocket sut) (ARead (CmdListItems False False)))
        v <- evalEither r
        evalMaybe (parseMaybe flags v)
    )
    [Ensure $ \before _ _ got -> got === amFlags before]
  where
    flags = withObject "response" $ \o -> do
      items <- o .: "result"
      Map.fromList <$> traverse (withObject "item" (\i -> (,) <$> i .: "id" <*> (i .: "pending" >>= withObject "pending" (.: "action")))) items

getState :: IORef (Maybe Sut) -> Command Gen (TestT IO) AgentModel
getState ref =
  Command
    (\_ -> Just (pure GetState))
    ( \_ -> do
        r <- evalIO (sutOf ref >>= \sut -> callAgent (sutSocket sut) (ARead CmdGetState))
        v <- evalEither r
        evalMaybe (parseMaybe (withObject "response" (\o -> o .: "result" >>= withObject "state" (.: "version"))) v)
    )
    [ Update $ \m _ v -> m {amVersion = Just v},
      Ensure $ \before _ _ v -> for_ (amVersion before) (\seen -> assert (v >= concrete seen))
    ]

smAgent :: Property
smAgent = withTests 100 . withShrinks 20 . property $ do
  ref <- evalIO (newIORef Nothing)
  -- Hedgehog picks a command uniformly: listing flags and reads twice makes
  -- them common. The prefix is sequential and cheap, so it is long; the
  -- branches are short, as their interleavings multiply.
  let commands = [setFlags ref, setFlags ref, clearFlags ref, otherWrites ref, getFlags ref, getFlags ref, getState ref]
  actions <- forAll (Gen.parallel (Range.linear 1 20) (Range.linear 1 6) initialAgentModel commands)
  test (control (\run -> withSut (run . scenario ref actions)))
  where
    scenario :: IORef (Maybe Sut) -> Parallel (TestT IO) AgentModel -> Sut -> TestT IO ()
    scenario ref actions sut = do
      evalIO (writeIORef ref (Just sut))
      executeParallel initialAgentModel actions
      idle <- evalIO (timeout 10000000 (atomically (loopIdle (sutEnv sut) (sutLoop sut) >>= STM.check)))
      annotate "the loop did not go idle within 10 s: a write stuck or the slot held"
      idle === Just ()
