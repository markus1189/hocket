{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | SM-agent (plan I10): two agents write to one headless hocket over the
-- real socket at once, and some order of their calls explains every answer.
--
-- smAgent: no write is lost or applied twice (the flags equal the model's
-- after any linearisation), a write lands or is answered "channel full" and
-- retried, versions never go back, and the slot is free once the loop is
-- idle. Every write is valid in every state (no reminders, no execute), so
-- no rejection can come from a race with an archive. After a write the
-- harness waits until the channel is drained: the socket only promises that
-- a write was injected. The skill's wait_version --after V is not enough
-- here: it returns once any later event was applied, also one queued before
-- the write (with it in place of the drain, this property fails).
--
-- smAgentExecute: agents write while a batch runs. Flags are not modelled;
-- a write lands, is retried on "channel full", or is a set_flag rejected for
-- one of the reasons a concurrent archive or reminder can cause.
module Shell.AgentParallelTests (agentParallelTests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, readTVarIO)
import qualified Control.Concurrent.STM as STM
import Control.Lens ((&), (.~))
import Control.Monad (void)
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
import Hocket.Adapter.Fake (FakeRaindrop, fakePort, frItems, seedFake)
import Hocket.Agent.Protocol (AgentCmd (..), FlagAction (..), ReadCmd (..), WriteCmd (..))
import Hocket.Agent.Snapshot (asAsyncOp)
import Hocket.App.Model (AsyncOp (OpExecuteBatch), initialModel)
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
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

agentParallelTests :: TestTree
agentParallelTests =
  testGroup
    "SM-agent"
    [ testPropertyNamed "concurrent agents on one socket (I10)" "smAgent" smAgent,
      testPropertyNamed "agents writing while a batch runs" "smAgentExecute" smAgentExecute
    ]

t0 :: UTCTime
t0 = read "2026-11-01 12:00:00 UTC"

ids :: [Text]
ids = ["1001", "1002", "1003", "1004"]

-- | A running headless hocket with its socket.
data Sut = Sut {sutSocket :: FilePath, sutEnv :: Env, sutLoop :: Loop}

-- | seed-basic, prepared, behind a fake whose every request takes this many
-- microseconds.
withSut :: (FakeRaindrop -> FakeRaindrop) -> Int -> (Sut -> IO a) -> IO a
withSut prepare delay act = withSystemTempDirectory "hocket-sm" $ \dir -> do
  body <- LBS.readFile "test/e2e/seed-basic.json"
  seed <- either fail pure (seedFake 999 body)
  store <- newIORef (prepare seed)
  ticks <- newIORef (0 :: Int)
  let clock = threadDelay delay >> atomicModifyIORef' ticks (\n -> (n + 1, addUTCTime (fromIntegral (n + 1)) t0))
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
  -- Without its one reminder, every flag is a valid write.
  test (control (\run -> withSut (frItems . traverse . biReminder .~ Nothing) 0 (run . scenario ref actions)))
  where
    scenario :: IORef (Maybe Sut) -> Parallel (TestT IO) AgentModel -> Sut -> TestT IO ()
    scenario ref actions sut = do
      evalIO (writeIORef ref (Just sut))
      executeParallel initialAgentModel actions
      idle <- evalIO (timeout 10000000 (atomically (loopIdle (sutEnv sut) (sutLoop sut) >>= STM.check)))
      annotate "the loop did not go idle within 10 s: a write stuck or the slot held"
      idle === Just ()

-- | What the agents ran into, for the coverage of smAgentExecute.
data Seen = Seen {seenRetry :: !Bool, seenRejection :: !Bool, seenWriteInBatch :: !Bool}

data Answer = Landed | Rejected !Text | Unexpected !Text deriving (Show, Eq)

newtype ExecModel (v :: Type -> Type) = ExecModel (Maybe (Var Int v))

-- | Writes in order, as one command: a flag and an execute make sure the
-- batch has work, which an execute on its own often has not.
newtype Exec (v :: Type -> Type) = Exec [WriteCmd] deriving (Show)

instance FunctorB Exec where bmap _ (Exec ws) = Exec ws

instance TraversableB Exec where btraverse _ (Exec ws) = pure (Exec ws)

-- | A write, retried while the channel is full; no waiting for it to apply.
writeAnswer :: IORef Seen -> Sut -> WriteCmd -> IO Answer
writeAnswer seen sut w = do
  inBatch <- (== Just OpExecuteBatch) . asAsyncOp <$> readTVarIO (loopSnapshot (sutLoop sut))
  note (\s -> s {seenWriteInBatch = seenWriteInBatch s || inBatch})
  r <- callAgent (sutSocket sut) (AWrite w)
  case r of
    Right v | isChannelFullError v -> note (\s -> s {seenRetry = True}) >> threadDelay 1000 >> writeAnswer seen sut w
    Right v | okOf v == Just True -> Landed <$ startedBatch
    Right v -> maybe (pure (Unexpected (T.pack (show v)))) (\e -> Rejected e <$ note (\s -> s {seenRejection = True})) (errorOf v)
    Left e -> pure (Unexpected e)
  where
    note f = atomicModifyIORef' seen (\s -> (f s, ()))
    -- Steers only the timing: after an execute the agent goes on once the
    -- batch runs (or after 100 ms, if update dropped it), so that what it
    -- writes next meets the batch.
    startedBatch = case w of
      CmdExecute -> void (timeout 100000 (atomically (readTVar (loopSnapshot (sutLoop sut)) >>= STM.check . (== Just OpExecuteBatch) . asAsyncOp)))
      _ -> pure ()
    errorOf = parseMaybe (withObject "response" (.: "error"))

-- | What a concurrent archive or reminder can make validateWrite answer.
raceRejection :: Text -> Bool
raceRejection e = any (`T.isPrefixOf` e) ["unknown item id: ", "item already has a reminder", "item has no reminder to remove: "]

execWrites :: IORef (Maybe Sut) -> IORef Seen -> Command Gen (TestT IO) ExecModel
execWrites ref seen =
  Command
    ( \_ ->
        Just . fmap Exec . Gen.frequency $
          [ (4, pure <$> setFlag),
            (3, (\f -> [f, CmdExecute]) <$> setFlag),
            (1, pure [CmdExecute]),
            (1, pure [CmdClearFlags]),
            (1, pure [CmdRefresh])
          ]
    )
    (\(Exec ws) -> evalIO (sutOf ref >>= \sut -> traverse (writeAnswer seen sut) ws))
    [ Ensure $ \_ _ (Exec ws) answers -> for_ (zip ws answers) $ \case
        (_, Landed) -> success
        (CmdSetFlag _ _, Rejected e) | raceRejection e -> success
        other -> annotateShow other >> failure
    ]
  where
    setFlag = CmdSetFlag . BookmarkItemId <$> Gen.element ids <*> Gen.element [FlagArchive, FlagReminder, FlagRemoveReminder, FlagNone]

execState :: IORef (Maybe Sut) -> Command Gen (TestT IO) ExecModel
execState ref =
  Command
    (\_ -> Just (pure GetState))
    ( \_ -> do
        r <- evalIO (sutOf ref >>= \sut -> callAgent (sutSocket sut) (ARead CmdGetState))
        v <- evalEither r
        evalMaybe (parseMaybe (withObject "response" (\o -> o .: "result" >>= withObject "state" (.: "version"))) v)
    )
    [ Update $ \_ _ v -> ExecModel (Just v),
      Ensure $ \(ExecModel before) _ _ v -> for_ before (\seenV -> assert (v >= concrete seenV))
    ]

smAgentExecute :: Property
smAgentExecute = withTests 50 . withShrinks 20 . property $ do
  ref <- evalIO (newIORef Nothing)
  seen <- evalIO (newIORef (Seen False False False))
  let commands = [execWrites ref seen, execWrites ref seen, execState ref]
  actions <- forAll (Gen.parallel (Range.linear 1 10) (Range.linear 1 6) (ExecModel Nothing) commands)
  -- The original seed: 1003 has a reminder. Every request takes 10 ms, so a
  -- batch an agent starts runs on while the agents write.
  test (control (\run -> withSut id 10000 (run . scenario ref seen actions)))
  where
    scenario :: IORef (Maybe Sut) -> IORef Seen -> Parallel (TestT IO) ExecModel -> Sut -> TestT IO ()
    scenario ref seen actions sut = do
      evalIO (writeIORef ref (Just sut) >> writeIORef seen (Seen False False False))
      executeParallel (ExecModel Nothing) actions
      idle <- evalIO (timeout 20000000 (atomically (loopIdle (sutEnv sut) (sutLoop sut) >>= STM.check)))
      annotate "the loop did not go idle within 20 s: a write stuck or the slot held"
      idle === Just ()
      Seen retry rejection inBatch <- evalIO (readIORef seen)
      -- The loop drains faster than two agents fill ten places; the full
      -- channel has its own test in AgentServerTests.
      classify "a write retried on a full channel" retry
      cover 10 "a set_flag rejected by a race" rejection
      cover 10 "a write while a batch ran" inBatch
