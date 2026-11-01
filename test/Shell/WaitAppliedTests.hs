{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Exact write confirmation (plan §12, I11) on a running headless hocket
-- behind the real socket: write seqs, wait_applied, and a version that
-- connects and disconnects leave alone.
module Shell.WaitAppliedTests (waitAppliedTests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently, withAsync)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, check, readTVar, readTVarIO)
import Control.Lens ((&), (.~))
import Control.Monad (forM)
import Data.Aeson (FromJSON, Key, Value, withObject, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Hocket.Adapter.Fake (frItems)
import Hocket.Agent.Protocol (AgentCmd (..), FlagAction (..), ReadCmd (..), WriteCmd (..))
import Hocket.Agent.Snapshot (asAppliedSeq, asVersion)
import Hocket.Domain.Bookmark (BookmarkItemId (..), biReminder)
import Hocket.Port (SystemPort (..))
import Hocket.Shell.Agent.Client (callAgent, isChannelFullError)
import Hocket.Shell.Headless (Loop (..), loopIdle)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), close, connect, defaultProtocol, socket)
import Shell.AgentParallelTests (Sut (..), noDesktop, retryingFull, withSut)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

waitAppliedTests :: TestTree
waitAppliedTests =
  testGroup
    "wait_applied (I11)"
    [ parallelWriters,
      unknownSeq,
      notYetApplied,
      fullChannelSeqs,
      clientsLeaveVersion
    ]

-- | Answers as the tests read them: a result field, or the error text.
data Answer = Ok !Value | Err !Text deriving (Show, Eq)

call :: Sut -> AgentCmd -> IO Answer
call sut cmd =
  callAgent (sutSocket sut) cmd >>= \case
    Left e -> assertFailure ("transport: " <> T.unpack e)
    Right v -> maybe (assertFailure ("unexpected answer: " <> show v)) pure (parseMaybe answer v)
  where
    answer = withObject "response" $ \o ->
      o .: "ok" >>= \ok -> if ok then Ok <$> o .: "result" else Err <$> o .: "error"

field :: (FromJSON a) => Key -> Answer -> Maybe a
field k = \case
  Ok v -> parseMaybe (withObject "result" (.: k)) v
  Err _ -> Nothing

-- | A write, retried while the channel is full; its seq.
writeSeq :: Sut -> WriteCmd -> IO Int
writeSeq sut w =
  retryingFull (callAgent (sutSocket sut) (AWrite w)) >>= \case
    Right v | Just n <- parseMaybe (withObject "response" (\o -> o .: "result" >>= withObject "result" (.: "seq"))) v -> pure n
    other -> assertFailure ("write " <> show w <> " answered " <> show other)

waitApplied :: Sut -> Int -> Int -> IO Answer
waitApplied sut n ms = call sut (AWaitApplied n ms)

-- | The applied_seq a wait_applied answered with, if it did not fail.
appliedSeq :: Sut -> Int -> Int -> IO (Maybe Int)
appliedSeq sut n ms = field "applied_seq" <$> waitApplied sut n ms

status :: Text -> WriteCmd
status = CmdSetStatus

-- | Four agents, each flagging its own item twenty times, and a watcher on
-- the snapshot. Once wait_applied N answers, the item shows what write N
-- set: nobody else writes it, so a later snapshot cannot hide it. The seqs
-- are 1..80 without a gap or a repeat, and applied_seq only grows.
parallelWriters :: TestTree
parallelWriters =
  testCase "parallel writers: dense unique seqs, each write applied once wait_applied answers" $
    withSut (frItems . traverse . biReminder .~ Nothing) 0 noDesktop $ \sut -> do
      let total = 4 * rounds
          snapVar = loopSnapshot (sutLoop sut)
          watch lastSeen seen
            | lastSeen >= total = pure (reverse seen)
            | otherwise = do
                n <- atomically (readTVar snapVar >>= \s -> asAppliedSeq s <$ check (asAppliedSeq s /= lastSeen))
                watch n (n : seen)
      withAsync (timeout 20000000 (watch 0 [])) $ \watcher -> do
        seqs <- forConcurrently ["1001", "1002", "1003", "1004"] $ \i ->
          forM (take rounds (cycle [("archive", FlagArchive), ("reminder", FlagReminder), ("none", FlagNone)])) $ \(name, act) -> do
            n <- writeSeq sut (CmdSetFlag (BookmarkItemId i) act)
            applied <- waitApplied sut n 5000
            assertBool ("wait_applied " <> show n <> " answered " <> show applied) (maybe False (>= n) (field "applied_seq" applied))
            item <- call sut (ARead (CmdGetItem (BookmarkItemId i)))
            (n, pendingOf item) @?= (n, Just (name :: Text))
            pure n
        sort (concat seqs) @?= [1 .. total]
        seen <- Async.wait watcher
        case seen of
          Nothing -> assertFailure "applied_seq did not reach the last write within 20 s"
          Just xs -> assertBool ("applied_seq went back: " <> show xs) (and (zipWith (<) xs (drop 1 xs)))
  where
    rounds = 20
    pendingOf = \case
      Ok v -> parseMaybe (withObject "item" (\o -> o .: "pending" >>= withObject "pending" (.: "action"))) v
      Err _ -> Nothing

-- | Nothing waits for a seq that was never handed out; the last one issued,
-- and any seq up to 0, is a wait like any other. timeout_ms 0 polls.
unknownSeq :: TestTree
unknownSeq =
  testCase "unknown seq, the last issued seq, seq <= 0 and timeout_ms 0" $
    withSut id 0 noDesktop $ \sut -> do
      waitApplied sut 1 5000 >>= (@?= Err "unknown seq 1: last issued 0")
      traverse (\n -> appliedSeq sut n 0) [0, -3] >>= (@?= [Just 0, Just 0])
      call sut (AWrite (CmdSetFlag (BookmarkItemId "9999") FlagArchive)) >>= (@?= Err "unknown item id: 9999")
      n <- writeSeq sut (status "one")
      n @?= 1
      waitApplied sut 2 5000 >>= (@?= Err "unknown seq 2: last issued 1")
      appliedSeq sut 1 5000 >>= (@?= Just 1)
      appliedSeq sut 1 0 >>= (@?= Just 1)

-- | The loop is held in the browser of write 1, so neither write is
-- applied: update has run write 1, but its step has not been published.
notYetApplied :: TestTree
notYetApplied =
  testCase "a write in the channel or in the loop's hands times out; once published it answers" $ do
    (browser, entered, release) <- blockingBrowser
    withSut id 0 browser $ \sut -> do
      writeSeq sut (CmdOpenItem (BookmarkItemId "1001")) >>= (@?= 1)
      timeout 5000000 (takeMVar entered) >>= (@?= Just ())
      writeSeq sut (status "queued") >>= (@?= 2)
      waitApplied sut 1 200 >>= (@?= Err "timeout waiting for write 1")
      waitApplied sut 2 0 >>= (@?= Err "timeout waiting for write 2")
      putMVar release ()
      applied <- waitApplied sut 2 5000
      (field "applied_seq" applied, field "status" applied) @?= (Just (2 :: Int), Just ("agent: queued" :: Text))

-- | A burst against a held loop: what the channel refused took no seq, so
-- the seqs stay dense and the next one is still unknown.
fullChannelSeqs :: TestTree
fullChannelSeqs =
  testCase "a write refused as channel full consumes no seq" $ do
    (browser, entered, release) <- blockingBrowser
    withSut id 0 browser $ \sut -> do
      writeSeq sut (CmdOpenItem (BookmarkItemId "1001")) >>= (@?= 1)
      timeout 5000000 (takeMVar entered) >>= (@?= Just ())
      answers <- traverse (const (callAgent (sutSocket sut) (AWrite (status "burst")))) [1 .. 20 :: Int]
      let accepted = [n | Right v <- answers, Just n <- [parseMaybe (withObject "response" (\o -> o .: "result" >>= withObject "result" (.: "seq"))) v]]
          full = length [() | Right v <- answers, isChannelFullError v]
          lastIssued = 1 + length accepted
      assertBool ("no write of 20 was refused: " <> show answers) (full > 0)
      accepted @?= [2 .. lastIssued]
      waitApplied sut (lastIssued + 1) 0 >>= (@?= Err ("unknown seq " <> tshow (lastIssued + 1) <> ": last issued " <> tshow lastIssued))
      putMVar release ()
      n <- writeSeq sut (status "after")
      n @?= lastIssued + 1
      appliedSeq sut n 5000 >>= (@?= Just n)
  where
    tshow = T.pack . show

-- | The version an agent waits on does not count clients: a held
-- connection and its close leave it where it was. The pauses give the
-- server's count poster time to post; they cannot make this pass wrongly.
clientsLeaveVersion :: TestTree
clientsLeaveVersion =
  testCase "connects and disconnects leave the version alone" $
    withSut id 0 noDesktop $ \sut -> do
      let version = asVersion <$> readTVarIO (loopSnapshot (sutLoop sut))
          quiet = do
            threadDelay 300000
            idle <- timeout 5000000 (atomically (loopIdle (sutEnv sut) (sutLoop sut) >>= check))
            idle @?= Just ()
      quiet
      before <- version
      held <- socket AF_UNIX Stream defaultProtocol
      connect held (SockAddrUnix (sutSocket sut))
      quiet
      connected <- version
      close held
      quiet
      after <- version
      (connected, after) @?= (before, before)

-- | A browser that holds the loop in its step until released.
blockingBrowser :: IO (SystemPort, MVar (), MVar ())
blockingBrowser = do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  pure (SystemPort (\_ _ -> putMVar entered () >> takeMVar release) (\_ -> pure ()), entered, release)
