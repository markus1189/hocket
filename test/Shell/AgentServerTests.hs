{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Shell.AgentServerTests
  ( agentServerIntegrationTests,
  )
where

import Brick.BChan (BChan, newBChan, readBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, cancel, poll, wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO)
import Control.Exception (IOException, SomeException, displayException, finally, throwIO, try)
import Control.Monad (void, when)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime (utc)
import Hocket.Agent.Protocol
import Hocket.Agent.Snapshot
import Hocket.App.Model
import qualified Hocket.App.Msg as I
import Hocket.Domain.Bookmark
import Hocket.Port (RaindropPort (..), SystemPort (..))
import Hocket.Shell.Agent.Client (callAgent, isChannelFullError)
import Hocket.Shell.Agent.Server (AgentEnv (..), BoundSocket, boundSocketId, cleanupAgentSocket, newBoundSocket, runAgentServer, socketDirIsOurs)
import qualified Hocket.Shell.Agent.Server as Server
import Hocket.Shell.Interpret (newEnv)
import Hocket.Shell.Signals (Terminated (..), exitOnTerminated, unwindOnTermination)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), close, connect, defaultProtocol, socket, socketToHandle)
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getTemporaryDirectory, listDirectory, pathIsSymbolicLink, removeDirectoryRecursive, removeFile)
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
import Test.Tasty
import Test.Tasty.HUnit
import TestUtil

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
-- channel (blocking). Skips AgentClients connect/disconnect bookkeeping.
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
withAgentServer action = do
  events <- newBChan 10
  withInjectingServer (\evt -> True <$ writeBChan events evt) (`action` events)

-- | 'withAgentServer' with this injection in place of the channel.
withInjectingServer :: (I.Msg -> IO Bool) -> (TVar AgentSnapshot -> FilePath -> IO a) -> IO a
withInjectingServer inject action = withSystemTempDirectory "hk" $ \dir -> do
  let path = dir </> "s.sock"
  snapVar <- newTVarIO emptySnapshot
  atomically (modifyTVar' snapVar (const (buildTestSnapshot 1)))
  let env = AgentEnv {aeSnapshot = snapVar, aeInject = inject, aeNow = pure testNow}
  -- The readiness probe is a real client: the server announces it with two
  -- AgentClients messages (+1, then -1 from its own thread, so possibly
  -- after the test's first events). readAgentInjected skips them; any test asserting
  -- exact BChan contents must account for them.
  bound <- newBoundSocket
  withAsync (runAgentServer bound path env) $ \_ -> do
    waitUntilReachable 100 path
    action snapVar path

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

-- | A field of an ok answer's result, and an error answer's text.
resultField :: (A.FromJSON a) => A.Key -> A.Value -> Maybe a
resultField k = parseMaybe (A.withObject "response" (\o -> o A..: "result" >>= A.withObject "result" (A..: k)))

errorText :: A.Value -> Maybe Text
errorText = parseMaybe (A.withObject "response" (A..: "error"))

-- | What a reader found in a call's answer, or the whole answer.
answered :: (A.Value -> Maybe a) -> Either Text A.Value -> Either Text a
answered reader = (>>= \v -> maybe (Left (T.pack (show v))) Right (reader v))

reportsFailure :: I.Msg -> Bool
reportsFailure (I.Intent (I.SetStatus t)) = "inject broke" `T.isInfixOf` t
reportsFailure _ = False

agentServerIntegrationTests :: TestTree
agentServerIntegrationTests =
  testGroup
    "Agent server (socket)"
    [ testCase "the server leaves no file descriptor behind when it stops" $ do
        env <- newEnv (RaindropPort (\_ -> fail "no Raindrop here")) (SystemPort (\_ _ -> pure ()) (\_ -> pure ())) (pure testNow)
        -- One start and stop, with a call once it is up so it has bound.
        let cycleOnce = withSystemTempDirectory "hocket-fd" $ \dir -> do
              let path = dir </> "sock" </> "s.sock"
              up <- Server.withAgentServer env path (const (timeout 5000000 (untilUp path)))
              maybe (assertFailure "the agent socket did not come up within 5 s") pure up
            untilUp path = callAgent path (ARead CmdGetState) >>= either (const (threadDelay 10000 >> untilUp path)) (const (pure ()))
            fds = length <$> listDirectory "/proc/self/fd"
        cycleOnce
        before <- fds
        mapM_ (const cycleOnce) [1 .. 20 :: Int]
        after <- fds
        assertBool ("open file descriptors went from " <> show before <> " to " <> show after <> " over 20 cycles") (after - before <= 2),
      testCase "get_state returns the snapshot view with a version" $
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
          -- The first write gets seq 1, in the reply and on the message.
          resultField "seq" resp @?= Just (1 :: Int)
          injected <- readAgentInjected events
          injected @?= I.AgentWrite 1 (I.SetFilter "haskell"),
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
      testCase "the seq lock: a second write waits until the first one's inject returns (§12.5 #4)" $ do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        injected <- newIORef []
        let inject = \case
              I.AgentWrite n _ -> do
                before <- atomicModifyIORef' injected (\ns -> (ns <> [n], ns))
                when (null before) (putMVar entered () >> takeMVar release)
                pure True
              _ -> pure True
        withInjectingServer inject $ \_ path -> do
          first <- async (callAgent path (AWrite (CmdSetStatus "first")))
          takeMVar entered
          second <- async (callAgent path (AWrite (CmdSetStatus "second")))
          -- Unlocked, the second inject is logged within this time, with
          -- the seq the first has not committed yet; locked, it never is.
          _ <- waitFor 25 ((> 1) . length <$> readIORef injected)
          putMVar release ()
          seqs <- traverse (fmap (answered (resultField "seq")) . wait) [first, second]
          readIORef injected >>= (@?= [1, 2])
          seqs @?= [Right (1 :: Int), Right 2],
      testCase "wait_version with timeout_ms 0 polls: a passed version answers at once" $
        withAgentServer $ \snapVar _events path -> do
          atomically (modifyTVar' snapVar (const (buildTestSnapshot 4)))
          res <- callAgent path (AWait 3 0)
          answered (resultField "version") res @?= Right (4 :: Int)
          stale <- callAgent path (AWait 4 0)
          answered errorText stale @?= Right "timeout waiting for state change",
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
          injected @?= I.AgentWrite 1 (I.SetFlag (BookmarkItemId "in-1") FlagArchive testNow),
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
