{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The IO shell of the agent control socket: a Unix-domain socket speaking
-- newline-delimited JSON. Reads are answered from the snapshot mirror
-- without touching the Brick event loop; validated writes are injected into
-- the same 'BChan' the keyboard uses, so every TUI invariant holds for the
-- agent as well.
module Hocket.Shell.Agent.Server
  ( AgentEnv (..),
    runAgentServer,
    BoundSocket,
    newBoundSocket,
    boundSocketId,
    socketDirIsOurs,
    cleanupAgentSocket,
    resolveAgentSocketPath,
    withAgentServer,
  )
where

import Control.Concurrent (forkFinally, threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    check,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
  )
import Control.Exception (IOException, SomeException, bracket, finally, try)
import Control.Monad (forever, unless, void, when)
import Data.Aeson (Value, eitherDecodeStrict, encode, object, (.=))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import Hocket.Agent.Intent (toIntent)
import Hocket.Agent.Protocol
  ( AgentCmd (..),
    RawRequest (..),
    WriteCmd (..),
    decodeCmd,
    errResponse,
    okResponse,
    serveRead,
    stateView,
    validateWrite,
  )
import Hocket.Agent.Snapshot (AgentSnapshot, asVersion, emptySnapshot)
import Hocket.App.Msg (Intent (SetStatus), Msg (AgentClients, AgentServerDied, Intent), Now)
import Hocket.Shell.Interpret (Env (..), injectMsg, postMsg)
import Hocket.Shell.Worker (exceptionMessage, trySync)
import Network.Socket
  ( Family (AF_UNIX),
    SockAddr (SockAddrUnix),
    Socket,
    SocketType (Stream),
    accept,
    bind,
    close,
    connect,
    defaultProtocol,
    listen,
    socket,
    socketToHandle,
  )
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO
  ( BufferMode (LineBuffering),
    Handle,
    IOMode (ReadWriteMode),
    hClose,
    hSetBuffering,
  )
import System.Posix.Files (deviceID, fileID, getSymbolicLinkStatus, isSocket, setFileMode)
import System.Posix.Types (DeviceID, FileID)
import System.Posix.User (getEffectiveUserID)
import System.Timeout (timeout)

data AgentEnv = AgentEnv
  { aeSnapshot :: !(TVar AgentSnapshot),
    -- | Non-blocking injection into the TUI event channel; False = full.
    aeInject :: !(Msg -> IO Bool),
    -- | The clock, sampled once per write (reminders: same policy as 's').
    aeNow :: !(IO Now)
  }

-- | Reclaim a stale socket file left over from a crash or hard kill, but
-- never touch a socket that a live instance still owns. The probe is a
-- connect: a live Unix-domain listener accepts it; a stale one (whose owning
-- process is gone) refuses it with ECONNREFUSED. A live competitor is left
-- intact so the subsequent 'bind' raises the kernel's canonical
-- "address already in use" instead of us silently hijacking it. Anything
-- but a socket (checked with lstat, so a symlink counts as "anything") is
-- refused: connect reports ECONNREFUSED for a regular file too, and an
-- explicit --agent-socket-path may name the user's own file (B7b).
reclaimStaleSocket :: FilePath -> IO ()
reclaimStaleSocket path = do
  existing <- try @IOException (getSymbolicLinkStatus path)
  case existing of
    Left _ -> pure () -- nothing there; 'bind' creates it
    Right st
      | not (isSocket st) -> ioError (userError (path <> " exists and is not a socket"))
      | otherwise -> do
          live <-
            try @IOException $ do
              probe <- socket AF_UNIX Stream defaultProtocol
              connect probe (SockAddrUnix path)
              close probe
          case live of
            Right () -> pure () -- a live listener owns the path; leave it for 'bind'
            Left _ -> void (try @IOException (removeFile path))

-- | Default socket location: @$XDG_RUNTIME_DIR/hocket/control.sock@, falling
-- back to a per-uid directory under /tmp.
resolveAgentSocketPath :: IO FilePath
resolveAgentSocketPath = do
  mruntime <- lookupEnv "XDG_RUNTIME_DIR"
  case mruntime of
    Just dir | not (null dir) -> pure (dir </> "hocket" </> "control.sock")
    _ -> do
      uid <- getEffectiveUserID
      pure ("/tmp" </> ("hocket-" <> show uid) </> "control.sock")

-- | The socket this instance bound, by device and inode, so cleanup never
-- unlinks a file some other process put at the path.
newtype BoundSocket = BoundSocket (IORef (Maybe (DeviceID, FileID)))

newBoundSocket :: IO BoundSocket
newBoundSocket = BoundSocket <$> newIORef Nothing

-- | What the instance recorded after its bind; 'Nothing' until then.
boundSocketId :: BoundSocket -> IO (Maybe (DeviceID, FileID))
boundSocketId (BoundSocket ref) = readIORef ref

-- | Whether 'runAgentServer' will create the socket's directory, making it
-- hocket's to remove on exit. Ask before starting the server.
socketDirIsOurs :: FilePath -> IO Bool
socketDirIsOurs = fmap not . doesDirectoryExist . takeDirectory

-- | On graceful exit, and only if this instance bound: remove the socket
-- if the path still holds that same socket (an instance that lost the bind
-- must not unlink the winner's, B7b), and the directory if hocket created
-- it ('socketDirIsOurs'). 'removeDirectory' refuses a non-empty directory,
-- and an explicit --agent-socket-path into /tmp or $HOME leaves that
-- directory alone (B7). Best-effort: a failed cleanup never changes the
-- exit outcome.
cleanupAgentSocket :: BoundSocket -> Bool -> FilePath -> IO ()
cleanupAgentSocket (BoundSocket ref) ours path =
  readIORef ref
    >>= traverse_
      ( \boundId -> do
          void . try @SomeException $ do
            st <- getSymbolicLinkStatus path
            when (isSocket st && (deviceID st, fileID st) == boundId) (removeFile path)
          when ours (void (try @SomeException (removeDirectory (takeDirectory path))))
      )

-- | Bind the control socket and serve clients until killed. Never returns
-- normally; exceptions propagate to the caller (which surfaces them in the
-- TUI status bar).
runAgentServer :: BoundSocket -> FilePath -> AgentEnv -> IO ()
runAgentServer (BoundSocket ref) path env = do
  let dir = takeDirectory path
  dirExisted <- doesDirectoryExist dir
  createDirectoryIfMissing True dir
  -- Tighten the directory only when we are the ones who created it. An
  -- explicit --agent-socket-path may point into a directory that is none of
  -- our business: chmodding a shared one (/tmp) fails outright and kills the
  -- server, and chmodding one the user owns ($HOME) would silently lock it
  -- down. The socket file itself is always 0600, which is what actually
  -- gates access.
  unless dirExisted (setFileMode dir 0o700)
  reclaimStaleSocket path
  clients <- newTVarIO (0 :: Int)
  bracket (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
    bind sock (SockAddrUnix path)
    st <- getSymbolicLinkStatus path
    writeIORef ref (Just (deviceID st, fileID st))
    setFileMode path 0o600
    listen sock 5
    -- Counting is STM only, so nothing on the accept path can fail or block
    -- on the TUI; postCounts mirrors the count into the header.
    withAsync (postCounts clients) $ \_ -> forever $ do
      (conn, _) <- accept sock
      count clients 1
      void $
        forkFinally
          (serveClient env conn)
          (\_ -> count clients (-1))
  where
    count clients d = atomically (modifyTVar' clients (+ d))
    -- A post carries the absolute count, so only the newest matters: wait
    -- for a change, post it, and retry one the full channel dropped (False)
    -- or that failed, so the header heals instead of keeping a stale count.
    -- A failure is reported once per streak; async exceptions (the cancel on
    -- exit) still propagate.
    postCounts clients = go 0 False
      where
        go posted failing = do
          n <- atomically (readTVar clients >>= \n -> n <$ check (n /= posted))
          trySync (aeInject env (AgentClients n)) >>= \case
            Right True -> go n False
            Right False -> retry posted failing
            Left e -> do
              unless failing (report ("agent: client count failed: " <> exceptionMessage e))
              retry posted True
        retry posted failing = threadDelay 50000 >> go posted failing
    -- Best effort: if posting is what fails, this can fail the same way.
    report t = void (trySync (aeInject env (Intent (SetStatus t))))

serveClient :: AgentEnv -> Socket -> IO ()
serveClient env conn = do
  h <- socketToHandle conn ReadWriteMode
  hSetBuffering h LineBuffering
  loop h `finally` void (try @SomeException (hClose h))
  where
    loop :: Handle -> IO ()
    loop h = do
      eline <- try @SomeException (BS.hGetLine h)
      case eline of
        Left _ -> pure () -- EOF or client hung up
        Right line
          | BS.all isSpace line -> loop h
          | otherwise -> do
              resp <- handleLine env line
              BSL.hPutStrLn h (encode resp)
              loop h

handleLine :: AgentEnv -> BS.ByteString -> IO Value
handleLine env line = case eitherDecodeStrict line of
  Left err -> pure (errResponse Nothing ("bad request: " <> T.pack err))
  Right raw -> case decodeCmd (rawMethod raw) (rawParams raw) of
    Left err -> pure (errResponse (rawId raw) err)
    Right cmd -> dispatch env (rawId raw) cmd

dispatch :: AgentEnv -> Maybe Value -> AgentCmd -> IO Value
dispatch env rid = \case
  ARead r -> do
    snap <- readTVarIO (aeSnapshot env)
    pure (either (errResponse rid) (okResponse rid) (serveRead snap r))
  AWait after timeoutMs -> do
    msnap <- awaitSnapshot (aeSnapshot env) timeoutMs ((> after) . asVersion)
    pure $ case msnap of
      Nothing -> errResponse rid "timeout waiting for state change"
      Just snap -> okResponse rid (stateView snap)
  AWrite w -> do
    snap <- readTVarIO (aeSnapshot env)
    case validateWrite snap w of
      Left err -> pure (errResponse rid err)
      Right w' -> do
        evt <- writeMsg env w'
        accepted <- aeInject env evt
        pure $
          if accepted
            then
              okResponse
                rid
                (object ["injected" .= True, "version" .= asVersion snap])
            else errResponse rid "event channel full, retry"

-- | The first snapshot that passes, within the timeout clamped to
-- [0, 60000] ms. Checked once before the timed wait: 'timeout' 0 gives up
-- without running its action, and a timeout of 0 is how a client polls.
awaitSnapshot :: TVar AgentSnapshot -> Int -> (AgentSnapshot -> Bool) -> IO (Maybe AgentSnapshot)
awaitSnapshot snapVar timeoutMs ok = do
  now <- readTVarIO snapVar
  if ok now
    then pure (Just now)
    else timeout (max 0 (min 60000 timeoutMs) * 1000) . atomically $ do
      snap <- readTVar snapVar
      snap <$ check (ok snap)

-- | A validated write as the Intent the keyboard would produce. The clock is
-- sampled here, in the shell, once per write.
writeMsg :: AgentEnv -> WriteCmd -> IO Msg
writeMsg env cmd = Intent . (`toIntent` cmd) <$> aeNow env

-- | Serve the agent socket at this path for the length of the action, which
-- gets the snapshot its loop must publish to. On the way out the server
-- thread is cancelled first, which closes the listening socket and ends
-- the client-count poster, then the socket is removed (B7). bracket, not
-- finally: a signal while the server starts is held off until it is
-- recorded for cleanup. Server failures surface in the TUI's status line
-- (AgentServerDied) instead of killing the app.
withAgentServer :: Env -> FilePath -> (TVar AgentSnapshot -> IO a) -> IO a
withAgentServer env path act =
  bracket
    ((,,) <$> socketDirIsOurs path <*> newBoundSocket <*> newTVarIO emptySnapshot)
    (\(ours, bound, _) -> cleanupAgentSocket bound ours path)
    (\(_, bound, snapVar) -> withAsync (serve bound snapVar) (const (act snapVar)))
  where
    serve bound snapVar = do
      let agentEnv = AgentEnv {aeSnapshot = snapVar, aeInject = injectMsg env, aeNow = envNow env}
      -- trySync: a cancel is the way out, not a failure to report.
      r <- trySync (runAgentServer bound path agentEnv)
      case r of
        -- Blocking: rare, the main loop drains, and a dropped report would
        -- leave a dead socket looking alive.
        Left e -> postMsg env (AgentServerDied (T.pack (show e)))
        Right () -> pure ()
