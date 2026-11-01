{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The IO shell of the agent control socket: a Unix-domain socket speaking
-- newline-delimited JSON. Reads are answered from the snapshot mirror
-- without touching the Brick event loop; validated writes are injected into
-- the same 'BChan' the keyboard uses, so every TUI invariant holds for the
-- agent as well.
module AgentServer
  ( AgentEnv (..),
    runAgentServer,
    BoundSocket,
    newBoundSocket,
    boundSocketId,
    socketDirIsOurs,
    cleanupAgentSocket,
    resolveAgentSocketPath,
  )
where

import Control.Concurrent (forkFinally)
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
import Network.Bookmark.Agent.Intent (toIntent)
import Network.Bookmark.Agent.Protocol
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
import Network.Bookmark.Agent.Snapshot (AgentSnapshot, asVersion)
import Network.Bookmark.App.Msg (Intent (SetStatus), Msg (AgentClients, Intent), Now)
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
import Worker (exceptionMessage, trySync)

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
    forever $ do
      (conn, _) <- accept sock
      -- One connection's failure must not end the loop: the listener would
      -- close and leave a silently dead socket file. Async exceptions (the
      -- cancel on exit) still propagate.
      tracked clients 1
      void $
        forkFinally
          (serveClient env conn)
          (\_ -> tracked clients (-1))
  where
    -- The count is only for the header, so a failure to post it is
    -- reported and the client served anyway.
    tracked clients d =
      trySync (announce clients d) >>= either (report . ("agent: client tracking failed: " <>) . exceptionMessage) pure
    -- Best effort: if posting is what fails, this can fail the same way.
    report t = void (trySync (aeInject env (Intent (SetStatus t))))
    -- Track connected clients and mirror the count into the TUI header.
    announce clients d = do
      n <- atomically (modifyTVar' clients (+ d) >> readTVar clients)
      void (aeInject env (AgentClients n))

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
    let clampedMs = max 0 (min 60000 timeoutMs)
    msnap <-
      timeout (clampedMs * 1000) . atomically $ do
        snap <- readTVar (aeSnapshot env)
        check (asVersion snap > after)
        pure snap
    pure $ case msnap of
      Nothing -> errResponse rid "timeout waiting for state change"
      Just snap -> okResponse rid (stateView snap)
  AWrite w -> do
    snap <- readTVarIO (aeSnapshot env)
    case validateWrite snap w of
      Left err -> pure (errResponse rid err)
      Right w' -> do
        evt <- toEvent env w'
        accepted <- aeInject env evt
        pure $
          if accepted
            then
              okResponse
                rid
                (object ["injected" .= True, "version" .= asVersion snap])
            else errResponse rid "event channel full, retry"

-- | A validated write as the Intent the keyboard would produce. The clock is
-- sampled here, in the shell, once per write.
toEvent :: AgentEnv -> WriteCmd -> IO Msg
toEvent env cmd = Intent . (`toIntent` cmd) <$> aeNow env
