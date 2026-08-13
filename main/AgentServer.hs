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
import Control.Monad (forever, void)
import Data.Aeson (Value, eitherDecodeStrict, encode, object, (.=))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Events
  ( HocketEvent,
    clearAllFlagsEvt,
    executeBatchEvt,
    fetchItemsEvt,
    openItemByIdEvt,
    selectItemEvt,
    setAgentClientsEvt,
    setAllFlagsToArchiveEvt,
    setFilterQueryEvt,
    setPendingActionEvt,
    setShowFutureRemindersEvt,
    setStatusEvt,
    setVideoFilterModeEvt,
  )
import Network.Bookmark.Agent.Protocol
  ( AgentCmd (..),
    FlagAction (..),
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
import Network.Bookmark.Types (PendingAction (..))
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
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO
  ( BufferMode (LineBuffering),
    Handle,
    IOMode (ReadWriteMode),
    hClose,
    hSetBuffering,
  )
import System.Posix.Files (setFileMode)
import System.Posix.User (getEffectiveUserID)
import System.Timeout (timeout)

data AgentEnv = AgentEnv
  { aeSnapshot :: !(TVar AgentSnapshot),
    -- | Non-blocking injection into the TUI event channel; False = full.
    aeInject :: !(HocketEvent -> IO Bool),
    -- | Policy for agent-scheduled reminders (same as the 's' key).
    aeReminderTime :: !(IO UTCTime)
  }

-- | Reclaim a stale socket file left over from a crash or hard kill, but
-- never touch a socket that a live instance still owns. The probe is a
-- connect: a live Unix-domain listener accepts it; a stale one (whose owning
-- process is gone) refuses it with ECONNREFUSED. A live competitor is left
-- intact so the subsequent 'bind' raises the kernel's canonical
-- "address already in use" instead of us silently hijacking it.
reclaimStaleSocket :: FilePath -> IO ()
reclaimStaleSocket path = do
  live <-
    try @IOException $ do
      probe <- socket AF_UNIX Stream defaultProtocol
      connect probe (SockAddrUnix path)
      close probe
  case live of
    Right () -> pure () -- a live listener owns the path; leave it for 'bind'
    Left _ ->
      -- Not reachable via a listener. Only unlink if a (dead) socket file
      -- actually exists; a wholly absent path is not ours to remove.
      void (try @IOException (removeFile path))

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

-- | Bind the control socket and serve clients until killed. Never returns
-- normally; exceptions propagate to the caller (which surfaces them in the
-- TUI status bar).
runAgentServer :: FilePath -> AgentEnv -> IO ()
runAgentServer path env = do
  createDirectoryIfMissing True (takeDirectory path)
  setFileMode (takeDirectory path) 0o700
  reclaimStaleSocket path
  clients <- newTVarIO (0 :: Int)
  bracket (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
    bind sock (SockAddrUnix path)
    setFileMode path 0o600
    listen sock 5
    forever $ do
      (conn, _) <- accept sock
      announce clients 1
      void $
        forkFinally
          (serveClient env conn)
          (\_ -> announce clients (-1))
  where
    -- Track connected clients and mirror the count into the TUI header.
    announce clients d = do
      n <- atomically (modifyTVar' clients (+ d) >> readTVar clients)
      void (aeInject env (setAgentClientsEvt n))

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

-- | Translate a validated write into the event the keyboard would have sent.
-- The clock for reminder scheduling is sampled here, in the shell.
toEvent :: AgentEnv -> WriteCmd -> IO HocketEvent
toEvent env = \case
  CmdSetFlag bid FlagArchive -> pure (setPendingActionEvt bid ToBeArchived)
  CmdSetFlag bid FlagReminder -> do
    t <- aeReminderTime env
    pure (setPendingActionEvt bid (ToBeReminded t))
  CmdSetFlag bid FlagRemoveReminder -> pure (setPendingActionEvt bid ReminderToBeRemoved)
  CmdSetFlag bid FlagNone -> pure (setPendingActionEvt bid None)
  CmdClearFlags -> pure clearAllFlagsEvt
  CmdFlagAllArchive -> pure setAllFlagsToArchiveEvt
  CmdExecute -> pure executeBatchEvt
  CmdRefresh -> pure fetchItemsEvt
  CmdSetFilter q -> pure (setFilterQueryEvt q)
  CmdSetVideoFilter m -> pure (setVideoFilterModeEvt m)
  CmdSetShowFutureReminders b -> pure (setShowFutureRemindersEvt b)
  CmdSelectItem bid -> pure (selectItemEvt bid)
  CmdOpenItem bid -> pure (openItemByIdEvt bid)
  CmdSetStatus t -> pure (setStatusEvt (Just ("agent: " <> t)))
