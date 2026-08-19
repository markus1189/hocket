{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The client half of the agent control socket: connect, send one
-- newline-delimited JSON request, read one newline-delimited response.
--
-- Requests are built from the typed 'AgentCmd' via 'encodeCmd', so the
-- shipped CLI cannot misspell a method name, a param key or an enum value
-- the way a hand-assembled @socat@ line can. Deliberately free of
-- @optparse-applicative@ and @brick@: the argument parsing lives in
-- @hocket.hs@, and this module is linked into the test suite.
module AgentClient
  ( callAgent,
    runAgentCommand,
    isChannelFullError,
  )
where

import AgentServer (resolveAgentSocketPath)
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, bracketOnError, try)
import Data.Aeson (Value, eitherDecodeStrict, encode, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Bookmark.Agent.Protocol (AgentCmd, encodeCmd)
import Network.Socket
  ( Family (AF_UNIX),
    SockAddr (SockAddrUnix),
    SocketType (Stream),
    close,
    connect,
    defaultProtocol,
    socket,
    socketToHandle,
  )
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO
  ( BufferMode (LineBuffering),
    Handle,
    IOMode (ReadWriteMode),
    hClose,
    hFlush,
    hPutStrLn,
    hSetBuffering,
    stderr,
  )

-- | One request, one response, over a fresh connection. 'Left' is a
-- transport failure (nothing listening, the server hung up, an undecodable
-- reply); a protocol-level rejection is a 'Right' carrying @ok: false@.
callAgent :: FilePath -> AgentCmd -> IO (Either Text Value)
callAgent path cmd = do
  res <- try @IOException . bracket (connectTo path) hClose $ \h -> do
    BSL.hPutStr h (encode (requestFor cmd) <> "\n")
    hFlush h
    BS.hGetLine h
  pure $ case res of
    Left err -> Left (transportError path err)
    Right line -> first (\e -> "bad response json: " <> T.pack e) (eitherDecodeStrict line)

-- | CLI entry point: resolve the socket path, make the call, print the raw
-- response line to stdout (one line, jq-friendly) and exit with 0 for
-- @ok: true@, 1 for @ok: false@, 2 when the socket could not be reached.
--
-- The only failure treated as transient is the TUI's event channel being
-- full (the server answers @ok:false@ / "event channel full, retry"), which
-- happens when writes land faster than the render loop can drain the BChan.
-- Everything else -- a validation rejection, a malformed reply, a dead
-- socket -- fails immediately: waiting would not cure those.
runAgentCommand :: Maybe FilePath -> AgentCmd -> IO ()
runAgentCommand mpath cmd = do
  path <- maybe resolveAgentSocketPath pure mpath
  callAgentWithRetry path cmd >>= \case
    Left err -> do
      hPutStrLn stderr (T.unpack err)
      exitWith (ExitFailure 2)
    Right v -> do
      BSL.putStrLn (encode v)
      if responseOk v then exitSuccess else exitWith (ExitFailure 1)

-- | Bounded back-pressure retry for the one genuinely transient server
-- response. Rather than yielding to the 'retry' package's 'RetryPolicyM'
-- machinery for a single fixed short sleep, this is a plain count: up to
-- six attempts with a 20ms pause between them (~0.1s total budget). Six
-- rapid-fire @agent open_item@ calls therefore no longer fail the moment
-- the channel cap is hit. Non-channel responses pass straight through.
callAgentWithRetry :: FilePath -> AgentCmd -> IO (Either Text Value)
callAgentWithRetry path cmd = go (0 :: Int)
  where
    go attempts = do
      res <- callAgent path cmd
      case res of
        Right v
          | isChannelFullError v && attempts < maxAttempts -> do
              threadDelay retryDelayUs
              go (attempts + 1)
        _ -> pure res

    maxAttempts = 6 -- initial attempt plus five retries
    retryDelayUs = 20000

-- | @ok:false@ carrying the server's channel-cap message is the only shape
-- worth retrying; a genuine rejection is final and must not be re-sent.
-- Requiring @ok:false@ also guards against ever retrying (and thus
-- re-injecting) a response that actually succeeded.
isChannelFullError :: Value -> Bool
isChannelFullError v =
  case parseMaybe (withObject "response" $ \o -> (,) <$> o .: "ok" <*> o .: "error") v of
    Just (ok, e) -> not ok && (e :: Text) == "event channel full, retry"
    Nothing -> False

requestFor :: AgentCmd -> Value
requestFor cmd =
  let (method, params) = encodeCmd cmd
   in object ["id" .= (1 :: Int), "method" .= method, "params" .= params]

-- | @ok@ is the authoritative discriminator; a response missing it is not
-- one this server produced, so treat it as failure.
responseOk :: Value -> Bool
responseOk = fromMaybe False . parseMaybe (withObject "response" (.: "ok"))

connectTo :: FilePath -> IO Handle
connectTo path =
  bracketOnError (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
    connect sock (SockAddrUnix path)
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h LineBuffering
    pure h

transportError :: FilePath -> IOException -> Text
transportError path err =
  "no hocket agent socket at "
    <> T.pack path
    <> " ("
    <> T.pack (show err)
    <> ")\nstart the TUI with: hocket tui --agent-socket"
