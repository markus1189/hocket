{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The shell's only way from an 'Effect' to IO (plan §4.3).
module Hocket.Shell.Interpret
  ( Env (..),
    newEnv,
    postMsg,
    injectMsg,
    tookMsg,
    step,
    publish,
    publishSTM,
    interpret,
    copyCleaned,
    batchWorker,
  )
where

import Brick.BChan (BChan, newBChan, writeBChan, writeBChanNonBlocking)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO)
import Control.Monad (foldM, unless)
import Data.Bifunctor (second)
import Data.Text (Text)
import qualified Data.Text as T
import Hocket.Adapter.System (cleanUrl)
import Hocket.Agent.Snapshot (AgentSnapshot, asVersion, takeSnapshot)
import Hocket.App.Effect
import Hocket.App.Model (Model)
import Hocket.App.Msg
import Hocket.App.Plan (BatchPlan (..))
import Hocket.App.Update (update)
import Hocket.Domain.Bookmark (URL (..))
import Hocket.Port (RaindropPort (..), SystemPort (..))
import Hocket.Shell.Worker (runBatch, runFetch, trySync)

-- | Where to post events, Raindrop, the desktop and the clock.
data Env = Env
  { -- | Write only through 'postMsg' and 'injectMsg': they count what is
    -- in flight, which the headless loop's idle check needs and brick's
    -- channel cannot tell.
    envEvents :: !(BChan Msg),
    -- | Posted and not yet taken by the event loop.
    envPending :: !(TVar Int),
    envPort :: !RaindropPort,
    envSystem :: !SystemPort,
    envNow :: !(IO Now)
  }

newEnv :: RaindropPort -> SystemPort -> IO Now -> IO Env
newEnv port system now = Env <$> newBChan 10 <*> newTVarIO 0 <*> pure port <*> pure system <*> pure now

-- | Post, waiting while the channel is full (the workers). Counts before
-- writing, so a message in the channel is always pending: loopIdle relies
-- on it, and brick's channel hides its queue, so the count and the write
-- cannot share a transaction.
postMsg :: Env -> Msg -> IO ()
postMsg env m = atomically (modifyTVar' (envPending env) (+ 1)) >> writeBChan (envEvents env) m

-- | Post unless the channel is full (the agent socket); whether it was.
-- Counts before writing, as 'postMsg' does.
injectMsg :: Env -> Msg -> IO Bool
injectMsg env m = do
  atomically (modifyTVar' (envPending env) (+ 1))
  posted <- writeBChanNonBlocking (envEvents env) m
  unless posted (atomically (modifyTVar' (envPending env) (subtract 1)))
  pure posted

-- | The event loop took a message off the channel.
tookMsg :: Env -> STM ()
tookMsg env = modifyTVar' (envPending env) (subtract 1)

-- | One message as both event loops run it: update, then its effects in
-- order, each effect's immediate answers through 'step' again, depth first.
-- True when an effect was 'Halt'; the effects after it still run, as brick
-- only stops after the event.
step :: Env -> Msg -> Model -> IO (Model, Bool)
step env m s0 = foldM run (s1, False) effects
  where
    (s1, effects) = update m s0
    run (s, halted) = \case
      Halt -> pure (s, True)
      effect -> interpret env effect >>= foldM (\(s', h) reply -> second (|| h) <$> step env reply s') (s, halted)

-- | Mirror the settled model for the agent socket, one version per event.
-- Persistent data sharing keeps this cheap; the item projection is only
-- fully forced by a server thread that actually serves it.
publish :: TVar AgentSnapshot -> Model -> IO ()
publish snapVar = atomically . publishSTM snapVar

-- | 'publish' inside a transaction, for a loop that must change more in
-- the same step.
publishSTM :: TVar AgentSnapshot -> Model -> STM ()
publishSTM snapVar s = modifyTVar' snapVar $ \old -> takeSnapshot (asVersion old + 1) s

-- | Run one effect. What the TUI waits for (browser, clipboard) answers
-- with messages for the same event, so the status line is as quick as the
-- old handlers'.
interpret :: Env -> Effect -> IO [Msg]
interpret env = \case
  -- The shell halts the event loop itself.
  Halt -> pure []
  OpenUrl opener url -> failed <$> trySync (sysOpenUrl (envSystem env) (program opener) (URL (T.unpack url)))
  StartFetch plan -> [] <$ async (fetch plan)
  StartBatch plan -> [] <$ async (batchWorker post (envPort env) plan)
  CopyText link -> copyCleaned (sysCopy (envSystem env)) link
  where
    fetch = runFetch (runRaindrop (envPort env)) post
    post = postMsg env
    failed = either (\e -> [SystemFailed (T.pack (show e))]) (const [])
    program = \case
      Firefox -> "firefox"
      XdgOpen -> "xdg-open"

-- | Copy a link the way @y@ always did: cleaned first ('cleanUrl'), and the
-- cleaned text is what the status line reports.
copyCleaned :: (String -> IO ()) -> Text -> IO [Msg]
copyCleaned copy link = do
  let url = cleanUrl (T.unpack link)
  r <- trySync (copy url)
  pure [either (SystemFailed . ("Copy failed: " <>) . T.pack . show) (const (Copied (T.pack url))) r]

-- | The batch worker over this port, as the TUI runs it in a thread.
batchWorker :: (Msg -> IO ()) -> RaindropPort -> BatchPlan -> IO ()
batchWorker post port = runBatch (runRaindrop port) post
