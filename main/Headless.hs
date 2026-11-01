-- | The event loop without brick or a terminal (plan §6 Phase 4.7): per
-- message the 'step', 'settle' and 'publish' the TUI runs per event, so a
-- test driving it drives the TUI's code path. The agent socket server can
-- serve it unchanged, from 'loopSnapshot' and 'envEvents'.
module Headless
  ( Loop (..),
    newLoop,
    loopIdle,
    runHeadless,
  )
where

import Brick.BChan (readBChan)
import Control.Concurrent.STM
import Data.Maybe (isNothing)
import Interpret (Env (..), publish, step, tookMsg)
import Network.Bookmark.Agent.Snapshot (AgentSnapshot, asAsyncOp, emptySnapshot)
import Network.Bookmark.App.Msg (Intent (Refresh), Msg (Intent))
import Network.Bookmark.Ui.State (HocketState, settle)

data Loop = Loop
  { loopSnapshot :: !(TVar AgentSnapshot),
    -- | From taking a message until what it did is published.
    loopBusy :: !(TVar Bool)
  }

-- | Busy until the startup refresh is published.
newLoop :: IO Loop
newLoop = Loop <$> newTVarIO emptySnapshot <*> newTVarIO True

-- | Nothing posted and untaken, nothing in hand, and no fetch or batch
-- running: a worker frees the slot only through its terminal message,
-- which counts as pending until the loop has taken it, and as in hand until
-- the loop has published what it did.
loopIdle :: Env -> Loop -> STM Bool
loopIdle env loop = do
  empty <- (== 0) <$> readTVar (envPending env)
  busy <- readTVar (loopBusy loop)
  free <- isNothing . asAsyncOp <$> readTVar (loopSnapshot loop)
  pure (empty && not busy && free)

-- | Start as the TUI does, with a refresh, then run every message until one
-- halts; the final model.
runHeadless :: Env -> Loop -> HocketState -> IO HocketState
runHeadless env loop = run (Intent Refresh)
  where
    run m s = do
      (s', halted) <- step env m s
      let settled = settle s'
      publish (loopSnapshot loop) settled
      atomically (writeTVar (loopBusy loop) False)
      if halted
        then pure settled
        else do
          next <- readBChan (envEvents env)
          atomically (tookMsg env >> writeTVar (loopBusy loop) True)
          run next settled
