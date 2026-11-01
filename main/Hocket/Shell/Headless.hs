-- | The event loop without brick or a terminal (plan §6 Phase 4.7): per
-- message the 'step', 'settle' and 'publish' the TUI runs per event, so a
-- test driving it drives the TUI's code path. The agent socket server can
-- serve it unchanged, from 'loopSnapshot' and 'envEvents'.
module Hocket.Shell.Headless
  ( Loop (..),
    newLoop,
    loopIdle,
    runHeadless,
  )
where

import Brick.BChan (readBChan)
import Control.Concurrent.STM
import Data.Maybe (isNothing)
import Hocket.Agent.Snapshot (AgentSnapshot, asAsyncOp, emptySnapshot)
import Hocket.App.Model (HocketState, settle)
import Hocket.App.Msg (Intent (Refresh), Msg (Intent))
import Hocket.Shell.Interpret (Env (..), publishSTM, step, tookMsg)

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
-- the loop has published what it did. Not covered: the agent server's
-- client count, which its poster thread posts on its own schedule (retrying
-- every 50 ms while the channel is full), so hsAgentClients may still lag
-- behind an idle loop.
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
      -- One transaction: no observer sees busy cleared with the old
      -- snapshot (an Execute's async_op not yet published).
      atomically (publishSTM (loopSnapshot loop) settled >> writeTVar (loopBusy loop) False)
      if halted
        then pure settled
        else do
          next <- readBChan (envEvents env)
          atomically (tookMsg env >> writeTVar (loopBusy loop) True)
          run next settled
