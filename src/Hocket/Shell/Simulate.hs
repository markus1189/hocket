-- | The workers without IO (plan §11 #23): stepEffect runs the code the TUI
-- runs for a fetch or a batch, over the pure fake, and answers what the
-- worker would have posted. State-machine tests deliver effects with it.
module Hocket.Shell.Simulate (stepEffect) where

import Control.Monad.Catch (throwM)
import Control.Monad.Catch.Pure (CatchT, runCatchT)
import Control.Monad.State.Strict (State, modify', runState, state)
import Control.Monad.Trans (lift)
import Data.Time (UTCTime)
import Hocket.Adapter.Fake (FakeRaindrop, serveFake)
import Hocket.App.Effect (Effect (..))
import Hocket.App.Msg (Msg)
import Hocket.Domain.Bookmark (BookmarkRequest)
import Hocket.Shell.Worker (runBatch, runFetch)

-- | The state lives below the exceptions, so a failed request keeps what it
-- did to the store, as it does in fakePort's IORef; StateT over Either
-- would roll it back.
type Sim = CatchT (State (FakeRaindrop, Int, [Msg]))

-- | One request as fakePort serves it, at the time of the n-th request.
simCall :: (Int -> UTCTime) -> BookmarkRequest a -> Sim a
simCall clock req = do
  res <- lift . state $ \(fr, n, posted) ->
    let (r, fr') = serveFake (clock n) req fr in (r, (fr', n + 1, posted))
  either throwM pure res

-- | What the worker for this effect posts, in order, and the store and the
-- request count after its calls. The browser, the clipboard and Halt do
-- not reach Raindrop: nothing posted, nothing changed.
stepEffect :: (Int -> UTCTime) -> Effect -> (FakeRaindrop, Int) -> ([Msg], (FakeRaindrop, Int))
stepEffect clock effect (fr, n) = case effect of
  StartFetch plan -> run (runFetch (simCall clock) post plan)
  StartBatch plan -> run (runBatch (simCall clock) post plan)
  _ -> ([], (fr, n))
  where
    post :: Msg -> Sim ()
    post m = lift (modify' (\(f, c, ms) -> (f, c, m : ms)))
    -- Both workers catch every exception a request throws.
    run act = case runState (runCatchT act) (fr, n, []) of
      (Right (), (fr', n', posted)) -> (reverse posted, (fr', n'))
      (Left e, _) -> error ("stepEffect: an exception escaped the worker: " <> show e)
