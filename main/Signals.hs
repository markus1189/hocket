-- | GHC's default action for SIGTERM and SIGHUP ends the process without
-- unwinding, so 'finally' cleanups (agent socket, fixture dump) never ran
-- on @kill@ or @tmux kill-session@ (B8).
module Signals (unwindOnTermination) where

import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (AsyncException (UserInterrupt))
import Control.Monad (void)
import System.Posix.Signals (Handler (Catch), installHandler, sigHUP, sigTERM)

-- | Turn SIGTERM and SIGHUP into a 'UserInterrupt' on the calling thread,
-- the way Ctrl-C already arrives. Call it on the main thread.
unwindOnTermination :: IO ()
unwindOnTermination = do
  tid <- myThreadId
  let interrupt = Catch (throwTo tid UserInterrupt)
  void (installHandler sigTERM interrupt Nothing)
  void (installHandler sigHUP interrupt Nothing)
