-- | GHC's default action for SIGTERM and SIGHUP ends the process without
-- unwinding, so 'finally' cleanups (agent socket, fixture dump) never ran
-- on @kill@ or @tmux kill-session@ (B8).
module Signals (Terminated (..), unwindOnTermination, exitOnTerminated) where

import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException, catch)
import Control.Monad (void)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.Posix.Signals (Handler (Catch), Signal, installHandler, sigHUP, sigTERM)

-- | Not 'UserInterrupt': GHC turns an uncaught one into exit 130 (SIGINT),
-- which aborts the rest of an interactive command line.
newtype Terminated = Terminated Signal deriving (Show, Eq)

instance Exception Terminated where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Turn SIGTERM and SIGHUP into 'Terminated' on the calling thread, so it
-- unwinds like Ctrl-C does. Call it on the main thread.
unwindOnTermination :: IO ()
unwindOnTermination = do
  tid <- myThreadId
  let terminate sig = void (installHandler sig (Catch (throwTo tid (Terminated sig))) Nothing)
  terminate sigTERM
  terminate sigHUP

-- | Exit with the shell convention for death by signal, 128 + n.
exitOnTerminated :: IO a -> IO a
exitOnTerminated act = act `catch` \(Terminated sig) -> exitWith (ExitFailure (128 + fromIntegral sig))
