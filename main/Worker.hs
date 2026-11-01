{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | What the async workers post, as pure functions of what their calls
-- returned, so the posting rules can be tested without IO.
module Worker
  ( fetchResultEvents,
    stepEvents,
    runWhile,
    trySync,
    exceptionMessage,
  )
where

import Control.Exception (SomeAsyncException, SomeException (..), displayException, fromException, throwIO, try)
import Control.Lens ((^.))
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Events
import Network.Bookmark.Batch
import Network.Bookmark.Types (BookmarkItemBatch)

-- | The events a finished fetch posts, given the rendered error or the pages.
fetchResultEvents :: Bool -> Either (Maybe Text) [BookmarkItemBatch] -> [HocketEvent]
fetchResultEvents _ (Left err) = [fetchFailedEvt err]
fetchResultEvents isUpdateFetch (Right batches) = [setStatusEvt Nothing, fetchCompletedEvt isUpdateFetch batches]

-- | The events a finished batch step posts: its outcome first, so the
-- successes are recorded even when the step was aborted, then the status.
stepEvents :: (StepOutcome a Text -> HocketEvent) -> StepOutcome a Text -> [HocketEvent]
stepEvents done outcome = done outcome : status
  where
    status = case (outcome ^. soAborted, outcome ^. soFailed) of
      (Just err, _) -> [asyncActionFailedEvt (Just err)]
      (Nothing, []) -> [setStatusEvt Nothing]
      (Nothing, (_, failure) : _) -> [setStatusEvt (Just ("failed: " <> failureText failure))]
    failureText ResultFalse = "result: false"
    failureText (CallFailed err) = err

-- | Run steps in order until one answers 'False' (aborted); later ones are
-- not run.
runWhile :: (Monad m) => [m Bool] -> m ()
runWhile [] = pure ()
runWhile (step : rest) = step >>= \continue -> when continue (runWhile rest)

-- | Catch what the action itself threw, so a worker can still post its
-- terminal message; asynchronous exceptions (a cancel) are rethrown.
trySync :: IO a -> IO (Either SomeException a)
trySync act =
  try act >>= \case
    Left e -> case fromException e of
      Just (_ :: SomeAsyncException) -> throwIO e
      Nothing -> pure (Left e)
    Right a -> pure (Right a)

-- | For the status line: since GHC 9.10, 'displayException' on a
-- 'SomeException' appends a multi-line backtrace, so render the inner one.
exceptionMessage :: SomeException -> Text
exceptionMessage (SomeException e) = T.pack (displayException e)
