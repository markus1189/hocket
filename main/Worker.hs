{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | What the async workers post, as pure functions of what their calls
-- returned, so the posting rules can be tested without IO.
module Worker
  ( fetchResultEvents,
    trySync,
    exceptionMessage,
  )
where

import Control.Exception (SomeAsyncException, SomeException (..), displayException, fromException, throwIO, try)
import Data.Text (Text)
import qualified Data.Text as T
import Events
import Network.Bookmark.Types (BookmarkItemBatch)

-- | The events a finished fetch posts, given the rendered error or the pages.
fetchResultEvents :: Bool -> Either (Maybe Text) [BookmarkItemBatch] -> [HocketEvent]
fetchResultEvents _ (Left err) = [fetchFailedEvt err]
fetchResultEvents isUpdateFetch (Right batches) = [setStatusEvt Nothing, fetchCompletedEvt isUpdateFetch batches]

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
