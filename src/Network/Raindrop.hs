{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Raindrop
  ( raindrop,
  )
where

import Control.Exception (throwIO)
import Control.Lens (view, (^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logWarnN)
import Control.Retry (RetryPolicyM, RetryStatus (rsIterNumber), exponentialBackoff, limitRetries, retrying)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.Bookmark.Types (BookmarkCredentials, BookmarkItemId (..), BookmarkRequest (..), RaindropCollectionId (RaindropCollectionId), RaindropToken (..), archiveCollectionId, raindropToken, _BookmarkItemId)
import Network.HTTP.Types.Status (statusCode)
import Network.Raindrop.Api
import Servant.Client (ClientError (..), ClientM, ResponseF (responseStatusCode), runClientM)

retryPolicy :: (MonadIO m) => RetryPolicyM m
retryPolicy = exponentialBackoff 100000 <> limitRetries 3

-- | Worth retrying: the request may not have reached Raindrop, or Raindrop
-- asked us to back off. Auth and decode errors would fail identically again.
isTransient :: ClientError -> Bool
isTransient (ConnectionError _) = True
isTransient (FailureResponse _ r) = let c = statusCode (responseStatusCode r) in c == 429 || c >= 500
isTransient _ = False

-- | Run a call, retrying transient failures; the final failure is thrown.
runRetrying :: (MonadIO m, MonadLogger m) => ClientM a -> m a
runRetrying act = do
  env <- liftIO raindropEnv
  result <- retrying retryPolicy shouldRetry (const (liftIO (runClientM act env)))
  either (liftIO . throwIO) pure result
  where
    shouldRetry st (Left e)
      | isTransient e = do
          logWarnN $ "raindrop: transient failure on attempt " <> T.pack (show (rsIterNumber st + 1)) <> ": " <> T.pack (show e)
          pure True
    shouldRetry _ _ = pure False

-- | For non-idempotent calls: a retry after a lost response would repeat the effect.
runOnce :: (MonadIO m) => ClientM a -> m a
runOnce act = liftIO $ do
  env <- raindropEnv
  runClientM act env >>= either throwIO pure

-- | Failures are thrown as 'ClientError'; @result: false@ answers come back as 'False'.
raindrop :: (MonadIO m, MonadLogger m) => BookmarkCredentials -> BookmarkRequest a -> m a
raindrop creds req = case req of
  AddBookmark link mCollection tags -> do
    Created i <- runOnce (addItem api (NewRaindrop link (fromMaybe "-1" mCollection) tags))
    pure (Just (BookmarkItemId (T.pack (show i))))
  ArchiveBookmark bid -> ok (updateItem api (rawId bid) (MoveTo archiveId))
  BatchArchiveBookmarks bids -> ok (moveItems api (BatchMove (map rawId bids) archiveId))
  SetReminder bid t -> ok (updateItem api (rawId bid) (SetReminderAt t))
  RemoveReminder bid -> ok (updateItem api (rawId bid) ClearReminder)
  RetrieveBookmarks page (RaindropCollectionId cid) search -> do
    Items count items <- runRetrying (listItems api cid page 50 search)
    pure (count, items)
  where
    RaindropToken token = view raindropToken creds
    api = raindropApi token
    archiveId = view archiveCollectionId creds
    rawId = (^. _BookmarkItemId)
    ok call = (\(ApiResult b) -> b) <$> runRetrying call
