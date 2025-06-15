{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.Raindrop where

import Control.Exception (SomeException, try)
import Control.Lens ((&), (.~), (^?), view)
import Control.Lens.Operators ((^.), (^..))
import Control.Monad.Logger (MonadLogger, logErrorN)
import Control.Monad.IO.Class (MonadIO)
import Control.Retry (RetryPolicy, RetryStatus, exponentialBackoff, limitRetries, retrying)
import qualified Data.Aeson as A
import Data.Aeson.Lens (AsJSON (_JSON), AsValue (_Bool), key, values, _Integral)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Bookmark.Types (BookmarkCredentials, BookmarkItemId (..), BookmarkRequest (..), RaindropCollectionId (RaindropCollectionId), RaindropToken (..), _BookmarkItemId, raindropToken, archiveCollectionId)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as HCTLS (tlsManagerSettings)
import Network.Wreq (param)
import Control.Monad.IO.Class (liftIO)
import qualified Network.Wreq as W
import Numeric.Natural (Natural)

commonOpts :: RaindropToken -> W.Options
commonOpts (RaindropToken t) =
  W.defaults
    & W.manager .~ Left (HCTLS.tlsManagerSettings {HC.managerResponseTimeout = HC.responseTimeoutDefault})
    & W.header "Authorization" .~ ["Bearer " <> TE.encodeUtf8 t]

retryPolicy :: RetryPolicy
retryPolicy = exponentialBackoff 100000 <> limitRetries 3

shouldRetry :: RetryStatus -> Either SomeException a -> IO Bool
shouldRetry _ (Left _) = return True
shouldRetry _ (Right _) = return False

raindrop :: (MonadIO m, MonadLogger m) => BookmarkCredentials -> BookmarkRequest a -> m a
raindrop creds (AddBookmark link mCollection tags) = do
  let rt = view raindropToken creds
      collection = fromMaybe "-1" mCollection
      payload = A.object
        [ "link" A..= link
        , "collection" A..= collection
        , "tags" A..= tags
        , "pleaseParse" A..= A.object []
        ]
  result <- liftIO $ retrying retryPolicy shouldRetry $ \_ -> do
    try $ do
      resp <- W.postWith (commonOpts rt) "https://api.raindrop.io/rest/v1/raindrop" payload
      pure (BookmarkItemId . T.pack . show <$> resp ^? W.responseBody . key "item" . key "_id" . _Integral @_ @Int)
  case result of
    Left ex -> do
      logErrorN $ "Failed to add bookmark after retries: " <> T.pack (show ex)
      pure Nothing
    Right value -> pure value
raindrop creds (ArchiveBookmark bid) = do
  let rt = view raindropToken creds
      archiveId = view archiveCollectionId creds
  resp <-
    liftIO $ W.putWith (commonOpts rt) ("https://api.raindrop.io/rest/v1/raindrop/" <> T.unpack (bid ^. _BookmarkItemId)) $
      A.object
        [ "collection" A..= A.object ["$id" A..= archiveId]
        ]
  pure ((== Just True) $ resp ^? W.responseBody . key "result" . _Bool)
raindrop creds (BatchArchiveBookmarks bids) = do
  let rt = view raindropToken creds
      archiveId = view archiveCollectionId creds
      payload = A.object 
        [ "ids" A..= map (^. _BookmarkItemId) bids
        , "collection" A..= A.object ["$id" A..= archiveId]
        ]
  resp <- liftIO $ W.putWith (commonOpts rt) "https://api.raindrop.io/rest/v1/raindrops/-1" payload
  pure ((== Just True) $ resp ^? W.responseBody . key "result" . _Bool)
raindrop creds (RetrieveBookmarks page (RaindropCollectionId cid) mSearchParam) = do
  let rt = view raindropToken creds
      baseOpts = commonOpts rt & param "page" .~ [T.pack $ show page] & param "perpage" .~ ["50"]
      opts = case mSearchParam of
               Nothing -> baseOpts
               Just searchParam -> baseOpts & param "search" .~ [searchParam]
  resp <- liftIO $ W.getWith opts ("https://api.raindrop.io/rest/v1/raindrops/" <> T.unpack cid)
  let count = resp ^? W.responseBody . key "count" . _Integral @_ @Natural
  pure (fromMaybe 0 count, resp ^.. W.responseBody . key "items" . values . _JSON)
