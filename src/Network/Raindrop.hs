{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.Raindrop where

import Control.Lens ((&), (.~), (^?), view)
import Control.Lens.Operators ((^.), (^..))
import qualified Data.Aeson as A
import Data.Aeson.Lens (AsJSON (_JSON), AsValue (_Bool), key, values, _Integral)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Bookmark.Types (BookmarkCredentials, BookmarkItemId (..), BookmarkRequest (..), RaindropCollectionId (RaindropCollectionId), RaindropToken (..), _BookmarkItemId, raindropToken, archiveCollectionId)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as HCTLS (tlsManagerSettings)
import Network.Wreq (param)
import qualified Network.Wreq as W
import Numeric.Natural (Natural)

commonOpts :: RaindropToken -> W.Options
commonOpts (RaindropToken t) =
  W.defaults
    & W.manager .~ Left (HCTLS.tlsManagerSettings {HC.managerResponseTimeout = HC.responseTimeoutDefault})
    & W.header "Authorization" .~ ["Bearer " <> TE.encodeUtf8 t]

raindrop :: BookmarkCredentials -> BookmarkRequest a -> IO a
raindrop creds (AddBookmark link) = do
  let rt = view raindropToken creds
  resp <- W.postWith (commonOpts rt) "https://api.raindrop.io/rest/v1/raindrop" $ A.object ["link" A..= link, "pleaseParse" A..= A.object []]
  pure (BookmarkItemId . T.pack . show <$> resp ^? W.responseBody . key "item" . key "_id" . _Integral @_ @Int)
raindrop creds (ArchiveBookmark bid) = do
  let rt = view raindropToken creds
      archiveId = view archiveCollectionId creds
  resp <-
    W.putWith (commonOpts rt) ("https://api.raindrop.io/rest/v1/raindrop/" <> T.unpack (bid ^. _BookmarkItemId)) $
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
  resp <- W.putWith (commonOpts rt) "https://api.raindrop.io/rest/v1/raindrops/-1" payload
  pure ((== Just True) $ resp ^? W.responseBody . key "result" . _Bool)
raindrop creds (RetrieveBookmarks page (RaindropCollectionId cid)) = do
  let rt = view raindropToken creds
      opts = commonOpts rt & param "page" .~ [T.pack $ show page] & param "perpage" .~ ["50"]
  resp <- W.getWith opts ("https://api.raindrop.io/rest/v1/raindrops/" <> T.unpack cid)
  let count = resp ^? W.responseBody . key "count" . _Integral @_ @Natural
  pure (fromMaybe 0 count, resp ^.. W.responseBody . key "items" . values . _JSON)
