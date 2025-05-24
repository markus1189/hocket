{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.Raindrop where

import Control.Lens ((&), (.~), (^?))
import Control.Lens.Operators ((^.), (^..))
import qualified Data.Aeson as A
import Data.Aeson.Lens (AsJSON (_JSON), AsValue (_Bool), key, values, _Integral)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Bookmark.Types (BookmarkItemId (..), BookmarkRequest (..), RaindropCollectionId (RaindropCollectionId), RaindropToken (..), _BookmarkItemId)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as HCTLS (tlsManagerSettings)
import Network.Wreq (param)
import qualified Network.Wreq as W
import Numeric.Natural (Natural)

archiveCollectionId :: Natural
archiveCollectionId = 55615716

commonOpts :: RaindropToken -> W.Options
commonOpts (RaindropToken t) =
  W.defaults
    & W.manager .~ Left (HCTLS.tlsManagerSettings {HC.managerResponseTimeout = HC.responseTimeoutDefault})
    & W.header "Authorization" .~ ["Bearer " <> TE.encodeUtf8 t]

raindrop :: RaindropToken -> BookmarkRequest a -> IO a
raindrop rt (AddBookmark link) = do
  resp <- W.postWith (commonOpts rt) "https://api.raindrop.io/rest/v1/raindrop" $ A.object ["link" A..= link, "pleaseParse" A..= A.object []]
  pure (BookmarkItemId . T.pack . show <$> resp ^? W.responseBody . key "item" . key "_id" . _Integral @_ @Int)
raindrop rt (ArchiveBookmark bid) = do
  resp <-
    W.putWith (commonOpts rt) ("https://api.raindrop.io/rest/v1/raindrop/" <> T.unpack (bid ^. _BookmarkItemId)) $
      A.object
        [ "collection" A..= A.object ["$id" A..= archiveCollectionId]
        ]
  pure ((== Just True) $ resp ^? W.responseBody . key "result" . _Bool)
raindrop rt (RetrieveBookmarks page (RaindropCollectionId cid)) = do
  let opts = commonOpts rt & param "page" .~ [T.pack $ show page] & param "perpage" .~ ["50"]
  resp <- W.getWith opts ("https://api.raindrop.io/rest/v1/raindrops/" <> T.unpack cid)
  let count = resp ^? W.responseBody . key "count" . _Integral @_ @Natural
  pure (fromMaybe 0 count, resp ^.. W.responseBody . key "items" . values . _JSON)
