{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The subset of the Raindrop.io REST API hocket uses, as a servant client.
module Network.Raindrop.Api
  ( RaindropRoutes (..),
    raindropApi,
    raindropEnv,
    Items (..),
    Created (..),
    ApiResult (..),
    NewRaindrop (..),
    RaindropPatch (..),
    BatchMove (..),
  )
where

import Data.Aeson
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Generics (Generic)
import Network.Bookmark.Types (BookmarkItem)
import Network.HTTP.Client.TLS (getGlobalManager)
import Numeric.Natural (Natural)
import Servant.API
import Servant.Client

data RaindropRoutes mode = RaindropRoutes
  { listItems ::
      mode
        :- "raindrops"
          :> Capture "collection" Text
          :> QueryParam' '[Required, Strict] "page" Natural
          :> QueryParam' '[Required, Strict] "perpage" Int
          :> QueryParam "search" Text
          :> Get '[JSON] Items,
    addItem ::
      mode :- "raindrop" :> ReqBody '[JSON] NewRaindrop :> Post '[JSON] Created,
    updateItem ::
      mode :- "raindrop" :> Capture "id" Text :> ReqBody '[JSON] RaindropPatch :> Put '[JSON] ApiResult,
    -- | Moves the listed ids, wherever they are; @-1@ means "any collection".
    moveItems ::
      mode :- "raindrops" :> "-1" :> ReqBody '[JSON] BatchMove :> Put '[JSON] ApiResult
  }
  deriving (Generic)

type RaindropAPI =
  "rest" :> "v1" :> Header' '[Required, Strict] "Authorization" Text :> NamedRoutes RaindropRoutes

raindropApi :: Text -> RaindropRoutes (AsClientT ClientM)
raindropApi token = client (Proxy @RaindropAPI) ("Bearer " <> token)

-- | Shares http-client-tls's process-wide manager, so connections are reused
-- across requests without threading a 'ClientEnv' through the UI state.
raindropEnv :: IO ClientEnv
raindropEnv = do
  mgr <- getGlobalManager
  pure (mkClientEnv mgr (BaseUrl Https "api.raindrop.io" 443 ""))

data Items = Items {itemsCount :: Natural, itemsItems :: [BookmarkItem]}

instance FromJSON Items where
  parseJSON = withObject "Items" $ \o -> Items <$> o .: "count" <*> o .: "items"

newtype Created = Created Int

instance FromJSON Created where
  parseJSON = withObject "Created" $ \o -> o .: "item" >>= withObject "item" (fmap Created . (.: "_id"))

newtype ApiResult = ApiResult Bool

instance FromJSON ApiResult where
  parseJSON = withObject "ApiResult" $ fmap ApiResult . (.: "result")

data NewRaindrop = NewRaindrop {nrLink :: Text, nrCollection :: Text, nrTags :: [Text]}

instance ToJSON NewRaindrop where
  toJSON (NewRaindrop l c t) =
    object ["link" .= l, "collection" .= c, "tags" .= t, "pleaseParse" .= object []]

data RaindropPatch = MoveTo Natural | SetReminderAt UTCTime | ClearReminder

instance ToJSON RaindropPatch where
  toJSON (MoveTo cid) = object ["collection" .= object ["$id" .= cid]]
  toJSON (SetReminderAt t) =
    object ["reminder" .= object ["date" .= T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%03qZ" t)]]
  toJSON ClearReminder = object ["reminder" .= Null]

data BatchMove = BatchMove {bmIds :: [Text], bmTarget :: Natural}

instance ToJSON BatchMove where
  toJSON (BatchMove ids cid) = object ["ids" .= ids, "collection" .= object ["$id" .= cid]]
