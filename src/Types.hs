{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types (
  ConsumerKey (..),
  AccessToken (..),

  PocketCredentials (..),
  credConsumerKey,
  credAccessToken,

  PocketAPIUrls,
  addEndpoint,
  retrieveEndpoint,
  modifyEndpoint,
  requestEndpoint,
  authorizeEndpoint,


  PocketItem (..),
  excerpt,
  favorite,
  givenTitle,
  givenUrl,
  hasImage,
  hasVideo,
  isArticle,
  isIndex,
  itemId,
  resolvedId,
  resolvedTitle,
  resolvedUrl,
  sortId,
  status,
  timeAdded,
  timeFavorited,
  timeRead,
  timeUpdated,
  wordCount,

  PocketItemId (..),
  PocketAction,
  _Archive,
  _UnArchive,
  _Add,

  PocketRequest (..),

  AsFormParams (..),
  Hocket,
  HocketC,
  HocketA,
  HocketCA,
  runHocket
) where

import           Control.Applicative ((<$>),(<*>))
import           Control.Lens (view)
import           Control.Lens.TH
import           Control.Monad (mzero)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.Aeson
import           Data.Default
import           Data.Function (on)
import           Data.Text (Text)
import           GHC.Generics
import           Network.Wreq (FormValue, FormParam((:=)))
import qualified Network.Wreq as W
import           Numeric.Natural

newtype ConsumerKey = ConsumerKey Text deriving FormValue
newtype AccessToken = AccessToken Text deriving FormValue

data PocketCredentials = PocketCredentials { _credConsumerKey :: ConsumerKey
                                           , _credAccessToken :: AccessToken
                                           }
makeLenses ''PocketCredentials

data PocketAPIUrls = PocketAPIUrls { _addEndpoint :: String
                                   , _retrieveEndpoint :: String
                                   , _modifyEndpoint :: String
                                   , _requestEndpoint :: String
                                   , _authorizeEndpoint :: String
                                   }
makeLenses ''PocketAPIUrls

instance Default PocketAPIUrls where
    def = PocketAPIUrls { _addEndpoint = "https://getpocket.com/v3/add"
                        , _retrieveEndpoint = "https://getpocket.com/v3/get"
                        , _modifyEndpoint = "https://getpocket.com/v3/send"
                        , _requestEndpoint = "https://getpocket.com/v3/oauth/request"
                        , _authorizeEndpoint = "https://getpocket.com/v3/oauth/authorize"
                        }

type Hocket c a = ReaderT c IO a

type HocketC a = Hocket PocketCredentials a
type HocketA a = Hocket PocketAPIUrls a
type HocketCA a = Hocket (PocketCredentials,PocketAPIUrls) a

runHocket :: c -> ReaderT c IO a -> IO a
runHocket = flip runReaderT

newtype PocketItemId = PocketItemId Text
                     deriving (Show, FormValue, Eq)

instance ToJSON PocketItemId where
  toJSON (PocketItemId i) = toJSON i

data PocketAction = Archive PocketItemId
                  | UnArchive PocketItemId
                  | Add PocketItemId
makePrisms ''PocketAction

instance ToJSON PocketAction where
  toJSON (Archive itmId) = object ["action" .= ("archive" :: String), "item_id" .= itmId]
  toJSON (UnArchive itmId) = object ["action" .= ("readd" :: String), "item_id" .= itmId]
  toJSON (Add url) = object [ "action" .= ("add" :: String)
                            , "item_id" .= (""::String)
                            , "url" .= url]

data PocketItem =
  PocketItem { _excerpt :: Text
             , _favorite :: !Text
             , _givenTitle :: !Text
             , _givenUrl :: !Text
             , _hasImage :: !Bool
             , _hasVideo :: !Bool
             , _isArticle :: !Bool
             , _isIndex :: !Bool
             , _itemId :: !PocketItemId
             , _resolvedId :: !Text
             , _resolvedTitle :: !Text
             , _resolvedUrl :: !Text
             , _sortId :: Int
             , _status :: !Text
             , _timeAdded :: !Text
             , _timeFavorited :: !Text
             , _timeRead :: !Text
             , _timeUpdated :: !Text
             , _wordCount :: !Int
             } deriving (Show,Generic)

makeLenses ''PocketItem

instance Eq PocketItem where
  (==) = (==) `on` view itemId

truthy :: Text -> Bool
truthy "1" = True
truthy _ = False

instance FromJSON PocketItem where
  parseJSON (Object o) = PocketItem
                     <$> o .: "excerpt"
                     <*> o .: "favorite"
                     <*> o .: "given_title"
                     <*> o .: "given_url"
                     <*> (truthy <$> (o .: "has_image"))
                     <*> (truthy <$> (o .: "has_video"))
                     <*> (truthy <$> (o .: "is_article"))
                     <*> (truthy <$> (o .: "is_index"))
                     <*> (PocketItemId <$> (o .: "item_id"))
                     <*> o .: "resolved_id"
                     <*> o .: "resolved_title"
                     <*> o .: "resolved_url"
                     <*> o .: "sort_id"
                     <*> o .: "status"
                     <*> o .: "time_added"
                     <*> o .: "time_favorited"
                     <*> o .: "time_read"
                     <*> o .: "time_updated"
                     <*> (read <$> (o .: "word_count"))
  parseJSON _ = mzero

instance ToJSON PocketItem

data PocketRequest a where
  AddItem :: Text -> PocketRequest Bool
  ArchiveItem :: PocketItemId -> PocketRequest Bool
  Batch :: [PocketAction] -> PocketRequest [Bool]
  RetrieveItems :: Maybe (Natural,Natural) -> PocketRequest [PocketItem]

class AsFormParams a where
  toFormParams :: a -> [W.FormParam]

instance (AsFormParams a, AsFormParams b) => AsFormParams (a,b) where
  toFormParams (x,y) = toFormParams x ++ toFormParams y

instance AsFormParams (PocketRequest a) where
  toFormParams (Batch pas) = ["actions" := encode pas]
  toFormParams (AddItem u) = [ "url" := u ]
  toFormParams (ArchiveItem i) = toFormParams $ Batch [Archive i]
  toFormParams (RetrieveItems _) = [ "detailType" := ("simple" :: Text)
                                   , "sort" := ("newest" :: Text)
                                   ]

instance AsFormParams PocketCredentials where
  toFormParams (PocketCredentials ck t) = [ "access_token" := t
                                          , "consumer_key" := ck
                                          ]
