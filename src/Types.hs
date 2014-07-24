{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}

module Types (
  ConsumerKey (..),
  RequestToken (..),
  AccessToken (..),
  PocketCredentials (..),
  PocketAPIUrls (..),
  PocketItem (..),
  PocketItemId (..),
  PocketAction (..),
  PocketRequest (..),
  AsFormParams (..),
  Hocket,
  HocketC,
  HocketA,
  HocketCA,
  runHocket
) where

import           Control.Applicative ((<$>),(<*>))
import           Control.Monad (mzero)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.Aeson
import qualified Data.ByteString as BS
import           Data.Default
import           Data.Text (Text)
import           GHC.Generics
import           Network.Wreq (FormValue, FormParam((:=)))
import qualified Network.Wreq as W
import           Numeric.Natural

newtype ConsumerKey = ConsumerKey { getConsumerKey :: BS.ByteString } deriving FormValue
newtype RequestToken = RequestToken { getRequestToken :: BS.ByteString } deriving FormValue
newtype AccessToken = AccessToken { getAccessToken :: BS.ByteString } deriving FormValue

data PocketCredentials =
    PocketCredentials { credConsumerKey :: ConsumerKey
                      , credAccessToken :: AccessToken
                      }

data PocketAPIUrls = PocketAPIUrls { addEndpoint :: String
                                   , retrieveEndpoint :: String
                                   , modifyEndpoint :: String
                                   , requestEndpoint :: String
                                   , authorizeEndpoint :: String
                                   }

instance Default PocketAPIUrls where
    def = PocketAPIUrls { addEndpoint = "https://getpocket.com/v3/add"
                        , retrieveEndpoint = "https://getpocket.com/v3/get"
                        , modifyEndpoint = "https://getpocket.com/v3/send"
                        , requestEndpoint = "https://getpocket.com/v3/oauth/request"
                        , authorizeEndpoint = "https://getpocket.com/v3/oauth/authorize"
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

data PocketRequest a where
  AddItem :: BS.ByteString -> PocketRequest Bool
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

data PocketItem =
  PocketItem { excerpt :: Text
             , favorite :: !Text
             , givenTitle :: !Text
             , givenUrl :: !Text
             , hasImage :: !Bool
             , hasVideo :: !Bool
             , isArticle :: !Bool
             , isIndex :: !Bool
             , itemId :: !PocketItemId
             , resolvedId :: !Text
             , resolvedTitle :: !Text
             , resolvedUrl :: !Text
             , sortId :: Int
             , status :: !Text
             , timeAdded :: !Text
             , timeFavorited :: !Text
             , timeRead :: !Text
             , timeUpdated :: !Text
             , wordCount :: !Int
             } deriving (Show,Generic)

instance Eq PocketItem where
  (PocketItem {itemId = id1}) == (PocketItem {itemId = id2}) = id1 == id2

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

data PocketAction = Archive PocketItemId
                  | UnArchive PocketItemId
                  | Add PocketItemId

instance ToJSON PocketAction where
  toJSON (Archive itmId) = object ["action" .= ("archive" :: String), "item_id" .= itmId]
  toJSON (UnArchive itmId) = object ["action" .= ("readd" :: String), "item_id" .= itmId]
  toJSON (Add url) = object [ "action" .= ("add" :: String)
                            , "item_id" .= (""::String)
                            , "url" .= url]
