{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Pocket.Types (
  ConsumerKey (..),
  AccessToken (..),
  URL(..),

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
  idEq,

  PocketItemId (..),
  BatchAction (..),
  _Archive,
  _UnArchive,
  _Add,

  PocketRequest (..),

  AsFormParams (..),
  Hocket,
  runHocket,
) where

import           Control.Applicative ((<$>),(<*>), empty, pure, Alternative)
import           Control.Lens (view)
import           Control.Lens.TH
import           Control.Monad (mzero)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.Aeson
import           Data.Default
import           Data.Function (on)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Network.Wreq (FormValue, FormParam((:=)))

import Network.Pocket.Retrieve

s :: String -> String
s = id

newtype ConsumerKey = ConsumerKey Text deriving (Show, FormValue)
newtype AccessToken = AccessToken Text deriving (Show, FormValue)
newtype URL = URL String deriving (Show, Eq, FormValue, FromJSON, ToJSON)

data ItemStatus = Normal | IsArchived | ShouldBeDeleted deriving (Show, Eq, Enum, Bounded)

instance ToJSON ItemStatus where
  toJSON = toJSON . show . fromEnum

data Has = No | Yes | Is deriving (Show, Eq, Enum, Bounded)
parseItemHas :: Alternative f => Text -> f Has
parseItemHas "0" = pure No
parseItemHas "1" = pure Yes
parseItemHas "2" = pure Is
parseItemHas _ = empty

instance ToJSON Has where
  toJSON = toJSON . show . fromEnum

parseItemState :: Alternative f => Text -> f ItemStatus
parseItemState "0" = pure Normal
parseItemState "1" = pure IsArchived
parseItemState "2" = pure ShouldBeDeleted
parseItemState _ = empty

data PocketCredentials = PocketCredentials { _credConsumerKey :: ConsumerKey
                                           , _credAccessToken :: AccessToken
                                           }
makeLenses ''PocketCredentials

data PocketAPIUrls = PocketAPIUrls { _addEndpoint :: URL
                                   , _retrieveEndpoint :: URL
                                   , _modifyEndpoint :: URL
                                   , _requestEndpoint :: URL
                                   , _authorizeEndpoint :: URL
                                   }
makeLenses ''PocketAPIUrls

instance Default PocketAPIUrls where
    def = PocketAPIUrls { _addEndpoint = URL "https://getpocket.com/v3/add"
                        , _retrieveEndpoint = URL "https://getpocket.com/v3/get"
                        , _modifyEndpoint = URL "https://getpocket.com/v3/send"
                        , _requestEndpoint = URL "https://getpocket.com/v3/oauth/request"
                        , _authorizeEndpoint = URL "https://getpocket.com/v3/oauth/authorize"
                        }

type Hocket a = ReaderT (PocketCredentials,PocketAPIUrls) IO a

runHocket :: c -> ReaderT c IO a -> IO a
runHocket = flip runReaderT

newtype PocketItemId = PocketItemId Text
                     deriving (Show, FormValue, Eq, Ord)

instance ToJSON PocketItemId where
  toJSON (PocketItemId i) = toJSON i

data BatchAction = Archive PocketItemId
                 | UnArchive PocketItemId
                 | Add PocketItemId
                 | Rename PocketItemId Text
makePrisms ''BatchAction

instance ToJSON BatchAction where
  toJSON (Archive itmId) = object [ "action" .= s "archive"
                                  , "item_id" .= itmId]
  toJSON (UnArchive itmId) = object [ "action" .= s "readd"
                                    , "item_id" .= itmId]
  toJSON (Rename itmId title) = object [ "action" .= s "add"
                                       , "item_id" .= itmId
                                       , "title" .= title
                                       ]
  toJSON (Add url) = object [ "action" .= s "add"
                            , "item_id" .= s ""
                            , "url" .= url]

data PocketItem =
  PocketItem { _excerpt :: Text
             , _favorite :: !Bool
             , _givenTitle :: !Text
             , _givenUrl :: !URL
             , _hasImage :: !Has
             , _hasVideo :: !Has
             , _isArticle :: !Bool
             , _isIndex :: !Bool
             , _itemId :: !PocketItemId
             , _resolvedId :: !PocketItemId
             , _resolvedTitle :: !Text
             , _resolvedUrl :: !URL
             , _sortId :: Int
             , _status :: !ItemStatus
             , _timeAdded :: !POSIXTime
             , _timeFavorited :: !POSIXTime
             , _timeRead :: !POSIXTime
             , _timeUpdated :: !POSIXTime
             , _wordCount :: !Int
             } deriving (Show,Eq,Generic)
makeLenses ''PocketItem

idEq :: PocketItem -> PocketItem -> Bool
idEq = (==) `on` view itemId

truthy :: Text -> Bool
truthy "1" = True
truthy _ = False

parseTime :: Text -> POSIXTime
parseTime = fromIntegral . (read :: String -> Integer) . T.unpack

instance FromJSON PocketItem where
  parseJSON (Object o) = PocketItem
                     <$> o .: "excerpt"
                     <*> (truthy <$> o .: "favorite")
                     <*> o .: "given_title"
                     <*> o .: "given_url"
                     <*> (parseItemHas =<< (o .: "has_image"))
                     <*> (parseItemHas =<< (o .: "has_video"))
                     <*> (truthy <$> (o .: "is_article"))
                     <*> (truthy <$> (o .: "is_index"))
                     <*> (PocketItemId <$> o .: "item_id")
                     <*> (PocketItemId <$> o .: "resolved_id")
                     <*> o .: "resolved_title"
                     <*> o .: "resolved_url"
                     <*> o .: "sort_id"
                     <*> (parseItemState =<< o .: "status")
                     <*> (parseTime <$> o .: "time_added")
                     <*> (parseTime <$> o .: "time_favorited")
                     <*> (parseTime <$> o .: "time_read")
                     <*> (parseTime <$> o .: "time_updated")
                     <*> (read <$> (o .: "word_count"))
  parseJSON _ = mzero

instance ToJSON PocketItem

instance ToJSON NominalDiffTime where
  toJSON = toJSON . (floor :: NominalDiffTime -> Integer)

data PocketRequest a where
  AddItem :: Text -> PocketRequest Bool
  ArchiveItem :: PocketItemId -> PocketRequest Bool
  RenameItem :: PocketItemId -> Text -> PocketRequest Bool
  Batch :: [BatchAction] -> PocketRequest [Bool]
  RetrieveItems :: RetrieveConfig -> PocketRequest [PocketItem]
  Raw :: PocketRequest a -> PocketRequest Text

instance (AsFormParams a, AsFormParams b) => AsFormParams (a,b) where
  toFormParams (x,y) = toFormParams x ++ toFormParams y

instance AsFormParams (PocketRequest a) where
  toFormParams (Raw x) = toFormParams x
  toFormParams (Batch pas) = ["actions" := encode pas]
  toFormParams (AddItem u) = [ "url" := u ]
  toFormParams (RenameItem i txt) = toFormParams $ Batch [Rename i txt]
  toFormParams (ArchiveItem i) = toFormParams $ Batch [Archive i]
  toFormParams (RetrieveItems c) = toFormParams c

instance AsFormParams PocketCredentials where
  toFormParams (PocketCredentials ck t) = [ "access_token" := t
                                          , "consumer_key" := ck
                                          ]
