{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Network.Pocket.Types (
  ConsumerKey (..),
  AccessToken (..),
  URL(..),

  PocketCredentials (..),

  PocketAPIUrls,
  addEndpoint,
  retrieveEndpoint,
  modifyEndpoint,
  requestEndpoint,
  authorizeEndpoint,

  PocketItem (..),
  itemId,
  givenTitle,
  resolvedTitle,
  resolvedUrl,
  timeAdded,
  timeUpdated,
  redditCommentCount,
  isRedditUrl,
  resolvedOrGivenUrl,

  idEq,

  PocketItemId (..),
  _PocketItemId,
  ItemStatus (..),
  BatchAction (..),
  _Archive,
  _UnArchive,
  _Add,

  PocketRequest (..),

  AsFormParams (..),

  PocketItemBatch(PocketItemBatch),
  batchItems,
  batchTS,
  batchTotal,

  Tag (Tag),
  tagName,
  tagId,

  RedditCommentCount (..),
  subredditAndArticleId
) where

import           Control.Lens (view, (^.))
import           Control.Lens.TH
import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Default
import           Data.Function (on)
import           Data.List (isInfixOf)
import qualified Data.List.Split as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Time.Clock.POSIX
import           Dhall (FromDhall)
import           GHC.Generics
import           Network.Wreq (FormValue, FormParam((:=)))

import           Network.Pocket.Retrieve
import Numeric.Natural (Natural)

s :: String -> String
s = id

newtype ConsumerKey = ConsumerKey LT.Text deriving (Show, FormValue, FromJSON, Generic, FromDhall)
newtype AccessToken = AccessToken LT.Text deriving (Show, FormValue, FromJSON, Generic, FromDhall)
newtype URL = URL String deriving (Show, Eq, FormValue, FromJSON, ToJSON, Generic)

data ItemStatus = Normal | IsArchived | ShouldBeDeleted deriving (Show, Eq, Enum, Bounded)

instance ToJSON ItemStatus where
  toJSON = toJSON . show . fromEnum

data PocketCredentials = PocketCredentials { consumerKey :: ConsumerKey
                                           , accessToken :: AccessToken
                                           } deriving (Generic)
instance FromDhall PocketCredentials
makeLenses ''PocketCredentials

instance FromJSON PocketCredentials where
  parseJSON (Object o) = PocketCredentials <$> o .: "consumer-key"
                                           <*> o .: "access-token"
  parseJSON _ = mempty

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

newtype PocketItemId = PocketItemId Text
                     deriving (Show, FormValue, Eq, Ord)
makePrisms ''PocketItemId

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

data Tag = Tag { _tagName :: !Text, _tagId :: !Text } deriving (Eq,Show)
makeLenses ''Tag

instance ToJSON Tag where
  toJSON (Tag name ident) = object [ "tag" .= name, "item_id" .= ident]

instance FromJSON Tag where
  parseJSON (Object o) = Tag <$> o .: "tag" <*> o .: "item_id"
  parseJSON _ = mzero

data PocketItem =
  PocketItem { _givenTitle :: !Text
             , _givenUrl :: !URL
             , _itemId :: !PocketItemId
             , _resolvedTitle :: !Text
             , _resolvedUrl :: !URL
             , _timeAdded :: !POSIXTime
             , _timeUpdated :: !POSIXTime
             , _redditCommentCount :: Maybe Integer
             } deriving (Show,Eq,Generic)
makeLenses ''PocketItem

idEq :: PocketItem -> PocketItem -> Bool
idEq = (==) `on` view itemId

parseTime :: Text -> POSIXTime
parseTime = fromIntegral . (read :: String -> Integer) . T.unpack

instance FromJSON PocketItem where
  parseJSON (Object o) = PocketItem
                     <$> o .: "given_title"
                     <*> o .: "given_url"
                     <*> (PocketItemId <$> o .: "item_id")
                     <*> o .: "resolved_title"
                     <*> o .: "resolved_url"
                     <*> (parseTime <$> o .: "time_added")
                     <*> (parseTime <$> o .: "time_updated")
                     <*> pure Nothing
  parseJSON _ = mzero

instance ToJSON PocketItem

data PocketRequest a where
  AddItem :: Text -> PocketRequest Bool
  ArchiveItem :: PocketItemId -> PocketRequest Bool
  RenameItem :: PocketItemId -> Text -> PocketRequest Bool
  Batch :: [BatchAction] -> PocketRequest [Bool]
  RetrieveItems :: RetrieveConfig -> PocketRequest PocketItemBatch
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

data PocketItemBatch = PocketItemBatch { _batchTS :: POSIXTime
                                       , _batchItems :: [PocketItem]
                                       , _batchTotal :: Natural
                                       } deriving (Show)
makeLenses ''PocketItemBatch

subredditAndArticleId :: PocketItem -> Maybe (String, String)
subredditAndArticleId item = if isRedditUrl rurl
                               then (,) <$> extractSubreddit rurl <*> extractArticleId rurl
                               else Nothing
  where rurl = item ^. resolvedUrl

newtype RedditCommentCount = RedditCommentCount  Integer deriving (Show, Eq)

isRedditUrl :: URL -> Bool
isRedditUrl (URL url) = prefix `isInfixOf` url
  where prefix = "reddit.com/r/"

extractSubreddit :: URL -> Maybe String
extractSubreddit (URL url) = if length splits /= 9
                                then Nothing
                                else Just $ splits !! 4
  where splits = S.splitOn "/" url

extractArticleId :: URL -> Maybe String
extractArticleId (URL url ) = if length splits /= 9
                                 then Nothing
                                 else Just $ splits !! 6
  where splits = S.splitOn "/" url

resolvedOrGivenUrl :: PocketItem -> URL
resolvedOrGivenUrl pit = if null resolved then pit ^. givenUrl else URL resolved
  where
    (URL resolved) = pit ^. resolvedUrl
