{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Bookmark.Types
  ( RaindropToken(..),
    _RaindropToken,
    BookmarkItemId (..),
    _BookmarkItemId,
    RaindropCollectionId (..),
    _RaindropCollectionId,
    BookmarkItem (..),
    BookmarkRequest (..),
    BookmarkCredentials (..),
    URL(..),
    BookmarkItemBatch(..),
    biId,
    biLink,
    biExcerpt,
    biNote,
    biType,
    biTags,
    biRemoved,
    biCreated,
    biLastUpdate,
    biDomain,
    biTitle,
    biSort,
    biHighlights,
    biCollectionId,
    biImportant,
    batchTS,
    batchItems,
    batchTotal,
    raindropToken,
    archiveCollectionId
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Control.Lens (makePrisms, makeLenses, (^.))
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), Value(Object), object, (.:), (.=), (.:?), (.!=))
import Data.Aeson.Types (Parser)
import Control.Monad (mzero)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics
import Dhall (FromDhall)

newtype RaindropToken = RaindropToken Text deriving (Show, Eq, FromDhall, Generic)
makePrisms ''RaindropToken

newtype BookmarkItemId = BookmarkItemId Text deriving (Show, Eq, Ord)
makePrisms ''BookmarkItemId

newtype RaindropCollectionId = RaindropCollectionId Text deriving (Show, Eq)
makePrisms ''RaindropCollectionId

newtype URL = URL String deriving (Show, Eq, Generic)

data BookmarkCredentials = BookmarkCredentials
  { _raindropToken :: RaindropToken,
    _archiveCollectionId :: Natural
  } deriving (Generic)

makeLenses ''BookmarkCredentials

instance FromDhall BookmarkCredentials where

data BookmarkItem = BookmarkItem
  { _biId :: !BookmarkItemId,
    _biLink :: !Text,
    _biExcerpt :: !Text,
    _biNote :: !Text,
    _biType :: !Text,
    _biTags :: ![Text],
    _biRemoved :: !Bool,
    _biCreated :: !UTCTime,
    _biLastUpdate :: !UTCTime,
    _biDomain :: !Text,
    _biTitle :: !Text,
    _biSort :: !Int,
    _biHighlights :: ![Text],
    _biCollectionId :: !Int,
    _biImportant :: !Bool
  }
  deriving (Show, Eq)

makeLenses ''BookmarkItem

data BookmarkRequest a where
  AddBookmark :: Text -> Maybe Text -> [Text] -> BookmarkRequest (Maybe BookmarkItemId)
  ArchiveBookmark :: BookmarkItemId -> BookmarkRequest Bool
  BatchArchiveBookmarks :: [BookmarkItemId] -> BookmarkRequest Bool
  RetrieveBookmarks :: Natural -> RaindropCollectionId -> Maybe Text -> BookmarkRequest (Natural, [BookmarkItem])

data BookmarkItemBatch = BookmarkItemBatch
  { _batchTS :: POSIXTime
  , _batchItems :: [BookmarkItem]
  , _batchTotal :: Natural
  } deriving (Show)

makeLenses ''BookmarkItemBatch

instance FromJSON BookmarkItem where
  parseJSON (Object o) =
    BookmarkItem
      <$> (BookmarkItemId . Text.pack . show <$> (o .: "_id" :: Parser Int))
      <*> o .: "link"
      <*> o .: "excerpt"
      <*> o .: "note"
      <*> o .: "type"
      <*> o .: "tags"
      <*> o .: "removed"
      <*> ((o .: "created") >>= (iso8601ParseM . Text.unpack))
      <*> ((o .: "lastUpdate") >>= (iso8601ParseM . Text.unpack))
      <*> o .: "domain"
      <*> o .: "title"
      <*> o .: "sort"
      <*> o .: "highlights"
      <*> ((o .: "collection") >>= (.: "$id"))
      <*> (o .:? "important" .!= False)
  parseJSON _ = mzero

instance ToJSON BookmarkItem where
  toJSON item = object
    [ "_id" .= (read . Text.unpack $ _biId item ^. _BookmarkItemId :: Int)
    , "link" .= _biLink item
    , "excerpt" .= _biExcerpt item
    , "note" .= _biNote item
    , "type" .= _biType item
    , "tags" .= _biTags item
    , "removed" .= _biRemoved item
    , "created" .= (Text.pack . iso8601Show $ _biCreated item)
    , "lastUpdate" .= (Text.pack . iso8601Show $ _biLastUpdate item)
    , "domain" .= _biDomain item
    , "title" .= _biTitle item
    , "sort" .= _biSort item
    , "highlights" .= _biHighlights item
    , "collection" .= object ["$id" .= _biCollectionId item]
    , "important" .= _biImportant item
    ]
