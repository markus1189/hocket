{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Raindrop.Types
  ( RaindropToken(..),
    _RaindropToken,
    RaindropItemId (..),
    _RaindropItemId,
    RaindropCollectionId (..),
    _RaindropCollectionId,
    RaindropItem (..),
    RaindropRequest (..),
    toPocketItem,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Control.Lens (makePrisms, (^.))
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), Value(Object), object, (.:), (.=))
import Control.Monad (mzero)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.Pocket.Types (PocketItem(..), PocketItemId(..), URL(..))

newtype RaindropToken = RaindropToken Text deriving (Show, Eq)
makePrisms ''RaindropToken

newtype RaindropItemId = RaindropItemId Int deriving (Show, Eq)
makePrisms ''RaindropItemId

newtype RaindropCollectionId = RaindropCollectionId Text deriving (Show, Eq)
makePrisms ''RaindropCollectionId

data RaindropRetrieveConfig = RaindropRetrieveConfig
  { _raindropRetrieveSort :: !(Maybe Text),
    _raindropRetrievePerPage :: !(Maybe Natural),
    _raindropRetrievePage :: !(Maybe Natural),
    _raindropRetrieveSearch :: !(Maybe Text)
  } deriving (Show, Eq)

data RaindropItem = RaindropItem
  { _riId :: !RaindropItemId,
    _riLink :: !Text,
    _riExcerpt :: !Text,
    _riNote :: !Text,
    _riType :: !Text,
    _riTags :: ![Text],
    _riRemoved :: !Bool,
    _riCreated :: !UTCTime,
    _riLastUpdate :: !UTCTime,
    _riDomain :: !Text,
    _riTitle :: !Text,
    _riSort :: !Int,
    _riHighlights :: ![Text], -- Assuming highlights are represented by Text
    _riCollectionId :: !Int -- Top-level collectionId
  }
  deriving (Show, Eq)

data RaindropRequest a where
  AddRaindrop :: Text -> RaindropRequest (Maybe RaindropItemId)
  ArchiveRaindrop :: RaindropItemId -> RaindropRequest Bool
  RetrieveRaindrops :: Natural -> RaindropCollectionId -> RaindropRequest (Natural, [RaindropItem])

instance FromJSON RaindropItem where
  parseJSON (Object o) =
    RaindropItem
      <$> (RaindropItemId <$> o .: "_id")
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
      <*> o .: "collectionId"
  parseJSON _ = mzero

instance ToJSON RaindropItem where
  toJSON item = object
    [ "_id" .= (_riId item ^. _RaindropItemId)
    , "link" .= _riLink item
    , "excerpt" .= _riExcerpt item
    , "note" .= _riNote item
    , "type" .= _riType item
    , "tags" .= _riTags item
    , "removed" .= _riRemoved item
    , "created" .= (Text.pack . iso8601Show $ _riCreated item)
    , "lastUpdate" .= (Text.pack . iso8601Show $ _riLastUpdate item)
    , "domain" .= _riDomain item
    , "title" .= _riTitle item
    , "sort" .= _riSort item
    , "highlights" .= _riHighlights item
    , "collectionId" .= _riCollectionId item
    ]

toPocketItem :: RaindropItem -> PocketItem
toPocketItem ri = PocketItem
  { _givenTitle = _riTitle ri
  , _givenUrl = URL (Text.unpack (_riLink ri))
  , _itemId = PocketItemId (Text.pack (show (_riId ri ^. _RaindropItemId)))
  , _resolvedTitle = _riTitle ri -- Using Raindrop title as resolved title
  , _resolvedUrl = URL (Text.unpack (_riLink ri)) -- Using Raindrop link as resolved URL
  , _timeAdded = utcTimeToPOSIXSeconds (_riCreated ri)
  , _timeUpdated = utcTimeToPOSIXSeconds (_riLastUpdate ri)
  , _redditCommentCount = Nothing
  }
