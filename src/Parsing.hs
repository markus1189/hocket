{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Parsing (
  PocketItem (..),
  parseItems,
  parseItemsFile,
  parseItem,
  bestTitle,
  PocketAction (..)
) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import           Data.Map as M
import           Data.Text
import           GHC.Generics

data PocketItem =
  PocketItem { excerpt :: Text
             , favorite :: !Text
             , givenTitle :: !Text
             , givenUrl :: !Text
             , hasImage :: !Bool
             , hasVideo :: !Bool
             , isArticle :: !Bool
             , isIndex :: !Bool
             , itemId :: !Text
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

bestTitle :: PocketItem -> Text
bestTitle itm = (if givenTitle itm /= "" then givenTitle else resolvedTitle) itm

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
                     <*> o .: "item_id"
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

data PocketAction a = Archive a
                    | UnArchive a

instance ToJSON a => ToJSON (PocketAction a) where
  toJSON (Archive itmId) = object ["action" .= ("archive" :: String), "item_id" .= itmId]
  toJSON (UnArchive itmId) = object ["action" .= ("readd" :: String), "item_id" .= itmId]

parseItem :: BL.ByteString -> Maybe PocketItem
parseItem = decode

parseItems :: BL.ByteString -> Maybe (M.Map String PocketItem)
parseItems content = case eitherDecode content of
  Right result -> parseMaybe (.: "list") result
  Left err -> fail err

parseItemsFile :: FilePath -> IO (Maybe (M.Map String PocketItem))
parseItemsFile file = parseItems <$> BL.readFile file
