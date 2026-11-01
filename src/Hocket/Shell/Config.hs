{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The Raindrop account, as config.dhall holds it: kept out of the domain
-- types so that only the adapter and the config loader see it (plan F1).
module Hocket.Shell.Config
  ( RaindropToken (..),
    _RaindropToken,
    BookmarkCredentials (..),
    raindropToken,
    archiveCollectionId,
  )
where

import Control.Lens (makeLenses, makePrisms)
import Data.Text (Text)
import Dhall (FromDhall)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

newtype RaindropToken = RaindropToken Text deriving (Show, Eq, FromDhall, Generic)

makePrisms ''RaindropToken

data BookmarkCredentials = BookmarkCredentials
  { _raindropToken :: RaindropToken,
    _archiveCollectionId :: Natural
  }
  deriving (Generic)

makeLenses ''BookmarkCredentials

instance FromDhall BookmarkCredentials
