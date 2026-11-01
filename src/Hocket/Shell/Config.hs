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
    getConfigPath,
    ensureSchemaFile,
  )
where

import Control.Lens (makeLenses, makePrisms)
import Control.Monad (unless)
import Data.Text (Text)
import Dhall (FromDhall)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

newtype RaindropToken = RaindropToken Text deriving (Show, Eq, FromDhall, Generic)

makePrisms ''RaindropToken

data BookmarkCredentials = BookmarkCredentials
  { _raindropToken :: RaindropToken,
    _archiveCollectionId :: Natural
  }
  deriving (Generic)

makeLenses ''BookmarkCredentials

instance FromDhall BookmarkCredentials

getConfigPath :: IO FilePath
getConfigPath = do
  xdgConfigDir <- getXdgDirectory XdgConfig "hocket"
  let xdgConfigPath = xdgConfigDir </> "config.dhall"
      legacyConfigPath = "./config.dhall"

  legacyExists <- doesFileExist legacyConfigPath
  xdgExists <- doesFileExist xdgConfigPath

  case (legacyExists, xdgExists) of
    (True, False) -> do
      hPutStrLn stderr $ "Warning: Using legacy config location: " <> legacyConfigPath
      hPutStrLn stderr $ "Consider moving config to: " <> xdgConfigPath
      return legacyConfigPath
    (False, True) -> return xdgConfigPath
    (True, True) -> do
      hPutStrLn stderr $ "Warning: Found config in both locations, using XDG: " <> xdgConfigPath
      return xdgConfigPath
    (False, False) -> do
      createDirectoryIfMissing True xdgConfigDir
      return xdgConfigPath

ensureSchemaFile :: IO ()
ensureSchemaFile = do
  xdgConfigDir <- getXdgDirectory XdgConfig "hocket"
  let xdgSchemaPath = xdgConfigDir </> "schema.dhall"
      legacySchemaPath = "./schema.dhall"

  xdgSchemaExists <- doesFileExist xdgSchemaPath
  legacySchemaExists <- doesFileExist legacySchemaPath

  unless xdgSchemaExists $ do
    createDirectoryIfMissing True xdgConfigDir
    if legacySchemaExists
      then do
        legacyContent <- readFile legacySchemaPath
        writeFile xdgSchemaPath legacyContent
      else writeFile xdgSchemaPath "{ _raindropToken : Text, _archiveCollectionId : Natural }\n"
