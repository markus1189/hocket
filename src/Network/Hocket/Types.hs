{-# LANGUAGE TemplateHaskell #-}

module Network.Hocket.Types
  ( Hocket,
    runHocket,
    HocketEnv(..),
    hocketEnvPocketCredentials,
    hocketEnvPocketAPIUrls,
    hocketEnvRaindropToken,
  )
where

import Control.Lens.TH ( makeLenses )
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Network.Pocket.Types (PocketAPIUrls, PocketCredentials)
import Network.Raindrop.Types (RaindropToken)

data HocketEnv = HocketEnv
  { _hocketEnvPocketCredentials :: !PocketCredentials,
    _hocketEnvPocketAPIUrls :: !PocketAPIUrls,
    _hocketEnvRaindropToken :: !RaindropToken
  }

makeLenses ''HocketEnv

type Hocket a = ReaderT HocketEnv IO a

runHocket :: c -> ReaderT c IO a -> IO a
runHocket = flip runReaderT
