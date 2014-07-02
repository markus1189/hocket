{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types (
  ConsumerKey (..),
  RequestToken (..),
  AccessToken (..),
  PocketCredentials (..),
  PocketAPIUrls (..),
  Hocket,
  HocketC,
  HocketA,
  HocketCA,
  runHocket
) where

import Data.Default
import qualified Data.ByteString as BS
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

newtype ConsumerKey = ConsumerKey { getConsumerKey :: BS.ByteString }
newtype RequestToken = RequestToken { getRequestToken :: BS.ByteString }
newtype AccessToken = AccessToken { getAccessToken :: BS.ByteString }

data PocketCredentials =
    PocketCredentials { credConsumerKey :: ConsumerKey
                      , credAccessToken :: AccessToken
                      , credShellCmd :: String
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
