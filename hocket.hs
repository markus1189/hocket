{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Network.HTTP.Conduit as HC
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as CS
import Network.HTTP.Types.Status
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Applicative
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

newtype ConsumerKey = ConsumerKey { getConsumerKey :: BS.ByteString }
newtype RequestToken = RequestToken { getRequestToken :: BS.ByteString }
newtype AccessToken = AccessToken { getAccessToken :: BS.ByteString }

data PocketCredentials = PocketCredentials {
      consumerKey :: ConsumerKey
    , accessToken :: AccessToken
    }

data PocketAPIUrls = PocketAPIUrls { addEndpoint :: String
                                   , retrieveEndpoint :: String
                                   , modifyEndpoint :: String
                                   , requestEndpoint :: String
                                   , authorizeEndpoint :: String
                                   }

defaultPocketAPIUrls :: PocketAPIUrls
defaultPocketAPIUrls =
    PocketAPIUrls { addEndpoint = "https://getpocket.com/v3/add"
                  , retrieveEndpoint = "https://getpocket.com/v3/get"
                  , modifyEndpoint = "https://getpocket.com/v3/send"
                  , requestEndpoint = "https://getpocket.com/v3/oauth/request"
                  , authorizeEndpoint = "https://getpocket.com/v3/oauth/authorize"
                  }

type Hocket c a = ReaderT c IO a

runHocket = flip runReaderT

defaultConsumerKey :: ConsumerKey
defaultConsumerKey = ConsumerKey "17131-ef4b2d99253edd194ed5ad87"

accToken = unsafePerformIO $ (head . CS.lines) <$> BS.readFile "accesstoken.txt"

pocketCredentials :: PocketCredentials
pocketCredentials = PocketCredentials
                      defaultConsumerKey
                      (AccessToken accToken)

authorize :: ConsumerKey -> PocketAPIUrls -> IO AccessToken
authorize consumeKey api = runHocket api $ do
  requestToken <- obtainRequestToken consumeKey
  let redirUrl = createDefaultRedirectUrl consumeKey
  _ <- liftIO $ do
    C.putStrLn . createRedirectUrl requestToken . toLazyBS $ redirUrl
    putStrLn "Press enter after finishing authorization of the app via the link above."
    getLine
  obtainPocketAccessToken consumeKey requestToken

obtainRequestToken :: ConsumerKey -> Hocket PocketAPIUrls RequestToken
obtainRequestToken ckey = do
  reqUrl <- reader requestEndpoint
  let redirUrl = createDefaultRedirectUrl ckey
  respBody <- requestBodySkeleton reqUrl (keyValuePairs redirUrl)
  return . RequestToken . toStrictBS . extractValue $ respBody
    where keyValuePairs redirUrl = [ ("consumer_key", getConsumerKey ckey)
                                   , ("redirect_uri", redirUrl)
                                   ]

createDefaultRedirectUrl :: ConsumerKey -> BS.ByteString
createDefaultRedirectUrl ck = BS.concat [ "pocketapp", appID ]
  where appID = (head . CS.split '-' $ getConsumerKey ck)

createRedirectUrl :: RequestToken -> BL.ByteString -> BL.ByteString
createRedirectUrl requestToken redirectUri =
    BL.concat [ "https://getpocket.com/auth/authorize?"
              , "request_token="
              , BL.pack . BS.unpack . getRequestToken $ requestToken
              , "&redirect_uri="
              , redirectUri
              ]

obtainPocketAccessToken :: ConsumerKey -> RequestToken -> Hocket PocketAPIUrls AccessToken
obtainPocketAccessToken ckey requestToken = do
  authUrl <- reader authorizeEndpoint
  respBody <- liftIO $ requestBodySkeleton authUrl keyValuePairs
  return $ AccessToken . toStrictBS . extractValue $ respBody
      where keyValuePairs = [ ("consumer_key", getConsumerKey ckey)
                            , ("code", getRequestToken requestToken)
                            ]

requestSkeleton url keyValuePairs = do
  request' <- HC.parseUrl url
  let request = setTimeOut 10
              . useHttps
              . HC.urlEncodedBody keyValuePairs
              $ request'
  HC.withManager $ HC.httpLbs request

requestBodySkeleton url kvs = HC.responseBody <$> requestSkeleton url kvs

toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BS.concat . BL.toChunks

toLazyBS :: BS.ByteString -> BL.ByteString
toLazyBS = BL.pack . BS.unpack

extractValue :: BL.ByteString -> BL.ByteString
extractValue = head . tail . C.split '='

useHttps :: HC.Request m -> HC.Request m
useHttps req = req { HC.secure = True }

setTimeOut :: Int -> HC.Request a -> HC.Request a
setTimeOut n req = req { HC.responseTimeout = Just n }

retrieveList :: Int -> Hocket (PocketCredentials,PocketAPIUrls) BL.ByteString
retrieveList n = do
  ckey <- reader $ getConsumerKey . consumerKey . fst
  token <- reader $ getAccessToken . accessToken . fst
  url <- reader $ retrieveEndpoint . snd
  liftIO $ requestBodySkeleton url (keyValuePairs ckey token)
    where keyValuePairs ckey token = [ ("consumer_key", ckey)
                                     , ("access_token", token)
                                     , ("count", CS.pack $ show n)
                                     , ("detailType", "simple")
                                     ]

addItem :: BS.ByteString -> Hocket (PocketCredentials,PocketAPIUrls) ()
addItem url = do
  ckey <- reader $ getConsumerKey . consumerKey . fst
  token <- reader $ getAccessToken . accessToken . fst
  reqUrl <- reader $ addEndpoint . snd
  response <- liftIO $ requestSkeleton reqUrl (keyValuePairs ckey token)
  liftIO $ if HC.responseStatus response == ok200
             then putStrLn "Success." -- TODO: use hocket monad exception
             else putStrLn "Failed to add."
  where keyValuePairs ckey token = [ ("consumer_key", ckey)
                                   , ("access_token", token)
                                   , ("url", url)
                                   ]

defaultHocket :: Hocket (PocketCredentials,PocketAPIUrls) a -> IO a
defaultHocket = runHocket (pocketCredentials,defaultPocketAPIUrls)

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "add" -> defaultHocket $ addItem . CS.pack $ args !! 2
    "get" -> (defaultHocket $ retrieveList (read $ args !! 2)) >>= C.putStrLn
    "auth" -> authorize defaultConsumerKey defaultPocketAPIUrls >>= (CS.putStrLn . getAccessToken)
    _ -> fail "Invalid args."
