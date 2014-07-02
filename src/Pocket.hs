{-# LANGUAGE OverloadedStrings #-}
module Pocket (
  authorize,
  retrieveList,
  addItem,
  archive
) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader.Class
import           Data.Aeson (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Text.Encoding (decodeUtf8)
import qualified Network.HTTP.Conduit as HC
import           Network.HTTP.Types.Status
import           Numeric.Natural

import           Util
import           Types
import           Parsing

authorize :: ConsumerKey -> PocketAPIUrls -> IO AccessToken
authorize consumeKey api = runHocket api $ do
  requestToken <- obtainRequestToken consumeKey
  let redirUrl = createDefaultRedirectUrl consumeKey
  _ <- liftIO $ do
    C.putStrLn . createRedirectUrl requestToken . toLazyBS $ redirUrl
    putStrLn "Press enter after finishing authorization of the app via the link above."
    getLine
  obtainPocketAccessToken consumeKey requestToken

retrieveList :: Maybe (Natural,Natural) -> HocketCA BL.ByteString
retrieveList maybeOffsetCount = do
  (ckey,token,reqUrl) <- getDetails retrieveEndpoint
  let kvps = keyValuePairs ckey token
      adjKVPS = case maybeOffsetCount of
        Just (offset,count) -> kvps ++ [ ("count", CS.pack . show $ count)
                                       , ("offset", CS.pack . show $ offset)]
        Nothing -> kvps
  liftIO $ requestBodySkeleton reqUrl adjKVPS
    where keyValuePairs ckey token = [ ("consumer_key", ckey)
                                     , ("access_token", token)
                                     , ("detailType", "simple")
                                     , ("sort", "newest")
                                     ]

addItem :: BS.ByteString -> HocketCA ()
addItem url = do
  (ckey,token,reqUrl) <- getDetails addEndpoint
  response <- liftIO $ requestSkeleton reqUrl (keyValuePairs ckey token)
  liftIO $ CS.putStrLn (if HC.responseStatus response == ok200
                        then BS.append "Success: " url
                        else BS.append "Failed to add: " url)
  where keyValuePairs ckey token = [ ("consumer_key", ckey)
                                   , ("access_token", token)
                                   , ("url", url)
                                   ]

archive :: BS.ByteString -> HocketCA Bool
archive itmId = do
  (ckey,token,reqUrl) <- getDetails modifyEndpoint
  response <- liftIO $ requestSkeleton reqUrl (keyValuePairs ckey token)
  return $ HC.responseStatus response == ok200
  where keyValuePairs ckey token = [ ("consumer_key", ckey)
                                   , ("access_token", token)
                                   , ("actions", actions)
                                   ]
        actions = toStrictBS $ encode [Archive . decodeUtf8 $ itmId]

obtainRequestToken :: ConsumerKey -> HocketA RequestToken
obtainRequestToken ckey = do
  reqUrl <- reader requestEndpoint
  let redirUrl = createDefaultRedirectUrl ckey
  respBody <- liftIO $ requestBodySkeleton reqUrl (keyValuePairs redirUrl)
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

obtainPocketAccessToken :: ConsumerKey -> RequestToken -> HocketA AccessToken
obtainPocketAccessToken ckey requestToken = do
  authUrl <- reader authorizeEndpoint
  respBody <- liftIO $ requestBodySkeleton authUrl keyValuePairs
  return $ AccessToken . toStrictBS . extractValue $ respBody
      where keyValuePairs = [ ("consumer_key", getConsumerKey ckey)
                            , ("code", getRequestToken requestToken)
                            ]

getDetails :: (PocketAPIUrls -> String) -> HocketCA (BS.ByteString,BS.ByteString,String)
getDetails endpointSelector = do
  ckey <- reader $ getConsumerKey . credConsumerKey . fst
  token <- reader $ getAccessToken . credAccessToken . fst
  reqUrl <- reader $ endpointSelector . snd
  return (ckey, token, reqUrl)
