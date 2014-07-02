{-# LANGUAGE NumDecimals #-}
module Util (
  requestSkeleton,
  requestBodySkeleton,
  toStrictBS,
  toLazyBS,
  extractValue,
  useHttps,
  setTimeOut
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Network.HTTP.Conduit as HC

requestSkeleton :: String
                -> [(BS.ByteString, BS.ByteString)]
                -> IO (HC.Response CL.ByteString)
requestSkeleton url keyValuePairs = do
  request' <- HC.parseUrl url
  let request = setTimeOut 10e7
              . useHttps
              . HC.urlEncodedBody keyValuePairs
              $ request'
  HC.withManager $ HC.httpLbs request

requestBodySkeleton :: String
                    -> [(BS.ByteString, BS.ByteString)]
                    -> IO CL.ByteString
requestBodySkeleton url kvs = HC.responseBody `fmap` requestSkeleton url kvs

toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BS.concat . BL.toChunks

toLazyBS :: BS.ByteString -> BL.ByteString
toLazyBS = BL.pack . BS.unpack

extractValue :: BL.ByteString -> BL.ByteString
extractValue = head . tail . CL.split '='

useHttps :: HC.Request -> HC.Request
useHttps req = req { HC.secure = True }

setTimeOut :: Int -> HC.Request -> HC.Request
setTimeOut n req = req { HC.responseTimeout = Just n }
