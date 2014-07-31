{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE RankNTypes #-}

module Pocket (
  perform,
  PocketRequest (..)
) where

import           Control.Applicative ((<$>),(<*>))
import           Control.Lens (_Left, view, _2, magnify, Getter)
import           Control.Lens.Operators
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader.Class
import           Data.Aeson.Lens (key, members, _JSON, values, _Bool)
import qualified Network.HTTP.Client as HC
import           Network.HTTP.Types.Status
import qualified Network.Wreq as W
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL

import           Types

selectEndpoint :: PocketRequest a -> Hocket URL
selectEndpoint req = magnify _2 $ view (sel req)
  where sel :: PocketRequest a -> Getter PocketAPIUrls URL
        sel r = case r of
          AddItem _ -> addEndpoint
          ArchiveItem _ -> modifyEndpoint
          RenameItem _ _ -> modifyEndpoint
          Batch _ -> modifyEndpoint
          RetrieveItems _ -> retrieveEndpoint
          Raw x -> sel x

perform :: PocketRequest a -> Hocket a
perform req = do
  (URL ep,c) <- (,) <$> selectEndpoint req <*> asks fst
  resp <- liftIO . W.postWith opts ep $ toFormParams (c,req)
  return $ case req of
    Raw _ -> resp ^. W.responseBody & TL.toStrict . TLE.decodeUtf8
    AddItem _ -> resp ^. W.responseStatus == ok200
    ArchiveItem _ -> resp ^. W.responseStatus == ok200
    RenameItem _ _ -> resp ^. W.responseStatus == ok200
    RetrieveItems _ -> resp ^.. W.responseBody . key "list" . members . _JSON
    Batch _ -> resp ^.. W.responseBody
                      . key "action_results"
                      . values
                      . _Bool
  where opts = W.defaults & W.manager . _Left %~ setManagerTimeOut 10e7

setManagerTimeOut :: Int -> HC.ManagerSettings -> HC.ManagerSettings
setManagerTimeOut n mset = mset { HC.managerResponseTimeout = Just n }
