{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NumDecimals #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Pocket (
  perform,
  PocketRequest (..)
) where


import           Control.Applicative ((<$>),(<*>))
import           Control.Lens (_Left, to)
import           Control.Lens.Operators
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader.Class
import           Data.Aeson.Lens (key, members, _JSON, _String, values)
import qualified Network.HTTP.Client as HC
import           Network.HTTP.Types.Status
import qualified Network.Wreq as W

import           Types

selectEndpoint :: PocketRequest a -> HocketCA String
selectEndpoint req = asks $ sel . snd
  where sel = case req of
          AddItem _ -> addEndpoint
          ArchiveItem _ -> modifyEndpoint
          Batch _ -> modifyEndpoint
          RetrieveItems _ -> retrieveEndpoint

selectEndpoint (AddItem _) = asks $ addEndpoint . snd

perform :: PocketRequest a -> HocketCA a
perform req = do
  (ep,c) <- (,) <$> selectEndpoint req <*> asks fst
  resp <- liftIO . W.postWith opts ep $ toFormParams (c,req)
  return $ case req of
    AddItem _ -> resp ^. W.responseStatus  == ok200
    ArchiveItem _ -> resp ^. W.responseStatus  == ok200
    RetrieveItems _ -> resp ^.. W.responseBody . key "list" . members . _JSON
    Batch _ -> resp ^.. W.responseBody
                      . key "action_results"
                      . values
                      . _String
                      . to (== "true")
  where opts = W.defaults & W.manager . _Left %~ setManagerTimeOut 10e7

setManagerTimeOut :: Int -> HC.ManagerSettings -> HC.ManagerSettings
setManagerTimeOut n mset = mset { HC.managerResponseTimeout = Just n }
