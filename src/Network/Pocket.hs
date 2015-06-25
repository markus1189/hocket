{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Pocket (
  module Network.Pocket.Types,

  pocket,
  PocketRequest (..)
) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>),(<*>))
#endif

import           Control.Lens (_Left, view, _2, magnify, Getter)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader.Class
import           Data.Aeson.Lens (key, members, _JSON, values, _Bool, _Integral)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Network.HTTP.Client as HC
import           Network.HTTP.Types.Status
import qualified Network.Wreq as W

import           Network.Pocket.Types

makeLensesFor [("managerResponseTimeout", "managerResponseTimeout")] ''HC.ManagerSettings

selectEndpoint :: PocketRequest a -> Hocket URL
selectEndpoint req = magnify _2 $ view (sel req)
  where sel :: PocketRequest a -> Getter PocketAPIUrls URL
        sel (AddItem _) = addEndpoint
        sel (ArchiveItem _) = modifyEndpoint
        sel (RenameItem _ _) = modifyEndpoint
        sel (Batch _) = modifyEndpoint
        sel (RetrieveItems _) = retrieveEndpoint
        sel (Raw x) = sel x

pocket :: PocketRequest a -> Hocket a
pocket req = do
  (URL ep,c) <- (,) <$> selectEndpoint req <*> asks fst
  let opts = W.defaults & W.manager . _Left . managerResponseTimeout ?~ 10e7
  resp <- liftIO . W.postWith opts ep $ toFormParams (c,req)
  return $ case req of
    Raw _ -> resp ^. W.responseBody & TL.toStrict . TLE.decodeUtf8
    AddItem _ -> resp ^. W.responseStatus == ok200
    ArchiveItem _ -> resp ^. W.responseStatus == ok200
    RenameItem _ _ -> resp ^. W.responseStatus == ok200
    RetrieveItems _ -> PocketItemBatch (fromInteger $ fromMaybe err t) is
      where is = resp ^.. W.responseBody . key "list" . members . _JSON
            t = resp ^? W.responseBody . key "since" . _Integral
            err = error "Result did not contain a timestamp"
    Batch _ -> resp ^.. W.responseBody
                      . key "action_results"
                      . values
                      . _Bool
