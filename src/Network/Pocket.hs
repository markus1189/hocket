{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Network.Pocket
  ( module Network.Pocket.Types,
    pocket,
    PocketRequest (..),
  )
where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>),(<*>))
#endif

import Control.Lens (Getter, magnify, view, _2)
import Control.Lens.Operators
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class
import Data.Aeson.Lens (key, members, values, _Bool, _Integral, _JSON)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as HCT
import Network.HTTP.Types.Status
import Network.Pocket.Types
import qualified Network.Wreq as W

selectEndpoint :: PocketRequest a -> Hocket URL
selectEndpoint req = magnify _2 $ view (sel req)
  where
    sel :: PocketRequest a -> Getter PocketAPIUrls URL
    sel (AddItem _) = addEndpoint
    sel (ArchiveItem _) = modifyEndpoint
    sel (RenameItem _ _) = modifyEndpoint
    sel (Batch _) = modifyEndpoint
    sel (RetrieveItems _) = retrieveEndpoint
    sel (Raw x) = sel x

pocket :: PocketRequest a -> Hocket a
pocket req = do
  (URL ep, credentials) <- (,) <$> selectEndpoint req <*> asks fst
  let opts =
        W.defaults
          & W.manager
            .~ Left
              ( HCT.tlsManagerSettings
                  { HC.managerResponseTimeout = HC.responseTimeoutDefault
                  }
              )
  resp <- liftIO . W.postWith opts ep $ toFormParams (credentials, req)
  return $ case req of
    Raw _ -> resp ^. W.responseBody & TL.toStrict . TLE.decodeUtf8
    AddItem _ -> resp ^. W.responseStatus == ok200
    ArchiveItem _ -> resp ^. W.responseStatus == ok200
    RenameItem _ _ -> resp ^. W.responseStatus == ok200
    RetrieveItems _ -> PocketItemBatch (fromInteger $ fromMaybe err t) is total
      where
        is = resp ^.. W.responseBody . key "list" . members . _JSON
        t = resp ^? W.responseBody . key "since" . _Integral
        total = fromMaybe 0 $ resp ^? W.responseBody . key "total" . _Integral
        err = error "Result did not contain a timestamp"
    Batch _ ->
      resp
        ^.. W.responseBody
          . key "action_results"
          . values
          . _Bool
