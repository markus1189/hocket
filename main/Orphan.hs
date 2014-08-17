{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphan where

import qualified Network.Pocket.Types as T
import Network.Pocket.Types (PocketItem)

import Control.Applicative hiding (empty)
import Control.Lens
import Data.Table
import Prelude hiding (null)

instance Tabular PocketItem where
  type PKT PocketItem = T.PocketItemId
  data Key k PocketItem b where
    PocketItemId:: Key Primary PocketItem T.PocketItemId
  data Tab PocketItem i = PIT (i Primary T.PocketItemId)

  fetch PocketItemId = view T.itemId

  primary = PocketItemId
  primarily PocketItemId r = r

  mkTab f               = PIT <$> f PocketItemId
  forTab (PIT x) f = PIT <$> f PocketItemId x
  ixTab (PIT x) PocketItemId  = x
