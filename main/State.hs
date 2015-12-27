{-# LANGUAGE TemplateHaskell #-}
module State (HocketState
             ,itemList
             ,pendingList

             ,initialState
             ) where

import qualified Brick.Widgets.List as L
import           Control.Lens
import           Brick (Name(Name))
import qualified Data.Vector as V

data HocketState = HocketState { _itemList :: L.List Int
                               , _pendingList :: L.List Int
                               }
makeLenses ''HocketState

initialState :: HocketState
initialState = HocketState (L.list (Name "items") V.empty 2)
                           (L.list (Name "pending") V.empty 1)
