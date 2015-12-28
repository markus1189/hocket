{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module State (HocketState
             ,itemList
             ,pendingList
             ,itemListVi
             ,pendingListVi
             ,focusRing
             ,itemListName
             ,pendingListName

             ,initialState
             ) where

import           Brick (Name)
import qualified Brick.Focus as F
import qualified Brick.Widgets.List as L
import           Control.Lens
import qualified Data.Vector as V

import Widgets
import Network.Pocket.Types

data HocketState = HocketState { _itemListVi :: ViList PocketItem
                               , _pendingListVi :: ViList PocketItem
                               , _focusRing :: F.FocusRing
                               }
makeLenses ''HocketState

initialState :: HocketState
initialState = HocketState (ViList $ L.list itemListName V.empty 2)
                           (ViList $ L.list pendingListName V.empty 1)
                           (F.focusRing [itemListName, pendingListName])

itemListName :: Name
itemListName = "items"

pendingListName :: Name
pendingListName = "pending"

itemList :: Lens' HocketState (L.List PocketItem)
itemList = itemListVi . _ViList

pendingList :: Lens' HocketState (L.List PocketItem)
pendingList = pendingListVi . _ViList
