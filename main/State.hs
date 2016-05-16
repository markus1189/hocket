{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module State (HocketState
             ,pendingList
             ,itemList
             ,itemListVi
             ,pendingListVi
             ,focusRing
             ,hsNumItems
             ,hsLastUpdated
             ,itemListName
             ,pendingListName

             ,initialState
             ,addItemsUnread
             ,contentsView
             ,fromContentsView
             ,withContents
             ) where

import           Brick (Name)
import qualified Brick.Focus as F
import qualified Brick.Widgets.List as L
import           Control.Lens
import           Data.Foldable (foldl',toList)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import           Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Vector as V

import           Widgets
import           Network.Pocket.Types

data HocketState = HocketState { _itemListVi :: ViList PocketItem
                               , _pendingListVi :: ViList PocketItem
                               , _focusRing :: F.FocusRing
                               , _hsLastUpdated :: Maybe POSIXTime
                               }
makeLenses ''HocketState

hsNumItems :: HocketState -> (Int,Int)
hsNumItems = (,) <$> V.length . view (itemList . L.listElementsL)
                 <*> V.length . view (pendingList . L.listElementsL)

initialState :: HocketState
initialState = HocketState (ViList $ L.list itemListName V.empty 1)
                           (ViList $ L.list pendingListName V.empty 1)
                           (F.focusRing [itemListName, pendingListName])
                           Nothing

itemListName :: Name
itemListName = "items"

pendingListName :: Name
pendingListName = "pending"

itemList :: Lens' HocketState (L.List PocketItem)
itemList = itemListVi . _ViList

addItemsUnread :: (Functor f, Foldable f)
               => POSIXTime -> f PocketItem -> HocketState -> HocketState
addItemsUnread ts pis s =
  s & itemList %~ applyAll (fmap (listInsertSorted (view timeUpdated)) pis)
    & itemList . L.listElementsL %~ V.reverse
    & hsLastUpdated ?~ ts

pendingList :: Lens' HocketState (L.List PocketItem)
pendingList = pendingListVi . _ViList

applyAll :: Foldable f => f (a -> a) -> a -> a
applyAll fs z = foldl (&) z fs

contentsView :: HocketState -> Map PocketItemId PocketItem
contentsView hs = buildMap (extract itemList hs <> extract pendingList hs)
  where extract l = view (l . L.listElementsL)
        buildMap = foldl' go Map.empty
        go acc item = Map.insert (item ^. itemId) item acc

fromContentsView :: Map PocketItemId PocketItem -> HocketState -> HocketState
fromContentsView contents s = s & itemList . L.listElementsL .~ V.fromList newItems
                                & pendingList . L.listElementsL .~ V.fromList newPendings
  where oldItemIds = Set.fromList (toListOf (itemList.L.listElementsL.each.itemId) s)
        oldPendingIds = Set.fromList (toListOf (pendingList.L.listElementsL.each.itemId) s)
        newItems = toList $ Map.filterWithKey (knownFrom oldItemIds) contents
        newPendings = toList $ Map.filterWithKey (knownFrom oldPendingIds) contents
        knownFrom frm pid _ = Set.member pid frm

withContents :: (Map PocketItemId PocketItem -> Map PocketItemId PocketItem)
             -> HocketState
             -> HocketState
withContents f s = fromContentsView (f (contentsView s)) s
