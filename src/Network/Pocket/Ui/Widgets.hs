{-# LANGUAGE TemplateHaskell #-}
module Network.Pocket.Ui.Widgets (ViList(ViList)
               ,_ViList
               ,EmacsList(..)
               ,_EmacsList
               ,listRemoveSelected
               ,listInsertSorted) where

import           Brick (HandleEvent(handleEvent))
import           Brick.Widgets.List (List)
import qualified Brick.Widgets.List as L
import           Control.Lens
import           Data.Function (on)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Vector as V
import           Graphics.Vty (Event(..), Key(..), Modifier(..))

newtype ViList a = ViList (List a)
makePrisms ''ViList

instance HandleEvent (ViList a) where
  handleEvent e@(EvKey (KChar c) ms) (ViList theList) =
    case (c,ms) of
      ('j',[]) -> return . ViList $ L.listMoveDown theList
      ('k',[]) -> return . ViList $ L.listMoveUp theList
      ('g',[]) -> return . ViList $ L.listMoveTo 0 theList
      ('G',[]) ->
        return . ViList $ L.listMoveTo (V.length (L.listElements theList))  theList
      _ -> fmap ViList (handleEvent e theList)
  handleEvent e (ViList theList) = fmap ViList (handleEvent e theList)

newtype EmacsList a = EmacsList (List a)
makePrisms ''EmacsList

instance HandleEvent (EmacsList a) where
  handleEvent e@(EvKey (KChar c) ms) (EmacsList theList) =
    case (c,ms) of
      ('n',[MCtrl]) -> return . EmacsList $ L.listMoveDown theList
      ('p',[MCtrl]) -> return . EmacsList $ L.listMoveUp theList
      ('<',[MMeta]) -> return . EmacsList $ L.listMoveTo 0 theList
      ('>',[MMeta]) ->
        return . EmacsList $ L.listMoveTo (V.length (L.listElements theList))  theList
      _ -> fmap EmacsList (handleEvent e theList)
  handleEvent e (EmacsList theList) = fmap EmacsList (handleEvent e theList)

listRemoveSelected :: List e -> (Maybe e, List e)
listRemoveSelected l = fromMaybe (Nothing, l) $ do
  sel <- l^.L.listSelectedL
  let es = l^.L.listElementsL
      newSel = clamp 0 (V.length es - 2) sel
      newEs = V.take sel es <> V.drop (sel+1) es
  return (es V.!? sel, l & L.listElementsL .~ newEs
              & L.listSelectedL .~ Just newSel)

listInsertSorted :: Ord b => (a -> b) -> a -> L.List a -> L.List a
listInsertSorted toOrd x lxs = L.listInsert insertPos x lxs
  where insertPos :: Int
        insertPos = fromMaybe (length xs)
                              (V.findIndex (((<) `on`) toOrd x) xs)
        xs = L.listElements lxs

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx val = max mn (min val mx)
