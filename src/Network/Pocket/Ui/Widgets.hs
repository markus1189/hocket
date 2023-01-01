module Network.Pocket.Ui.Widgets (listRemoveSelected
                                 ,listInsertSorted
                                 ,clamp) where

import           Brick.Widgets.List (List)
import qualified Brick.Widgets.List as L
import           Control.Lens
import           Data.Function (on)
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V

listRemoveSelected :: List n e -> (Maybe e, List n e)
listRemoveSelected l = fromMaybe (Nothing, l) $ do
  sel <- l^.L.listSelectedL
  let es = l^.L.listElementsL
      newSel = clamp 0 (V.length es - 2) sel
      newEs = V.take sel es <> V.drop (sel+1) es
  return (es V.!? sel, l & L.listElementsL .~ newEs
              & L.listSelectedL ?~ newSel)

listInsertSorted :: Ord b => (a -> b) -> a -> L.List n a -> L.List n a
listInsertSorted toOrd x lxs = L.listInsert insertPos x lxs
  where insertPos :: Int
        insertPos = fromMaybe (length xs)
                              (V.findIndex (((<) `on` toOrd) x) xs)
        xs = L.listElements lxs

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx val = max mn (min val mx)
