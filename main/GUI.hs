module GUI ( newList'
           , listItems
           , addToListSortedBy
           , boldBlackOnOrange
           ) where

import           Control.Applicative ((<$>))
import           Control.Lens (preview, _Just, _1)
import           Control.Lens.Operators
import           Control.Monad (replicateM_)
import           Data.Maybe (catMaybes)
import           Graphics.Vty (Attr, Modifier, Key)
import qualified Graphics.Vty as V
import           Graphics.Vty.Widgets.All (Widget, List)
import qualified Graphics.Vty.Widgets.All as W
import           Data.Traversable (for)

boldBlackOnOrange :: Attr
boldBlackOnOrange = realBlack `W.on` (V.Color240 147) `W.mergeAttr` W.style V.bold
  where realBlack = V.rgb_color (0::Int) 0 0

newList' :: Show b => Attr -> IO (Widget (List a b))
newList' focus = do
  w <- W.newList keepCurrent 1
  W.setFocusAttribute w focus
  w `W.onKeyPressed` listWidgetVIKeys
  return w

keepCurrent :: Attr
keepCurrent = V.Attr V.KeepCurrent V.KeepCurrent V.KeepCurrent

listWidgetVIKeys :: Widget (List a b) -> Key -> [Modifier] -> IO Bool
listWidgetVIKeys this key _ = case key of
  (V.KASCII 'j') -> W.scrollDown this >> return True
  (V.KASCII 'k') -> W.scrollUp this >> return True
  (V.KASCII 'J') -> replicateM_ 3 (W.scrollDown this) >> return True
  (V.KASCII 'K') -> replicateM_ 3 (W.scrollUp this) >> return True
  (V.KASCII 'g') -> W.scrollToBeginning this >> return True
  (V.KASCII 'G') -> W.scrollToEnd this >> return True
  _ -> return False

listItems :: Widget (List a b) -> IO [a]
listItems lst = do
  n <- W.getListSize lst
  catMaybes <$> for [0..(n-1)] getItem
  where getItem i = W.getListItem lst i <&> preview (_Just . _1)

addToListSortedBy :: Show b =>
                     (a -> a -> Ordering)
                  -> (a -> IO (Widget b))
                  -> Widget (List a b)
                  -> a
                  -> IO ()
addToListSortedBy cmp render lst x = do
  itms <- listItems lst
  wx <- render x
  let pos = foldr (\i acc -> if x `cmp` i == LT then acc else acc + 1) 0 itms
  W.insertIntoList lst x wx pos
