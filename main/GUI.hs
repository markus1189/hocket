module GUI ( newList'
           ) where

import qualified Graphics.Vty as V
import qualified Graphics.Vty.Widgets.All as W
import Control.Monad (replicateM_)

type ListWidget a b = W.Widget (W.List a b)

boldBlackOnOrange :: V.Attr
boldBlackOnOrange = realBlack `W.on` (V.Color240 147) `W.mergeAttr` W.style V.bold
  where realBlack = V.rgb_color (0::Int) 0 0

newList' :: Show b => IO (ListWidget a b)
newList' = do
  w <- W.newList keepCurrent 1
  W.setFocusAttribute w boldBlackOnOrange
  w `W.onKeyPressed` listWidgetVIKeys
  return w

keepCurrent :: V.Attr
keepCurrent = V.Attr V.KeepCurrent V.KeepCurrent V.KeepCurrent

listWidgetVIKeys :: Show b => ListWidget a b -> V.Key -> [V.Modifier] -> IO Bool
listWidgetVIKeys this key _ = case key of
  (V.KASCII 'j') -> W.scrollDown this >> return True
  (V.KASCII 'k') -> W.scrollUp this >> return True
  (V.KASCII 'J') -> replicateM_ 3 (W.scrollDown this) >> return True
  (V.KASCII 'K') -> replicateM_ 3 (W.scrollUp this) >> return True
  (V.KASCII 'g') -> W.scrollToBeginning this >> return True
  (V.KASCII 'G') -> W.scrollToEnd this >> return True
  _ -> return False
