{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GUI ( newList'
           , newEditDialog
           , listItems
           , addToListSortedBy
           , boldBlackOnOrange

           , EditDialog
           , editDlgDialog
           , editDlgWidget
           , editDlgFocusGroup
           , editVar
           ) where

import           Control.Applicative ((<$>))
import           Control.Concurrent.MVar (MVar, newMVar)
import           Control.Lens (preview, _Just, _1)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad (replicateM_, void)
import           Data.Maybe (catMaybes)
import           Data.Traversable (for)
import           Graphics.Vty (Attr, Modifier, Key)
import qualified Graphics.Vty as V
import           Graphics.Vty.Widgets.All (Widget, List, Dialog, FocusGroup, Edit)
import qualified Graphics.Vty.Widgets.All as W

import Network.Pocket

data EditDialog b = EditDialog { _editDlgDialog :: Dialog
                               , _editDlgWidget :: Widget Edit
                               , _editDlgFocusGroup :: Widget FocusGroup
                               , _editVar :: MVar (Maybe (Widget (List PocketItem b)))
                               }
makeLenses ''EditDialog

boldBlackOnOrange :: Attr
boldBlackOnOrange = realBlack `W.on` V.Color240 147 `W.mergeAttr` W.style V.bold
  where realBlack = V.rgb_color (0::Int) 0 0

newList' :: Show b => Attr -> Attr -> IO (Widget (List a b))
newList' focus normal = do
  w <- W.newList keepCurrent 1
  W.setFocusAttribute w focus
  W.setNormalAttribute w normal
  w `W.onKeyPressed` listWidgetVIKeys
  w `W.onKeyPressed` listWidgetEmacsKeys
  return w

newEditDialog :: IO (EditDialog b)
newEditDialog = do
  fg1 <- W.newFocusGroup
  e <- W.editWidget
  void $ W.addToFocusGroup fg1 e
  (dlg, fg2) <- W.newDialog e "Edit"
  fg <- W.mergeFocusGroups fg1 fg2
  m <- newMVar Nothing
  return $ EditDialog dlg e fg m

keepCurrent :: Attr
keepCurrent = V.Attr V.KeepCurrent V.KeepCurrent V.KeepCurrent

listWidgetEmacsKeys :: Widget (List a b) -> Key -> [Modifier] -> IO Bool
listWidgetEmacsKeys this key _ = case key of
  (V.KASCII 'n') -> W.scrollDown this >> return True
  (V.KASCII 'p') -> W.scrollUp this >> return True
  (V.KASCII '<') -> W.scrollToBeginning this >> return True
  (V.KASCII '>') -> W.scrollToEnd this >> return True
  _ -> return False

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
  let pos = foldr go 0 itms
      go i acc = if x `cmp` i == LT then acc else acc + 1
  W.insertIntoList lst x wx pos
