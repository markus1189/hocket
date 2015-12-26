{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Brick
import qualified Brick.Widgets.List as L
import           Control.Concurrent.Chan (newChan)
import           Control.Lens
import           Control.Monad (void)
import           Data.Default (def)
import           Data.Maybe (listToMaybe)
import           Data.Monoid ((<>))
import qualified Data.Vector as V
import           Graphics.Vty (Event, mkVty, Key (KChar), Event (EvKey))
import qualified Graphics.Vty as Vty

data HocketState = HocketState { _itemList :: L.List Int
                               }
makeLenses ''HocketState

data HocketEvent = VtyEvent Event

main :: IO ()
main = do
  events <- newChan
  void (customMain (mkVty def) events app initialState)
  return ()

initialState :: HocketState
initialState = HocketState (L.list (Name "itemList") V.empty 1)

app :: App HocketState HocketEvent
app = App {appDraw = drawGui
          ,appChooseCursor = \_ cs -> listToMaybe cs
          ,appHandleEvent = eventHandler
          ,appStartEvent = return
          ,appAttrMap = const hocketAttrMap
          ,appLiftVtyEvent = VtyEvent
          }

hocketAttrMap :: AttrMap
hocketAttrMap = attrMap Vty.defAttr [("list" <> "selectedItem", boldBlackOnOrange)
                                    ,("list" <> "unselectedItem", whiteFg)]

eventHandler :: HocketState -> HocketEvent -> EventM (Next HocketState)
eventHandler s (VtyEvent (EvKey (KChar 'q') [])) = halt s
eventHandler s (VtyEvent e) = handleEventLensed s itemList e >>= continue

drawGui :: HocketState -> [Widget]
drawGui s = [L.renderList (s ^. itemList) listDrawElement]

listDrawElement :: Show a => Bool -> a -> Widget
listDrawElement sel e = (if sel
                           then withAttr ("list" <> "selectedItem")
                           else withAttr ("list" <> "unselectedItem"))
                          (str (show e))

orange :: Vty.Color
orange = Vty.rgbColor 215 135 (0::Int)

boldBlackOnOrange :: Vty.Attr
boldBlackOnOrange =
  Vty.defAttr `Vty.withForeColor`
    realBlack `Vty.withBackColor`
      orange `Vty.withStyle` Vty.bold
  where realBlack = Vty.rgbColor zero zero zero
        zero = 0 :: Int

whiteFg :: Vty.Attr
whiteFg = Vty.defAttr `Vty.withForeColor` Vty.white
