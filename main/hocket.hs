{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Brick
import Brick.Widgets.Border (hBorder)
import qualified Brick.Widgets.List as L
import           Control.Concurrent.Chan (newChan)
import           Control.Lens
import           Control.Monad (void)
import           Data.Default (def)
import           Data.Maybe (listToMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Graphics.Vty (Event, mkVty, Key (KChar), Event (EvKey))
import qualified Graphics.Vty as Vty

import State

data HocketEvent = VtyEvent Event

main :: IO ()
main = do
  events <- newChan
  let s = initialState
  void (customMain (mkVty def) events app s)

app :: App HocketState HocketEvent
app = App {appDraw = drawGui
          ,appChooseCursor = \_ cs -> listToMaybe cs
          ,appHandleEvent = eventHandler
          ,appStartEvent = return
          ,appAttrMap = const hocketAttrMap
          ,appLiftVtyEvent = VtyEvent
          }

hocketAttrMap :: AttrMap
hocketAttrMap =
  attrMap Vty.defAttr [("list" <> "selectedItem", boldBlackOnOrange)
                      ,("list" <> "unselectedItem", whiteFg)
                      ,("bar", Vty.defAttr `Vty.withBackColor` Vty.black `Vty.withForeColor` Vty.white)]

eventHandler :: HocketState -> HocketEvent -> EventM (Next HocketState)
eventHandler s (VtyEvent (EvKey (KChar 'q') [])) = halt s
eventHandler s (VtyEvent e) = handleEventLensed s itemList e >>= continue

drawGui :: HocketState -> [Widget]
drawGui s = [hBar "This is hocket!"
         <=> L.renderList (s ^. itemList) listDrawElement
         <=> hBorder
         <=> vLimit 10 (L.renderList (s ^. pendingList) listDrawElement)
         <=> hBar "This is the bottom"]

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
    black `Vty.withBackColor`
      orange `Vty.withStyle` Vty.bold

black :: Vty.Color
black = Vty.rgbColor zero zero zero
  where zero = 0 :: Int

whiteFg :: Vty.Attr
whiteFg = Vty.defAttr `Vty.withForeColor` Vty.white

hBar :: Text -> Widget
hBar = withAttr "bar" . padRight Max . txt
