{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Brick
import qualified Brick.Focus as F
import           Brick.Widgets.Border (hBorder)
import qualified Brick.Widgets.List as L
import           Control.Concurrent.Chan (newChan)
import           Control.Exception.Base (try)
import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Default (def)
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock (diffUTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, POSIXTime)
import           Formatting (sformat, (%))
import qualified Formatting as F
import qualified Formatting.Time as F
import           Graphics.Vty (Event, mkVty, Key (KChar), Event (EvKey))
import qualified Graphics.Vty as Vty
import           Network.HTTP.Client (HttpException)

import           Network.Pocket
import           Network.Pocket.Retrieve
import           State

data HocketEvent = VtyEvent Event

eventHandler :: HocketState -> HocketEvent -> EventM (Next HocketState)
eventHandler s (VtyEvent (EvKey (KChar 'q') [])) = halt s
eventHandler s (VtyEvent (EvKey (KChar '\t') [])) = s & focusRing %~ F.focusNext & continue
eventHandler s (VtyEvent e) =
  continue =<< case F.focusGetCurrent $ view focusRing s of
                 Just n | n == itemListName -> handleEventLensed s itemListVi e
                 Just n | n == pendingListName -> handleEventLensed s pendingListVi e
                 _ -> return s

main :: IO ()
main = do
  events <- newChan
  void (customMain (mkVty def) events app initialState)

app :: App HocketState HocketEvent
app = App {appDraw = drawGui
          ,appChooseCursor = F.focusRingCursor (view focusRing)
          ,appHandleEvent = eventHandler
          ,appStartEvent = \s -> do
             eitherErrorPIs <- liftIO retrieveItems
             case eitherErrorPIs of
               Left _ -> return s
               Right (PocketItemBatch ts pis) -> return (addItemsUnread ts pis s)
          ,appAttrMap = const hocketAttrMap
          ,appLiftVtyEvent = VtyEvent
          }

hocketAttrMap :: AttrMap
hocketAttrMap =
  attrMap Vty.defAttr [("list" <> "selectedItem", boldBlackOnOrange)
                      ,("list" <> "unselectedItem", whiteFg)
                      ,("bar", Vty.defAttr `Vty.withBackColor`
                               Vty.black `Vty.withForeColor`
                               Vty.white)]

drawGui :: HocketState -> [Widget]
drawGui s = [w]
  where w = vBox [hBar ("This is hocket! ("
                     <> uncurry (sformat (F.int % " + " % F.int)) (hsNumItems s)
                     <> ")")
                 ,L.renderList (s ^. itemList) (listDrawElement (s ^. hsLastUpdated))
                 ,hBorder
                 ,vLimit 10 (L.renderList (s ^. pendingList)
                                          (listDrawElement (s ^. hsLastUpdated)))
                 ,hBar "This is the bottom"]

listDrawElement :: Maybe POSIXTime -> Bool -> PocketItem -> Widget
listDrawElement mtime sel e = (if sel
                                 then withAttr ("list" <> "selectedItem")
                                 else withAttr ("list" <> "unselectedItem"))
                                 (padRight Max (txt (display mtime e)))

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

retrieveItems :: IO (Either HttpException PocketItemBatch)
retrieveItems = tryHttpException
              . runHocket (pocketCredentials, def)
              . pocket
              $ RetrieveItems defaultRetrieval

tryHttpException :: IO a -> IO (Either HttpException a)
tryHttpException = try

pocketCredentials :: PocketCredentials
pocketCredentials = PocketCredentials (ConsumerKey "123")
                                      (AccessToken "456")

defaultRetrieval :: RetrieveConfig
defaultRetrieval = def & retrieveSort ?~ NewestFirst
                       & retrieveCount .~ NoLimit
                       & retrieveDetailType ?~ Complete

display :: Maybe POSIXTime -> PocketItem -> Text
display mtime pit = T.justifyRight 16 ' '
                                      ("(" <> maybe "?" (sformat (F.diff True)) dt <> ") ")
                 <> fromMaybe "<empty>"
                              (listToMaybe $ filter (not . T.null)
                                                    [given,resolved,T.pack url])
  where resolved = view resolvedTitle pit
        given = view givenTitle pit
        (URL url) = view resolvedUrl pit
        added = posixSecondsToUTCTime (view timeUpdated pit)
        dt = fmap (diffUTCTime added . posixSecondsToUTCTime) mtime
