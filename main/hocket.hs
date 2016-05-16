{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Brick
import qualified Brick.Focus as Focus
import           Brick.Widgets.Border (hBorder)
import qualified Brick.Widgets.List as L
import           Control.Concurrent.Chan (newChan,Chan,writeChan)
import           Control.Exception.Base (try)
import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Default (def)
import           Data.Foldable (for_)
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Formatting (sformat, (%))
import qualified Formatting as F
import qualified Formatting.Time as F
import           Graphics.Vty (Event, mkVty, Key (KChar), Event (EvKey))
import qualified Graphics.Vty as Vty
import           Network.HTTP.Client (HttpException)
import           Network.URI

import           Network.Pocket
import           Network.Pocket.Retrieve
import           State
import           Widgets

data HocketEvent = Internal InternalEvent | VtyEvent Event
data InternalEvent = ShiftItem PocketItemId

trigger :: Chan HocketEvent -> HocketEvent -> EventM ()
trigger es e = liftIO (writeChan es e)

eventHandler :: Chan HocketEvent -> HocketState -> HocketEvent -> EventM (Next HocketState)
eventHandler es s (VtyEvent (EvKey (KChar 'd') [])) = do
  for_ maybePid $ \pid -> es `trigger` Internal (ShiftItem pid)
  continue s
  where
    maybePid :: Maybe PocketItemId
    maybePid = do
      list <- focusedList s
      item <- snd <$> L.listSelectedElement list
      return $ item ^. itemId
eventHandler _ s (VtyEvent (EvKey (KChar 'q') [])) = halt s
eventHandler _ s (VtyEvent (EvKey (KChar '\t') [])) =
  s & focusRing %~ Focus.focusNext & continue
eventHandler _ s (VtyEvent e) =
  continue =<< case focused s of
                 Just n | n == itemListName -> handleEventLensed s itemListVi e
                 Just n | n == pendingListName -> handleEventLensed s pendingListVi e
                 _ -> return s
eventHandler _ s (Internal (ShiftItem pid)) =
  continue =<< case focused s of
                 Just n | n == itemListName -> shiftItem s pid itemList pendingList
                 Just n | n == pendingListName -> shiftItem s pid pendingList itemList
                 _ -> return s

shiftItem :: HocketState
          -> PocketItemId
          -> Lens' HocketState (L.List PocketItem)
          -> Lens' HocketState (L.List PocketItem)
          -> EventM HocketState
shiftItem s _ src tgt = do
  let (maybeRemoved, srcList) = listRemoveSelected (s ^. src)
      tgtList = s ^. tgt
      newTgtList = maybe tgtList
                         (\removed -> listInsertSorted (view timeAdded) removed tgtList)
                         maybeRemoved
  return $ s & src .~ srcList
             & tgt .~ newTgtList


main :: IO ()
main = do
  events <- newChan
  void (customMain (mkVty def) events (app events) initialState)

app :: Chan HocketEvent -> App HocketState HocketEvent
app events = App {appDraw = drawGui
                 ,appChooseCursor = Focus.focusRingCursor (view focusRing)
                 ,appHandleEvent = eventHandler events
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
  where w = vBox [hBar ("Hocket: ("
                     <> uncurry (sformat (F.int % "|" % F.int)) (hsNumItems s)
                     <> ")")
                 ,L.renderList (s ^. itemList) (listDrawElement (isFocused s itemListName))
                 ,hBorder
                 ,vLimit 10 $
                    L.renderList (s ^. pendingList) (listDrawElement (isFocused s pendingListName))
                 ,hBar "This is the bottom"]

focused :: HocketState -> Maybe Name
focused = Focus.focusGetCurrent . view focusRing

focusedList :: HocketState -> Maybe (L.List PocketItem)
focusedList s = case focused s of
                  Just n | n == itemListName -> Just $ s ^. itemList
                  Just n | n == pendingListName -> Just $ s ^. pendingList
                  _ -> Nothing

isFocused :: HocketState -> Name -> Bool
isFocused s name = maybe False (==name) (focused s)

listDrawElement :: Bool -> Bool -> PocketItem -> Widget
listDrawElement hasFocus sel e = (if hasFocus && sel
                                    then withAttr ("list" <> "selectedItem")
                                    else withAttr ("list" <> "unselectedItem"))
                                 (padRight Max (txtDisplay e))

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
pocketCredentials = PocketCredentials (ConsumerKey "<consumer-key>")
                                      (AccessToken "<access-token>")

defaultRetrieval :: RetrieveConfig
defaultRetrieval = def & retrieveSort ?~ NewestFirst
                       & retrieveCount .~ NoLimit
                       & retrieveDetailType ?~ Complete

txtDisplay :: PocketItem -> Widget
txtDisplay pit = txt (T.justifyRight 12 ' ' leftEdge)
             <+> txt (fromMaybe "<empty>"
                                 (listToMaybe $ filter (not . T.null)
                                                       [given,resolved,T.pack url]))
             <+> padLeft Max (hLimit horizontalUriLimit (txt trimmedUrl))
  where resolved = view resolvedTitle pit
        given = view givenTitle pit
        (URL url) = view resolvedUrl pit
        added = posixSecondsToUTCTime (view timeUpdated pit)
        leftEdge = "("
                <> sformat (F.dayOfMonth <> " " % F.monthNameShort <> " " % F.yy) added
                <> ") "
        trimmedUrl = T.pack (trimURI url)

horizontalUriLimit :: Int
horizontalUriLimit = 60

trimURI :: String -> String
trimURI uri = fromMaybe uri $ do
  parsed <- parseURI uri
  auth <- uriAuthority parsed
  return (uriRegName auth <> uriPath parsed)
