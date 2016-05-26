{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Brick
import qualified Brick.Focus as Focus
import           Brick.Widgets.Border (hBorder)
import qualified Brick.Widgets.List as L
import           Control.Concurrent.Async (async)
import           Control.Concurrent.Chan (newChan,Chan,writeChan)
import           Control.Exception.Base (try)
import           Control.Lens
import           Control.Monad (void, mfilter)
import           Control.Monad.IO.Class (liftIO)
import           Data.Default (def)
import           Data.Foldable (for_)
import           Data.List (isPrefixOf, partition)
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import           Data.Monoid ((<>))
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, POSIXTime)
import           Formatting (sformat, (%))
import qualified Formatting as F
import qualified Formatting.Time as F
import           Graphics.Vty (Event, mkVty, Key (KChar), Event (EvKey))
import qualified Graphics.Vty as Vty
import           Network.HTTP.Client (HttpException)
import           Network.URI

import           Network.Pocket
import           Network.Pocket.Retrieve
import           Network.Pocket.Ui.State

data HocketEvent = Internal InternalEvent
                 | VtyEvent Event
                 deriving (Show,Eq)

data InternalEvent = ShiftItem PocketItemId
                   | RemoveItems (Set PocketItemId)
                   | FetchItems
                   | ArchiveItems
                   | ArchivedItems [PocketItemId]
                   | FetchedItems POSIXTime [PocketItem]
                   | SetStatus (Maybe Text)
                   | AsyncActionFailed
                   deriving (Show,Eq)

trigger :: Chan HocketEvent -> HocketEvent -> IO ()
trigger = writeChan

vtyEventHandler :: Chan HocketEvent -> HocketState -> Event -> EventM (Next HocketState)
vtyEventHandler es s (EvKey (KChar 'u') []) = do
  liftIO $ es `trigger` Internal FetchItems
  continue s
vtyEventHandler es s (EvKey (KChar 'A') []) = do
  liftIO $ es `trigger` Internal ArchiveItems
  continue s
vtyEventHandler es s (EvKey (KChar 'd') []) = do
  liftIO $ for_ maybePid $ \pid -> es `trigger` Internal (ShiftItem pid)
  continue s
  where
    maybePid :: Maybe PocketItemId
    maybePid = do
      list <- focusedList s
      item <- snd <$> L.listSelectedElement list
      return $ item ^. itemId
vtyEventHandler _ s (EvKey (KChar 'q') []) = halt s
vtyEventHandler _ s (EvKey (KChar '\t') []) =
  s & focusRing %~ Focus.focusNext & continue
vtyEventHandler _ s e =
  continue =<< case focused s of
                 Just n | n == itemListName -> handleEventLensed s itemListVi e
                 Just n | n == pendingListName -> handleEventLensed s pendingListVi e
                 _ -> return s

internalEventHandler :: Chan HocketEvent
                     -> HocketState
                     -> InternalEvent
                     -> EventM (Next HocketState)
internalEventHandler _ s (ShiftItem pid) = continue (toggleStatus pid s)
internalEventHandler _ s (RemoveItems pis) = continue (removeItems pis s)

internalEventHandler es s@(view hsAsync -> Nothing) FetchItems = do
  fetchAsync <- liftIO . async $ do
    es `trigger` Internal (SetStatus (Just "fetching"))
    eitherErrorPis <- retrieveItems (s ^. hsLastUpdated)
    case eitherErrorPis of
      Left _ -> es `trigger` Internal AsyncActionFailed
      Right (PocketItemBatch ts pis) -> do
        es `trigger` Internal (SetStatus Nothing)
        es `trigger` Internal (FetchedItems ts pis)
  continue (s & hsAsync ?~ fetchAsync)
internalEventHandler _ s FetchItems = continue s

internalEventHandler _ s (FetchedItems ts pis) = do
  let (newItems,toBeDeleted) = partition ((==Normal) . view status) pis
  continue (s & insertItems newItems
              & removeItems (toBeDeleted ^.. each . itemId)
              & hsAsync .~ Nothing
              & hsLastUpdated ?~ ts)
internalEventHandler _ s (SetStatus t) = continue (s & hsStatus .~ t)
internalEventHandler es s AsyncActionFailed = do
  liftIO (es `trigger` Internal (SetStatus (Just "failed")))
  continue (s & hsAsync .~ Nothing)
internalEventHandler es s@(view hsAsync -> Nothing) ArchiveItems =
  case hsNumItems s of
    (_,0) -> continue s
    (_,_) -> do
      archiveAsync <- liftIO . async $ do
        es `trigger` Internal (SetStatus (Just "archiving"))
        eitherErrorResults <- performArchive (s ^.. pendingList . L.listElementsL . each)
        case eitherErrorResults of
          Left _ -> es `trigger` Internal AsyncActionFailed
          Right results -> do
            es `trigger` Internal (
              ArchivedItems (
                  mapMaybe (\(pit,successful) -> mfilter (const successful)
                                                 (Just (view itemId pit))) results))
            es `trigger` Internal (SetStatus Nothing)
      continue (s & hsAsync ?~ archiveAsync)
internalEventHandler _ s ArchiveItems = continue s
internalEventHandler _ s (ArchivedItems pis) = continue (removeItems pis s)

eventHandler :: Chan HocketEvent -> HocketState -> HocketEvent -> EventM (Next HocketState)
eventHandler es s (VtyEvent e) = vtyEventHandler es s e
eventHandler es s (Internal e) = internalEventHandler es s e

main :: IO ()
main = do
  events <- newChan
  void (customMain (mkVty def) events (app events) initialState)

app :: Chan HocketEvent -> App HocketState HocketEvent
app events = App {appDraw = drawGui
                 ,appChooseCursor = Focus.focusRingCursor (view focusRing)
                 ,appHandleEvent = \s e -> fmap syncForRender <$> eventHandler events s e
                 ,appStartEvent = \s -> do liftIO (events `trigger` Internal FetchItems)
                                           return s
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
                 ,hBar " " <+> withAttr "bar" (padLeft Max (txt (maybe "<never>" (sformat F.hms . posixSecondsToUTCTime) (s ^. hsLastUpdated))))
                 ,txt (fromMaybe " " (s ^. hsStatus))
                 ]

focused :: HocketState -> Maybe Name
focused = Focus.focusGetCurrent . view focusRing

focusedList :: HocketState -> Maybe (L.List PocketItem)
focusedList s = case focused s of
                  Just n | n == itemListName -> s ^? itemList
                  Just n | n == pendingListName -> s ^? pendingList
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

retrieveItems :: Maybe POSIXTime -> IO (Either HttpException PocketItemBatch)
retrieveItems = tryHttpException
              . runHocket (pocketCredentials, def)
              . pocket
              . RetrieveItems
              . maybe retrieveAllUnread retrieveDeltaSince
  where retrieveAllUnread = defaultRetrieval
        retrieveDeltaSince ts = defaultRetrieval & retrieveSince ?~ ts
                                                 & retrieveState .~ All

performArchive :: [PocketItem] -> IO (Either HttpException [(PocketItem, Bool)])
performArchive items = tryHttpException
                     . fmap (zip items)
                     . runHocket (pocketCredentials, def)
                     . pocket
                     $ Batch (map (Archive . view itemId) items)

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
  return (strip "reddit.com/" (strip "www." (uriRegName auth)
                            <> uriPath parsed
                            <> uriQuery parsed))
  where strip prefix s = if prefix `isPrefixOf` s then drop (length prefix) s else s
