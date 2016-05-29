{-# LANGUAGE TemplateHaskell #-}
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
import qualified Data.CaseInsensitive as CI
import           Data.Default (def)
import           Data.Foldable (for_)
import           Data.List (isPrefixOf, partition, find)
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, POSIXTime)
import           Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone, TimeZone, utcToLocalTime)
import           Formatting (sformat, (%))
import qualified Formatting as F
import qualified Formatting.Time as F
import           Graphics.Vty (Event, mkVty, Key (KChar), Event (EvKey))
import qualified Graphics.Vty as Vty
import           Graphics.Vty.Input.Events (Key(KEnter))
import           Network.HTTP.Client (HttpException(StatusCodeException))
import           Network.URI
import           System.Process (shell, createProcess, createProcess, CreateProcess)
import           System.Process.Internals (StdStream(CreatePipe))
import           Text.Printf (printf)

import           Events
import           Network.Pocket
import           Network.Pocket.Retrieve
import           Network.Pocket.Ui.State

makeLensesFor [("std_err", "stdErr"), ("std_out", "stdOut")] ''CreateProcess

trigger :: Chan HocketEvent -> HocketEvent -> IO ()
trigger = writeChan

vtyEventHandler :: Chan HocketEvent -> HocketState -> Event -> EventM (Next HocketState)
vtyEventHandler es s (EvKey (KChar ' ') []) = do
  liftIO $ for_ (focusedItem s) $ \pit ->
    es `trigger` browseItemEvt pit
  continue s
vtyEventHandler es s (EvKey KEnter []) = do
  liftIO $ for_ (focusedItem s) $ \pit -> do
    es `trigger` browseItemEvt pit
    es `trigger` shiftItemEvt (pit ^. itemId)
  continue s
vtyEventHandler es s (EvKey (KChar 'u') []) = do
  liftIO $ es `trigger` fetchItemsEvt
  continue s
vtyEventHandler es s (EvKey (KChar 'A') []) = do
  liftIO $ es `trigger` archiveItemsEvt
  continue s
vtyEventHandler es s (EvKey (KChar 'd') []) = do
  liftIO $ for_ (focusedItem s) $ \pit -> es `trigger` shiftItemEvt (pit ^. itemId)
  continue s
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
internalEventHandler es s (InternalAsync e) = asyncCommandEventHandler es s e
internalEventHandler es s (InternalUi e) = uiCommandEventHandler es s e

asyncCommandEventHandler :: Chan HocketEvent -> HocketState -> AsyncCommand -> EventM (Next HocketState)
asyncCommandEventHandler es s@(view hsAsync -> Nothing) FetchItems = do
  fetchAsync <- liftIO . async $ do
    es `trigger` setStatusEvt (Just "fetching")
    eitherErrorPis <- retrieveItems (s ^. hsLastUpdated)
    case eitherErrorPis of
      Left e -> es `trigger` (asyncActionFailedEvt (errorMessageFromException e))
      Right (PocketItemBatch ts pis) -> do
        es `trigger` setStatusEvt Nothing
        es `trigger` fetchedItemsEvt ts pis
  continue (s & hsAsync ?~ fetchAsync)
asyncCommandEventHandler _ s FetchItems = continue s

asyncCommandEventHandler _ s (FetchedItems ts pis) = do
  let (newItems,toBeDeleted) = partition ((==Normal) . view status) pis
  continue (s & insertItems newItems
              & removeItems (toBeDeleted ^.. each . itemId)
              & hsAsync .~ Nothing
              & hsLastUpdated ?~ ts)
asyncCommandEventHandler es s (AsyncActionFailed err) = do
  liftIO (es `trigger` setStatusEvt (Just ("failed" <> maybe "" (": " <>) err)))
  continue (s & hsAsync .~ Nothing)
asyncCommandEventHandler es s@(view hsAsync -> Nothing) ArchiveItems =
  case hsNumItems s of
    (_,0) -> continue s
    (_,_) -> do
      archiveAsync <- liftIO . async $ do
        es `trigger` setStatusEvt (Just "archiving")
        eitherErrorResults <- performArchive (s ^.. pendingList . L.listElementsL . each)
        case eitherErrorResults of
          Left e -> es `trigger` asyncActionFailedEvt (errorMessageFromException e)
          Right results -> do
            es `trigger` archivedItemsEvt (
                  mapMaybe (\(pit,successful) -> mfilter (const successful)
                                                 (Just (view itemId pit))) results)
            es `trigger` setStatusEvt Nothing
      continue (s & hsAsync ?~ archiveAsync)
asyncCommandEventHandler _ s ArchiveItems = continue s
asyncCommandEventHandler _ s (ArchivedItems pis) = continue (s & removeItems pis
                                                                                 & hsAsync .~ Nothing)

uiCommandEventHandler :: Chan HocketEvent -> HocketState -> UiCommand -> EventM (Next HocketState)
uiCommandEventHandler _ s (ShiftItem pid) = continue (toggleStatus pid s)
uiCommandEventHandler _ s (RemoveItems pis) = continue (removeItems pis s)
uiCommandEventHandler _ s (SetStatus t) = continue (s & hsStatus .~ t)
uiCommandEventHandler _ s (BrowseItem pit) = do
  liftIO $ browseItem "firefox %s" (pit ^. resolvedUrl)
  continue s



eventHandler :: Chan HocketEvent -> HocketState -> HocketEvent -> EventM (Next HocketState)
eventHandler es s (VtyEvent e) = vtyEventHandler es s e
eventHandler es s (Internal e) = internalEventHandler es s e

main :: IO ()
main = do
  events <- newChan
  tz <- getCurrentTimeZone
  void (customMain (mkVty def) events (app tz events) initialState)

app :: TimeZone -> Chan HocketEvent -> App HocketState HocketEvent
app tz events = App {appDraw = drawGui tz
                    ,appChooseCursor = Focus.focusRingCursor (view focusRing)
                    ,appHandleEvent = \s e -> fmap syncForRender <$> eventHandler events s e
                    ,appStartEvent = \s -> do liftIO (events `trigger` fetchItemsEvt)
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

drawGui :: TimeZone -> HocketState -> [Widget]
drawGui tz s = [w]
  where w = vBox [hBar ("Hocket: ("
                     <> uncurry (sformat (F.int % "|" % F.int)) (hsNumItems s)
                     <> ")")
                 ,L.renderList (s ^. itemList) (listDrawElement (isFocused s itemListName))
                 ,hBorder
                 ,vLimit 10 $
                    L.renderList (s ^. pendingList) (listDrawElement (isFocused s pendingListName))
                 ,hBar " " <+> withAttr "bar" (padLeft Max (txt (maybe "<never>" (sformat F.hms . utcToLocalTime tz . posixSecondsToUTCTime) (s ^. hsLastUpdated))))
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

focusedItem :: HocketState -> Maybe PocketItem
focusedItem s = do
  list <- focusedList s
  snd <$> L.listSelectedElement list

browseItem :: String -> URL -> IO ()
browseItem shellCmd (URL url) = do
  let spec = shell $ printf shellCmd url
  void . createProcess $ spec & stdOut .~ CreatePipe
                              & stdErr .~ CreatePipe

errorMessageFromException :: HttpException -> Maybe Text
errorMessageFromException (StatusCodeException _ hs _) =
  fmap (T.decodeUtf8 . snd) $ (find (\(k,_) -> k == CI.mk ("x-error"))) hs
errorMessageFromException _ = Nothing
