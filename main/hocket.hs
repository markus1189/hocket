{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Main
  ( main,
  )
where

import Brick
  ( App (..),
    AttrMap,
    BrickEvent (AppEvent, VtyEvent),
    EventM,
    Padding (Max),
    Widget,
    attrMap,
    attrName,
    customMain,
    hLimit,
    halt,
    padLeft,
    padRight,
    txt,
    vBox,
    vLimit,
    withAttr,
    zoom,
    (<+>),
  )
import Brick.BChan (BChan, newBChan, writeBChan)
import qualified Brick.Focus as Focus
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.List (handleListEvent, handleListEventVi)
import qualified Brick.Widgets.List as L
import Control.Applicative ((<|>))
import Control.Concurrent.Async (async)
import Control.Exception (SomeException)
import Control.Exception.Base (try)
import Control.Lens (Each (each), makeLensesFor, view)
import Control.Lens.Combinators (use)
import Control.Lens.Operators
  ( (%=),
    (&),
    (.=),
    (.~),
    (<&>),
    (?=),
    (^.),
    (^..),
    (^?),
  )
import Control.Monad (mfilter, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState)
import Control.Monad.Loops (unfoldrM)
import qualified Data.CaseInsensitive as CI
import Data.Foldable (for_)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.LocalTime
  ( TimeZone,
    getCurrentTimeZone,
    utcToLocalTime,
  )
import Data.Time.Format (formatTime, defaultTimeLocale)
import Dhall (auto, input)
import Events
  ( AsyncCommand (..),
    HocketEvent (..),
    UiCommand (..),
    archiveItemsEvt,
    archivedItemsEvt,
    asyncActionFailedEvt,
    browseItemEvt,
    fetchItemsEvt,
    fetchedItemsEvt,
    setStatusEvt,
    shiftItemEvt,
  )
import Formatting (sformat, (%))
import qualified Formatting as F
import qualified Formatting.Time as F
import Graphics.Vty (Event (EvKey), Key (KChar))
import qualified Graphics.Vty as Vty
import Graphics.Vty.Input.Events (Key (KEnter))
import Graphics.Vty.Platform.Unix (mkVty)
import Network.HTTP.Client
  ( HttpException (HttpExceptionRequest),
    HttpExceptionContent (StatusCodeException),
    responseHeaders,
    responseStatus,
  )
import Network.Bookmark.Types
  ( BookmarkCredentials,
    BookmarkItem,
    BookmarkItemBatch (..),
    URL (..),
    RaindropCollectionId (RaindropCollectionId),
    BookmarkRequest (BatchArchiveBookmarks, RetrieveBookmarks),
    biId,
    biLink,
     biExcerpt,
     biNote,
    biTitle,
    biCreated,
    biLastUpdate,
    biCollectionId,
    biImportant,
  )
import Network.Bookmark.Ui.State
  ( HocketState,
    Name (..),
    focusRing,
    hsAsync,
    hsCredentials,
    hsLastUpdated,
    hsNumItems,
    hsStatus,
    initialState,
    insertItems,
    itemList,
    pendingList,
    removeItems,
    syncForRender,
    toggleStatus,
  )
import Network.Raindrop (raindrop)
import Network.URI
  ( URI (uriAuthority, uriPath, uriQuery),
    URIAuth (uriRegName),
    parseURI,
  )
import System.Process
  ( CreateProcess,
    createProcess,
    shell,
    waitForProcess,
  )
import System.Process.Internals (StdStream (CreatePipe))
import Text.Printf (printf)

makeLensesFor [("std_err", "stdErr"), ("std_out", "stdOut")] ''CreateProcess

trigger :: BChan HocketEvent -> HocketEvent -> IO ()
trigger = writeBChan

vtyEventHandler ::
  BChan HocketEvent ->
  Event ->
  EventM Name HocketState ()
vtyEventHandler es (EvKey (KChar ' ') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit -> es `trigger` browseItemEvt bit
vtyEventHandler es (EvKey KEnter []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit -> do
    es `trigger` browseItemEvt bit
    es `trigger` shiftItemEvt (view biId bit)
vtyEventHandler es (EvKey (KChar 'u') []) = do
  liftIO $ es `trigger` fetchItemsEvt
  pure ()
vtyEventHandler es (EvKey (KChar 'A') []) = do
  liftIO $ es `trigger` archiveItemsEvt
  pure ()
vtyEventHandler es (EvKey (KChar 'd') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit ->
    es `trigger` shiftItemEvt (view biId bit)
vtyEventHandler _ (EvKey (KChar 'q') []) = halt
vtyEventHandler _ (EvKey (KChar '\t') []) = focusRing %= Focus.focusNext
vtyEventHandler _ e = do
  s <- use id
  case focused s of
    Just ItemListName -> zoom itemList (handleListEventVi handleListEvent e)
    Just PendingListName ->
      zoom pendingList (handleListEventVi handleListEvent e)
    _ -> pure ()

internalEventHandler ::
  BChan HocketEvent ->
  HocketEvent ->
  EventM Name HocketState ()
internalEventHandler es (HocketAsync e) = asyncCommandEventHandler es e
internalEventHandler es (HocketUi e) = uiCommandEventHandler es e

unlessAsyncRunning :: (MonadState HocketState m) => m () -> m ()
unlessAsyncRunning act = do
  asyncRunning <- use id <&> isJust . view hsAsync
  unless asyncRunning act

formatPOSIXTime :: POSIXTime -> Text
formatPOSIXTime t = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" (posixSecondsToUTCTime t)

asyncCommandEventHandler ::
  BChan HocketEvent ->
  AsyncCommand ->
  EventM Name HocketState ()
asyncCommandEventHandler es FetchItems = do
  s <- use id
  unlessAsyncRunning $ do
    fetchAsync <-
      liftIO . async $ do
        let searchParam = case s ^. hsLastUpdated of
              Nothing -> Nothing
              Just lastTime ->
                let dayBefore = lastTime - 86400
                    dateStr = formatPOSIXTime dayBefore
                in Just ("lastUpdate:>" <> dateStr)
            isUpdateFetch = isJust (s ^. hsLastUpdated)
            collectionToFetch = if isUpdateFetch
                                    then RaindropCollectionId "0" -- All items for updates
                                    else RaindropCollectionId "-1" -- Unsorted for initial fetch
            suffix = maybe "" (\since -> " since: " <> formatPOSIXTime (since - 86400)) $ s ^. hsLastUpdated
        es `trigger` setStatusEvt (Just ("fetching" <> suffix))
        eitherErrorBis <- retrieveItems (s ^. hsCredentials) searchParam collectionToFetch
        case eitherErrorBis of
          Left e ->
            es `trigger` asyncActionFailedEvt (errorMessageFromException e)
          Right batches -> do
            es `trigger` setStatusEvt Nothing
            for_ batches $ \(BookmarkItemBatch ts bis _) -> do
              es `trigger` fetchedItemsEvt ts bis isUpdateFetch -- Pass the flag
    hsAsync ?= fetchAsync
asyncCommandEventHandler _ (FetchedItems ts bis wasAllCollectionsFetch) = do
  if wasAllCollectionsFetch
    then do
      let itemsToPotentiallyAdd = filter (\item -> item ^. biCollectionId == -1) bis
          itemIdsToRemove = map (view biId) $ filter (\item -> item ^. biCollectionId /= -1) bis
      unless (null itemIdsToRemove) $
        id %= removeItems itemIdsToRemove
      id %= insertItems itemsToPotentiallyAdd
    else
      id %= insertItems bis

  hsAsync .= Nothing -- Reset async status
  -- Update hsLastUpdated, ensuring it only moves forward or sets if Nothing
  currentLastUpdated <- use hsLastUpdated
  let newTimestampToConsider = if null bis then Nothing else Just ts
  case (currentLastUpdated, newTimestampToConsider) of
    (Nothing, Just newTs) -> hsLastUpdated .= Just newTs
    (Just oldTs, Just newTs) -> when (newTs >= oldTs) $ hsLastUpdated .= Just newTs
    _ -> pure () -- No change if new items are older or no new items
asyncCommandEventHandler es (AsyncActionFailed err) = do
  hsAsync .= Nothing
  liftIO (es `trigger` setStatusEvt (Just ("failed" <> maybe "<no err>" (": " <>) err)))
asyncCommandEventHandler es ArchiveItems = do
  s <- use id
  case hsNumItems s of
    (_, 0) -> pure ()
    (_, _) -> do
      archiveAsync <-
        liftIO . async $ do
          es `trigger` setStatusEvt (Just "archiving")
          eitherErrorResults <-
            performArchive (s ^. hsCredentials) (s ^.. pendingList . L.listElementsL . each)
          case eitherErrorResults of
            Left e ->
              es `trigger` asyncActionFailedEvt (errorMessageFromException e)
            Right results -> do
              es
                `trigger` archivedItemsEvt
                  ( mapMaybe
                      ( \(bit, successful) ->
                          mfilter (const successful) (Just (view biId bit))
                      )
                      results
                  )
              es `trigger` setStatusEvt Nothing
      hsAsync ?= archiveAsync
asyncCommandEventHandler _ (ArchivedItems bis) = do
  id %= removeItems bis
  hsAsync .= Nothing

uiCommandEventHandler ::
  BChan HocketEvent ->
  UiCommand ->
  EventM Name HocketState ()
uiCommandEventHandler _ (ShiftItem bid) = id %= toggleStatus bid
uiCommandEventHandler _ (RemoveItems bis) = id %= removeItems bis
uiCommandEventHandler _ (SetStatus t) = hsStatus .= t
uiCommandEventHandler es (BrowseItem bit) = do
  res <- liftIO . try @SomeException $ browseItem "firefox '%s'" (URL . T.unpack $ view biLink bit)
  case res of
    Left e -> liftIO $ es `trigger` setStatusEvt (Just (T.pack $ show e))
    Right () -> pure ()

myEventHandler ::
  BChan HocketEvent ->
  BrickEvent Name HocketEvent ->
  EventM Name HocketState ()
myEventHandler es (VtyEvent e) = vtyEventHandler es e
myEventHandler es (AppEvent e) = internalEventHandler es e
myEventHandler _ _ = pure ()

main :: IO ()
main = do
  cred <- input auto "./config.dhall"
  events <- newBChan 10
  tz <- getCurrentTimeZone
  vty <- mkVty Vty.defaultConfig
  void
    ( customMain
        vty
        (mkVty Vty.defaultConfig)
        (Just events)
        (app tz events)
        (initialState cred)
    )

app :: TimeZone -> BChan HocketEvent -> App HocketState HocketEvent Name
app tz events =
  App
    { appDraw = drawGui tz,
      appChooseCursor = Focus.focusRingCursor (view focusRing),
      appHandleEvent = \e -> do
        myEventHandler events e
        id %= syncForRender,
      appStartEvent = liftIO (events `trigger` fetchItemsEvt),
      appAttrMap = const hocketAttrMap
    }

hocketAttrMap :: AttrMap
hocketAttrMap =
  attrMap
    Vty.defAttr
    [ (attrName "list" <> attrName "selected" <> attrName "focused", boldBlackOnOrange),
      (attrName "list" <> attrName "listSelected", Vty.defAttr `Vty.withStyle` Vty.bold),
      (attrName "list" <> attrName "unselectedItem", whiteFg),
      ( attrName "bar",
        Vty.defAttr `Vty.withBackColor` Vty.black `Vty.withForeColor` Vty.white
      )
    ]

getDisplayContent :: BookmarkItem -> Text
getDisplayContent item
  | not (T.null (item ^. biNote)) = "NOTE " <> T.replace "\n" " " (item ^. biNote)
  | not (T.null (item ^. biExcerpt)) = "EXCERPT " <> T.replace "\n" " " (item ^. biExcerpt)
  | otherwise = " "

drawGui :: TimeZone -> HocketState -> [Widget Name]
drawGui tz s = [w]
  where
    w =
      vBox
        [ hBar
            ( "Hocket: ("
                <> uncurry (sformat (F.int % "|" % F.int)) (hsNumItems s)
                <> ")"
            ),
          L.renderList
            listDrawElement
            (isFocused s ItemListName)
            (s ^. itemList),
          hBorder,
          vLimit 10 $
            L.renderList
              listDrawElement
              (isFocused s PendingListName)
              (s ^. pendingList),
          hBar
            ( maybe
                " "
                getDisplayContent
                (focusedItem s)
            ),
          hBar " "
            <+> withAttr
              (attrName "bar")
              ( padLeft
                  Max
                  ( txt
                      ( maybe
                          "<never>"
                          (sformat F.hms . utcToLocalTime tz . posixSecondsToUTCTime)
                          (s ^. hsLastUpdated)
                      )
                  )
              ),
          txt (fromMaybe " " (s ^. hsStatus))
        ]

focused :: HocketState -> Maybe Name
focused = Focus.focusGetCurrent . view focusRing

focusedList :: HocketState -> Maybe (L.List Name BookmarkItem)
focusedList s =
  case focused s of
    Just ItemListName -> s ^? itemList
    Just PendingListName -> s ^? pendingList
    _ -> Nothing

isFocused :: HocketState -> Name -> Bool
isFocused s name = Just name == focused s

listDrawElement :: Bool -> BookmarkItem -> Widget Name
listDrawElement sel e =
  ( if sel
      then withAttr (attrName "list" <> attrName "listSelected")
      else withAttr (attrName "list" <> attrName "unselectedItem")
  )
    (padRight Max (txtDisplay e))

orange :: Vty.Color
orange = Vty.rgbColor 215 135 (0 :: Int)

boldBlackOnOrange :: Vty.Attr
boldBlackOnOrange =
  Vty.defAttr
    `Vty.withForeColor` black
    `Vty.withBackColor` orange
    `Vty.withStyle` Vty.bold

black :: Vty.Color
black = Vty.rgbColor zero zero zero
  where
    zero = 0 :: Int

whiteFg :: Vty.Attr
whiteFg = Vty.defAttr `Vty.withForeColor` Vty.white

hBar :: Text -> Widget Name
hBar = withAttr (attrName "bar") . padRight Max . txt

retrieveItems :: BookmarkCredentials -> Maybe Text -> RaindropCollectionId -> IO (Either HttpException [BookmarkItemBatch])
retrieveItems cred searchParam collectionId = do
  tryHttpException $ unfoldrM (\currentPage -> do
    (_, items) <- raindrop cred (RetrieveBookmarks currentPage collectionId searchParam)

    let mostRecentUpdate = if null items
                          then 0
                          else maximum (map (utcTimeToPOSIXSeconds . view biLastUpdate) items)

    pure $ if length items == 0
      then Nothing
      else Just (BookmarkItemBatch mostRecentUpdate items (fromIntegral $ length items), currentPage + 1)
    ) 0

performArchive :: BookmarkCredentials -> [BookmarkItem] -> IO (Either HttpException [(BookmarkItem, Bool)])
performArchive cred items = do
  tryHttpException $ do
    let itemIds = map (view biId) items
    success <- raindrop cred (BatchArchiveBookmarks itemIds)
    pure $ map (, success) items

tryHttpException :: IO a -> IO (Either HttpException a)
tryHttpException = try @HttpException

txtDisplay :: BookmarkItem -> Widget Name
txtDisplay bit =
  txt (T.justifyRight 10 ' ' leftEdge)
    <+> txt favoriteIndicator
    <+> txt
      ( fromMaybe
          "<empty>"
          (find (not . T.null) [view biTitle bit, T.pack url])
      )
    <+> padLeft Max (hLimit horizontalUriLimit (txt trimmedUrl))
  where
    url = T.unpack (view biLink bit)
    added = view biCreated bit
    favoriteIndicator = if view biImportant bit then "★" else " "
    leftEdge = sformat (F.year % "-" <> F.month % "-" <> F.dayOfMonth) added <> ":"
    trimmedUrl = T.pack (trimURI url)

horizontalUriLimit :: Int
horizontalUriLimit = 60

trimURI :: String -> String
trimURI uri =
  fromMaybe uri $ do
    parsed <- parseURI uri
    auth <- uriAuthority parsed
    return
      ( strip
          "reddit.com/"
          (strip "www." (uriRegName auth) <> uriPath parsed <> uriQuery parsed)
      )
  where
    strip prefix s =
      if prefix `isPrefixOf` s
        then drop (length prefix) s
        else s

focusedItem :: HocketState -> Maybe BookmarkItem
focusedItem s = do
  list <- focusedList s
  snd <$> L.listSelectedElement list

browseItem :: String -> URL -> IO ()
browseItem shellCmd (URL url) = do
  let spec = shell $ T.unpack $ T.replace "m.aliexpress.us" "aliexpress.us" $ T.pack $ printf shellCmd url
  (_, _, _, ph) <- createProcess $ spec & stdOut .~ CreatePipe & stdErr .~ CreatePipe
  void . waitForProcess $ ph

errorMessageFromException :: HttpException -> Maybe Text
errorMessageFromException (HttpExceptionRequest _ (StatusCodeException resp _)) = msg
  where
    msg = xError <|> code
    xError = T.decodeUtf8 . snd <$> find (\(k, _) -> k == CI.mk "x-error") (responseHeaders resp)
    code = Just . T.pack $ "Got status: " <> (show . responseStatus $ resp)
errorMessageFromException e = Just . T.pack $ show e
