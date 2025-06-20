{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Lens (at, makeLensesFor, view)
import Control.Lens.Combinators (use)
import Control.Lens.Operators
  ( (%=),
    (&),
    (.=),
    (.~),
    (<&>),
    (?=),
    (^.),
    (^?),
  )
import Control.Monad (mfilter, unless, void, when)
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logErrorN, logInfoN, runStdoutLoggingT)
import Control.Monad.Loops (unfoldrM)
import Control.Monad.State (MonadState)
import qualified Data.CaseInsensitive as CI
import Data.Foldable (for_)
import Data.List (find, findIndex, foldl', isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime
  ( TimeZone,
    getCurrentTimeZone,
    utcToLocalTime,
  )
import Dhall (auto, input)
import Events
  ( AsyncCommand (..),
    HocketEvent (..),
    UiCommand (..),
    archiveItemsEvt,
    archivedItemsEvt,
    asyncActionFailedEvt,
    browseItemEvt,
    clearAllFlagsEvt,
    fetchItemsEvt,
    fetchedItemsEvt,
    setAllFlagsToArchiveEvt,
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
import Network.Bookmark.Types
  ( BookmarkCredentials,
    BookmarkItem,
    BookmarkItemBatch (..),
    BookmarkItemId,
    BookmarkRequest (AddBookmark, BatchArchiveBookmarks, RetrieveBookmarks),
    PendingAction (..),
    RaindropCollectionId (RaindropCollectionId),
    URL (..),
    biCollectionId,
    biCreated,
    biExcerpt,
    biId,
    biImportant,
    biLastUpdate,
    biLink,
    biNote,
    biTitle,
    _BookmarkItemId,
  )
import Network.Bookmark.Ui.State
  ( HocketState,
    Name (..),
    clearAllFlags,
    focusRing,
    hsAsync,
    hsContents,
    hsCredentials,
    hsLastUpdated,
    hsNumItems,
    hsStatus,
    initialState,
    insertItems,
    itemList,
    removeItems,
    setAllFlagsToArchive,
    syncForRender,
    togglePendingAction,
  )
import Network.HTTP.Client
  ( HttpException (HttpExceptionRequest),
    HttpExceptionContent (StatusCodeException),
    responseHeaders,
    responseStatus,
  )
import Network.Raindrop (raindrop)
import qualified Network.Raindrop as R
import Network.URI
  ( URI (uriAuthority, uriPath, uriQuery),
    URIAuth (uriRegName),
    parseURI,
  )
import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
    ParserInfo,
    argument,
    command,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    hsubparser,
    info,
    long,
    many,
    metavar,
    optional,
    progDesc,
    str,
    strOption,
    (<**>),
  )
import System.Exit (exitFailure)
import System.Process
  ( CreateProcess,
    createProcess,
    shell,
    waitForProcess,
  )
import System.Process.Internals (StdStream (CreatePipe))
import Text.Printf (printf)

makeLensesFor [("std_err", "stdErr"), ("std_out", "stdOut")] ''CreateProcess

data HocketCommand
  = RunTUI
  | AddBookmarkCmd !Text !(Maybe Text) ![Text]
  deriving (Show, Eq)

tuiCommandParser :: Mod CommandFields HocketCommand
tuiCommandParser =
  command "tui" (info (pure RunTUI) (progDesc "Run the Hocket Terminal User Interface"))

addCommandParser :: Mod CommandFields HocketCommand
addCommandParser =
  command "add" (info addBookmarkParser (progDesc "Add a bookmark to Raindrop"))
  where
    addBookmarkParser =
      AddBookmarkCmd
        <$> argument str (metavar "URL" <> help "URL to bookmark")
        <*> optional (strOption (long "collection" <> help "Collection ID (defaults to -1 for unsorted)"))
        <*> many (strOption (long "tag" <> help "Tags to add"))

hocketCommandParser :: Parser HocketCommand
hocketCommandParser = hsubparser (tuiCommandParser <> addCommandParser)

opts :: ParserInfo HocketCommand
opts =
  info
    (hocketCommandParser <**> helper)
    ( fullDesc
        <> progDesc "Hocket - A bookmark management tool"
        <> header "hocket - Your command-line bookmark helper"
    )

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
vtyEventHandler es (EvKey (KChar 'r') []) = do
  liftIO $ es `trigger` fetchItemsEvt
  pure ()
vtyEventHandler es (EvKey (KChar 'R') []) = do
  liftIO $ es `trigger` archiveItemsEvt
  pure ()
vtyEventHandler es (EvKey (KChar 'a') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit ->
    unless (getPendingActionForItem (view biId bit) s == ToBeArchived) $
      es `trigger` shiftItemEvt (view biId bit)
vtyEventHandler es (EvKey (KChar 'u') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit ->
    when (getPendingActionForItem (view biId bit) s == ToBeArchived) $
      es `trigger` shiftItemEvt (view biId bit)
vtyEventHandler es (EvKey (KChar 'm') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit ->
    es `trigger` shiftItemEvt (view biId bit)
vtyEventHandler _ (EvKey (KChar 'J') []) = do
  s <- use id
  case findNextFlaggedItem s of
    Just newIdx -> itemList %= L.listMoveTo newIdx
    Nothing -> pure ()
vtyEventHandler _ (EvKey (KChar 'K') []) = do
  s <- use id  
  case findPrevFlaggedItem s of
    Just newIdx -> itemList %= L.listMoveTo newIdx
    Nothing -> pure ()
vtyEventHandler es (EvKey (KChar 'U') []) = do
  liftIO $ es `trigger` clearAllFlagsEvt
  pure ()
vtyEventHandler es (EvKey (KChar 'A') []) = do
  liftIO $ es `trigger` setAllFlagsToArchiveEvt
  pure ()
vtyEventHandler _ (EvKey (KChar 'q') []) = halt
vtyEventHandler _ e = do
  zoom itemList (handleListEventVi handleListEvent e)

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
            collectionToFetch =
              if isUpdateFetch
                then RaindropCollectionId "0"
                else RaindropCollectionId "-1"
            suffix = maybe "" (\since -> " since: " <> formatPOSIXTime (since - 86400)) $ s ^. hsLastUpdated
        es `trigger` setStatusEvt (Just ("fetching" <> suffix))
        eitherErrorBis <- retrieveItems (s ^. hsCredentials) searchParam collectionToFetch
        case eitherErrorBis of
          Left e ->
            es `trigger` asyncActionFailedEvt (errorMessageFromException e)
          Right batches -> do
            es `trigger` setStatusEvt Nothing
            for_ batches $ \(BookmarkItemBatch ts bis _) -> do
              es `trigger` fetchedItemsEvt ts bis isUpdateFetch
    hsAsync ?= fetchAsync
asyncCommandEventHandler _ (FetchedItems ts bis wasAllCollectionsFetch) = do
  if wasAllCollectionsFetch
    then do
      let itemsToPotentiallyAdd = filter (\item -> item ^. biCollectionId == -1) bis
          itemIdsToRemove = map (view biId) $ filter (\item -> item ^. biCollectionId /= -1) bis
      unless (null itemIdsToRemove) $
        id %= removeItems itemIdsToRemove
      id %= insertItems itemsToPotentiallyAdd
    else id %= insertItems bis

  hsAsync .= Nothing
  currentLastUpdated <- use hsLastUpdated
  let newTimestampToConsider = if null bis then Nothing else Just ts
  case (currentLastUpdated, newTimestampToConsider) of
    (Nothing, Just newTs) -> hsLastUpdated .= Just newTs
    (Just oldTs, Just newTs) -> when (newTs >= oldTs) $ hsLastUpdated .= Just newTs
    _ -> pure ()
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
            performArchive (s ^. hsCredentials) (getItemsWithPendingAction ToBeArchived s)
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
uiCommandEventHandler _ (ShiftItem bid) = do
  id %= togglePendingAction bid
  itemList %= L.listMoveDown
uiCommandEventHandler _ (RemoveItems bis) = id %= removeItems bis
uiCommandEventHandler _ (SetStatus t) = hsStatus .= t
uiCommandEventHandler _ ClearAllFlags = id %= clearAllFlags
uiCommandEventHandler _ SetAllFlagsToArchive = id %= setAllFlagsToArchive
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

runTuiApp :: IO ()
runTuiApp = do
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

runAddCommand :: Text -> Maybe Text -> [Text] -> IO ()
runAddCommand url mCollection tags = do
  result <- runStdoutLoggingT $ do
    logInfoN $ "Adding bookmark: " <> url
    cred <-
      liftIO (input auto "./config.dhall") `Catch.catch` \(e :: SomeException) -> do
        logErrorN $ "Error loading config: " <> T.pack (show e)
        liftIO exitFailure
    result <-
      R.raindrop cred (AddBookmark url mCollection tags) `Catch.catch` \(e :: SomeException) -> do
        logErrorN $ "Error adding bookmark: " <> T.pack (show e)
        return Nothing
    case result of
      Just bookmarkId -> do
        logInfoN $ "Successfully added bookmark with ID: " <> (bookmarkId ^. _BookmarkItemId)
        return True
      Nothing -> do
        logErrorN "Failed to add bookmark"
        return False
  unless result exitFailure

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    RunTUI -> runTuiApp
    AddBookmarkCmd url mCollection tags -> runAddCommand url mCollection tags

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
      (attrName "list" <> attrName "flaggedItem", flaggedRedFg),
      (attrName "list" <> attrName "flaggedSelected", flaggedRedSelectedFg),
      (attrName "bar", Vty.defAttr `Vty.withBackColor` Vty.black `Vty.withForeColor` Vty.white)
    ]

getDisplayContent :: BookmarkItem -> Text
getDisplayContent item =
  let noteText = item ^. biNote
      excerptText = item ^. biExcerpt
      hasNote = not (T.null noteText)
      hasExcerpt = not (T.null excerptText)
      formattedNote = if hasNote then "NOTE " <> T.replace "\n" " " noteText else T.empty
      formattedExcerpt = if hasExcerpt then "EXCERPT " <> T.replace "\n" " " excerptText else T.empty
   in case (hasNote, hasExcerpt) of
        (True, True) -> formattedNote <> " | " <> formattedExcerpt
        (True, False) -> formattedNote
        (False, True) -> formattedExcerpt
        (False, False) -> " "

drawGui :: TimeZone -> HocketState -> [Widget Name]
drawGui tz s = [w]
  where
    w =
      vBox
        [ hBarWithHints
            ( "Hocket: ("
                <> uncurry (sformat (F.int % "|" % F.int)) (hsNumItems s)
                <> ")"
            )
            "spc:Browse ent:Browse+flag r:Refresh R:Archive a:Flag u:Unflag m:Toggle J/K:Jump U:Unflag all A:Archive all q:Quit",
          hBorder,
          hBar
            ( maybe
                " "
                getDisplayContent
                (focusedItem s)
            ),
          hBorder,
          L.renderList
            (listDrawElementWithAction s)
            True
            (s ^. itemList),
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

focusedList :: HocketState -> Maybe (L.List Name BookmarkItem)
focusedList s = s ^? itemList

listDrawElementWithAction :: HocketState -> Bool -> BookmarkItem -> Widget Name
listDrawElementWithAction s sel e =
  let actionIndicator = case getPendingActionForItem (view biId e) s of
        ToBeArchived -> "A "
        None -> "  "
      pendingAction = getPendingActionForItem (view biId e) s
      attrName' = case (pendingAction, sel) of
        (ToBeArchived, True) -> attrName "list" <> attrName "flaggedSelected"
        (ToBeArchived, False) -> attrName "list" <> attrName "flaggedItem"
        (None, True) -> attrName "list" <> attrName "listSelected"
        (None, False) -> attrName "list" <> attrName "unselectedItem"
   in withAttr attrName' (txt actionIndicator <+> padRight Max (txtDisplay e))

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

flaggedRed :: Vty.Color
flaggedRed = Vty.rgbColor (220 :: Int) (85 :: Int) (85 :: Int)

flaggedRedDark :: Vty.Color
flaggedRedDark = Vty.rgbColor (80 :: Int) (20 :: Int) (20 :: Int)

whiteFg :: Vty.Attr
whiteFg = Vty.defAttr `Vty.withForeColor` Vty.white

flaggedRedFg :: Vty.Attr
flaggedRedFg = Vty.defAttr `Vty.withForeColor` flaggedRed

flaggedRedSelectedFg :: Vty.Attr
flaggedRedSelectedFg = Vty.defAttr `Vty.withForeColor` flaggedRedDark `Vty.withBackColor` orange `Vty.withStyle` Vty.bold

hBar :: Text -> Widget Name
hBar = withAttr (attrName "bar") . padRight Max . txt

hBarWithHints :: Text -> Text -> Widget Name
hBarWithHints leftText rightText = 
  withAttr (attrName "bar") (txt leftText <+> padLeft Max (txt rightText))

retrieveItems :: BookmarkCredentials -> Maybe Text -> RaindropCollectionId -> IO (Either HttpException [BookmarkItemBatch])
retrieveItems cred searchParam collectionId = do
  tryHttpException $
    runStdoutLoggingT $
      unfoldrM
        ( \currentPage -> do
            (_, items) <- raindrop cred (RetrieveBookmarks currentPage collectionId searchParam)

            let mostRecentUpdate =
                  if null items
                    then 0
                    else maximum (map (utcTimeToPOSIXSeconds . view biLastUpdate) items)

            pure $
              if null items
                then Nothing
                else Just (BookmarkItemBatch mostRecentUpdate items (fromIntegral $ length items), currentPage + 1)
        )
        0

performArchive :: BookmarkCredentials -> [BookmarkItem] -> IO (Either HttpException [(BookmarkItem, Bool)])
performArchive cred items = do
  tryHttpException $ runStdoutLoggingT $ do
    let itemIds = map (view biId) items
    success <- raindrop cred (BatchArchiveBookmarks itemIds)
    pure $ map (,success) items

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

getPendingActionForItem :: BookmarkItemId -> HocketState -> PendingAction
getPendingActionForItem bid s =
  case s ^. hsContents . at bid of
    Just (action, _) -> action
    Nothing -> None

getItemsWithPendingAction :: PendingAction -> HocketState -> [BookmarkItem]
getItemsWithPendingAction targetAction s =
  [item | (action, item) <- Map.elems (s ^. hsContents), action == targetAction]


-- Find next flagged item
findNextFlaggedItem :: HocketState -> Maybe Int
findNextFlaggedItem s = do
  list <- focusedList s
  currentIdx <- view L.listSelectedL list
  let items = V.toList $ view L.listElementsL list
      remainingItems = drop (currentIdx + 1) items
  case findIndex (\item -> getPendingActionForItem (view biId item) s == ToBeArchived) remainingItems of
    Just relativeIdx -> Just (currentIdx + 1 + relativeIdx)
    Nothing -> Nothing

-- Find previous flagged item
findPrevFlaggedItem :: HocketState -> Maybe Int
findPrevFlaggedItem s = do
  list <- focusedList s
  currentIdx <- view L.listSelectedL list
  let items = V.toList $ view L.listElementsL list
      precedingItems = reverse $ take currentIdx items
  case findIndex (\item -> getPendingActionForItem (view biId item) s == ToBeArchived) precedingItems of
    Just relativeIdx -> Just (currentIdx - 1 - relativeIdx)
    Nothing -> Nothing

urlReplacements :: [(String, String)]
urlReplacements =
  [ ("m.imdb.", "imdb."),
    ("m.aliexpress.", "aliexpress.")
  ]

replace :: String -> String -> String -> String
replace old new = go
  where
    go [] = []
    go s@(x : xs)
      | old `isPrefixOf` s = new ++ go (drop (length old) s)
      | otherwise = x : go xs

-- AI? suggest how to add tests for this
cleanUrl :: String -> String
cleanUrl s = foldl' (\acc (old, new) -> replace old new acc) s urlReplacements

browseItem :: String -> URL -> IO ()
browseItem shellCmd (URL url) = do
  let cleanedUrl = cleanUrl url
      spec = shell $ printf shellCmd cleanedUrl
  (_, _, _, ph) <- createProcess $ spec & stdOut .~ CreatePipe & stdErr .~ CreatePipe
  void . waitForProcess $ ph

errorMessageFromException :: HttpException -> Maybe Text
errorMessageFromException (HttpExceptionRequest _ (StatusCodeException resp _)) = msg
  where
    msg = xError <|> code
    xError = T.decodeUtf8 . snd <$> find (\(k, _) -> k == CI.mk "x-error") (responseHeaders resp)
    code = Just . T.pack $ "Got status: " <> (show . responseStatus $ resp)
errorMessageFromException e = Just . T.pack $ show e
