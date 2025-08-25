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
    (?~),
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
import Data.Time (UTCTime, addDays, getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime
  ( LocalTime (LocalTime),
    TimeOfDay (TimeOfDay),
    TimeZone,
    getCurrentTimeZone,
    localDay,
    localTimeToUTC,
    utcToLocalTime,
  )
import qualified Data.Vector as V
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
    editItemInBrowserEvt,
    fetchItemsEvt,
    fetchedItemsEvt,
    remindersRemovedEvt,
    remindersSetEvt,
    removeRemindersEvt,
    setRemindersEvt,
    setStatusEvt,
    shiftItemEvt,
    shiftItemReminderEvt,
    toggleInvertedVideoFilterEvt,
    toggleRemindersEvt,
    toggleVideoFilterEvt,
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
    BookmarkRequest (AddBookmark, BatchArchiveBookmarks, RemoveReminder, RetrieveBookmarks, SetReminder),
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
    biReminder,
    biTitle,
    _BookmarkItemId,
  )
import Network.Bookmark.Ui.State
  ( HocketState,
    Name (..),
    VideoFilterMode (..),
    clearAllFlags,
    clearFlagsForItems,
    focusRing,
    hsAsync,
    hsContents,
    hsCredentials,
    hsLastUpdated,
    hsNumItems,
    hsStatus,
    hsVideoFilter,
    icFutureReminders,
    icNone,
    icReminderToBeRemoved,
    icToBeArchived,
    icToBeReminded,
    initialState,
    insertItems,
    itemList,
    removeItems,
    removeReminderFromItems,
    setAllFlagsToArchive,
    syncForRender,
    toggleInvertedVideoFilter,
    togglePendingAction,
    togglePendingActionToReminder,
    toggleShowFutureReminders,
    toggleVideoFilter,
    updateItemsWithStoredReminderTimes,
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
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
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
vtyEventHandler es (EvKey (KChar 'X') []) = do
  liftIO $ es `trigger` archiveItemsEvt
  liftIO $ es `trigger` setRemindersEvt
  liftIO $ es `trigger` removeRemindersEvt
  pure ()
vtyEventHandler es (EvKey (KChar 'a') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit ->
    unless (getPendingActionForItem (view biId bit) s == ToBeArchived) $
      es `trigger` shiftItemEvt (view biId bit)
vtyEventHandler es (EvKey (KChar 's') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit ->
    unless (isToBeReminded (getPendingActionForItem (view biId bit) s)) $
      es `trigger` shiftItemReminderEvt (view biId bit)
vtyEventHandler es (EvKey (KChar 'u') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit -> do
    let action = getPendingActionForItem (view biId bit) s
    when (action == ToBeArchived) $
      es `trigger` shiftItemEvt (view biId bit)
    when (isReminderAction action) $
      es `trigger` shiftItemReminderEvt (view biId bit)
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
vtyEventHandler es (EvKey (KChar 'S') []) = do
  liftIO $ es `trigger` toggleRemindersEvt
  pure ()
vtyEventHandler es (EvKey (KChar 'v') []) = do
  liftIO $ es `trigger` toggleVideoFilterEvt
  pure ()
vtyEventHandler es (EvKey (KChar 'V') []) = do
  liftIO $ es `trigger` toggleInvertedVideoFilterEvt
  pure ()
vtyEventHandler es (EvKey (KChar 'e') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit -> es `trigger` editItemInBrowserEvt bit
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

nextDayAt7AM :: IO UTCTime
nextDayAt7AM = do
  tz <- getCurrentTimeZone
  now <- getCurrentTime
  let localNow = utcToLocalTime tz now
      tomorrow = addDays 1 (localDay localNow)
      sevenAM = TimeOfDay 7 0 0
      tomorrowAt7AM = LocalTime tomorrow sevenAM
  pure $ localTimeToUTC tz tomorrowAt7AM

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
  let counts = hsNumItems s
  case counts ^. icToBeArchived of
    0 -> pure ()
    _ -> do
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
asyncCommandEventHandler es SetReminders = do
  s <- use id
  let counts = hsNumItems s
  case counts ^. icToBeReminded of
    0 -> pure ()
    _ -> do
      reminderAsync <-
        liftIO . async $ do
          es `trigger` setStatusEvt (Just "setting reminders")
          eitherErrorResults <-
            performSetReminders (s ^. hsCredentials) (getItemsToBeReminded s)
          case eitherErrorResults of
            Left e ->
              es `trigger` asyncActionFailedEvt (errorMessageFromException e)
            Right results -> do
              es
                `trigger` remindersSetEvt
                  ( mapMaybe
                      ( \(bit, successful) ->
                          mfilter (const successful) (Just (view biId bit))
                      )
                      results
                  )
              es `trigger` setStatusEvt Nothing
      hsAsync ?= reminderAsync
asyncCommandEventHandler _ (RemindersSet bis) = do
  id %= updateItemsWithStoredReminderTimes bis
  id %= clearToBeRemindedFlags bis
  hsAsync .= Nothing
asyncCommandEventHandler es RemoveReminders = do
  s <- use id
  let counts = hsNumItems s
  case counts ^. icReminderToBeRemoved of
    0 -> pure ()
    _ -> do
      removeAsync <-
        liftIO . async $ do
          es `trigger` setStatusEvt (Just "removing reminders")
          eitherErrorResults <-
            performRemoveReminders (s ^. hsCredentials) (getItemsWithPendingAction ReminderToBeRemoved s)
          case eitherErrorResults of
            Left e ->
              es `trigger` asyncActionFailedEvt (errorMessageFromException e)
            Right results -> do
              es
                `trigger` remindersRemovedEvt
                  ( mapMaybe
                      ( \(bit, successful) ->
                          mfilter (const successful) (Just (view biId bit))
                      )
                      results
                  )
              es `trigger` setStatusEvt Nothing
      hsAsync ?= removeAsync
asyncCommandEventHandler _ (RemindersRemoved bis) = do
  id %= removeReminderFromItems bis
  id %= clearFlagsForItems ReminderToBeRemoved bis
  hsAsync .= Nothing

uiCommandEventHandler ::
  BChan HocketEvent ->
  UiCommand ->
  EventM Name HocketState ()
uiCommandEventHandler _ (ShiftItem bid) = do
  id %= togglePendingAction bid
  itemList %= L.listMoveDown
uiCommandEventHandler _ (ShiftItemReminder bid) = do
  reminderTime <- liftIO nextDayAt7AM
  id %= togglePendingActionToReminder bid reminderTime
  itemList %= L.listMoveDown
uiCommandEventHandler _ (RemoveItems bis) = id %= removeItems bis
uiCommandEventHandler _ (SetStatus t) = hsStatus .= t
uiCommandEventHandler _ ClearAllFlags = id %= clearAllFlags
uiCommandEventHandler _ SetAllFlagsToArchive = id %= setAllFlagsToArchive
uiCommandEventHandler _ ToggleReminders = do
  id %= toggleShowFutureReminders
  id %= syncForRender
uiCommandEventHandler _ ToggleVideoFilter = do
  id %= toggleVideoFilter
  id %= syncForRender
uiCommandEventHandler _ ToggleInvertedVideoFilter = do
  id %= toggleInvertedVideoFilter
  id %= syncForRender
uiCommandEventHandler es (BrowseItem bit) = do
  res <- liftIO . try @SomeException $ browseItem "firefox '%s'" (URL . T.unpack $ view biLink bit)
  case res of
    Left e -> liftIO $ es `trigger` setStatusEvt (Just (T.pack $ show e))
    Right () -> pure ()
uiCommandEventHandler es (EditItemInBrowser bit) = do
  let itemId = view biId bit ^. _BookmarkItemId
      editUrl = "https://app.raindrop.io/my/-1/item/" <> T.unpack itemId <> "/edit"
  res <- liftIO . try @SomeException $ browseItem "xdg-open '%s'" (URL editUrl)
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

getConfigPath :: IO FilePath
getConfigPath = do
  xdgConfigDir <- getXdgDirectory XdgConfig "hocket"
  let xdgConfigPath = xdgConfigDir </> "config.dhall"
      legacyConfigPath = "./config.dhall"

  legacyExists <- doesFileExist legacyConfigPath
  xdgExists <- doesFileExist xdgConfigPath

  case (legacyExists, xdgExists) of
    (True, False) -> do
      hPutStrLn stderr $ "Warning: Using legacy config location: " <> legacyConfigPath
      hPutStrLn stderr $ "Consider moving config to: " <> xdgConfigPath
      return legacyConfigPath
    (False, True) -> return xdgConfigPath
    (True, True) -> do
      hPutStrLn stderr $ "Warning: Found config in both locations, using XDG: " <> xdgConfigPath
      return xdgConfigPath
    (False, False) -> do
      createDirectoryIfMissing True xdgConfigDir
      return xdgConfigPath

ensureSchemaFile :: IO ()
ensureSchemaFile = do
  xdgConfigDir <- getXdgDirectory XdgConfig "hocket"
  let xdgSchemaPath = xdgConfigDir </> "schema.dhall"
      legacySchemaPath = "./schema.dhall"

  xdgSchemaExists <- doesFileExist xdgSchemaPath
  legacySchemaExists <- doesFileExist legacySchemaPath

  unless xdgSchemaExists $ do
    createDirectoryIfMissing True xdgConfigDir
    if legacySchemaExists
      then do
        legacyContent <- readFile legacySchemaPath
        writeFile xdgSchemaPath legacyContent
      else writeFile xdgSchemaPath "{ _raindropToken : Text, _archiveCollectionId : Natural }\n"

runTuiApp :: IO ()
runTuiApp = do
  ensureSchemaFile
  configPath <- getConfigPath
  cred <- input auto (T.pack configPath)
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
    liftIO ensureSchemaFile
    configPath <- liftIO getConfigPath
    cred <-
      liftIO (input auto (T.pack configPath)) `Catch.catch` \(e :: SomeException) -> do
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
      (attrName "list" <> attrName "reminderItem", reminderBlueFg),
      (attrName "list" <> attrName "reminderSelected", reminderBlueSelectedFg),
      (attrName "list" <> attrName "reminderFlaggedItem", reminderFlaggedFg),
      (attrName "list" <> attrName "reminderFlaggedSelected", reminderFlaggedSelectedFg),
      (attrName "list" <> attrName "reminderRemovalItem", reminderRemovalFg),
      (attrName "list" <> attrName "reminderRemovalSelected", reminderRemovalSelectedFg),
      (attrName "list" <> attrName "favoriteItem", favoriteYellowFg),
      (attrName "list" <> attrName "favoriteSelected", favoriteYellowSelectedFg),
      (attrName "bar", Vty.defAttr `Vty.withBackColor` Vty.black `Vty.withForeColor` Vty.white)
    ]

cleanUnicodeText :: Text -> Text
cleanUnicodeText = T.map translateUnicodeChar
  where
    translateUnicodeChar c = if isValidChar c then c else '.'
    isValidChar c = case c of
      '\t' -> True
      '\n' -> True
      '\r' -> True
      _ -> c >= ' ' && c <= '\DEL'

getDisplayContent :: BookmarkItem -> Text
getDisplayContent item =
  let noteText = item ^. biNote
      excerptText = item ^. biExcerpt
      reminderDate = item ^. biReminder
      hasNote = not (T.null noteText)
      hasExcerpt = not (T.null excerptText)
      hasReminder = isJust reminderDate
      formattedNote = if hasNote then "NOTE " <> T.replace "\n" " " (cleanUnicodeText noteText) else T.empty
      formattedReminder = case reminderDate of
        Just reminder -> "REMINDER " <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d" reminder) <> " "
        Nothing -> T.empty
      formattedExcerpt = if hasExcerpt then "EXCERPT " <> T.replace "\n" " " (cleanUnicodeText excerptText) else T.empty
   in case (hasNote, hasReminder, hasExcerpt) of
        (True, True, True) -> formattedNote <> " | " <> formattedReminder <> formattedExcerpt
        (True, True, False) -> formattedNote <> " | " <> formattedReminder
        (True, False, True) -> formattedNote <> " | " <> formattedExcerpt
        (True, False, False) -> formattedNote
        (False, True, True) -> formattedReminder <> formattedExcerpt
        (False, True, False) -> formattedReminder
        (False, False, True) -> formattedExcerpt
        (False, False, False) -> " "

drawGui :: TimeZone -> HocketState -> [Widget Name]
drawGui tz s = [w]
  where
    w =
      vBox
        [ hBarWithHints
            ( "Hocket"
                <> ( case s ^. hsVideoFilter of
                       NoVideoFilter -> ""
                       ShowOnlyVideos -> " (+V)"
                       HideVideos -> " (-V)"
                   )
                <> ": "
                <> ( \counts ->
                       let base =
                             sformat
                               ("(" % F.int % "|" % F.int % ")")
                               (counts ^. icNone)
                               (counts ^. icToBeArchived + counts ^. icToBeReminded + counts ^. icReminderToBeRemoved)
                           reminderPart =
                             if counts ^. icFutureReminders > 0
                               then sformat (" (" % F.int % ")") (counts ^. icFutureReminders)
                               else ""
                        in base <> reminderPart
                   )
                  (hsNumItems s)
            )
            "spc:Browse ent:Browse+flag e:Edit r:Refresh S:Toggle future reminders v:Video filter V:Hide videos X:Execute Flags a:Archive flag s:Reminder flag u:Unflag J/K:Jump U:Unflag all q:Quit",
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
        ToBeReminded _ -> "R "
        ReminderToBeRemoved -> "r "
        None -> "  "
      pendingAction = getPendingActionForItem (view biId e) s
      hasReminder = isJust (view biReminder e)
      isFavorite = view biImportant e
      attrName' = case (pendingAction, hasReminder, isFavorite, sel) of
        (ToBeArchived, _, _, True) -> attrName "list" <> attrName "flaggedSelected"
        (ToBeArchived, _, _, False) -> attrName "list" <> attrName "flaggedItem"
        (ToBeReminded _, _, _, True) -> attrName "list" <> attrName "reminderFlaggedSelected"
        (ToBeReminded _, _, _, False) -> attrName "list" <> attrName "reminderFlaggedItem"
        (ReminderToBeRemoved, _, _, True) -> attrName "list" <> attrName "reminderRemovalSelected"
        (ReminderToBeRemoved, _, _, False) -> attrName "list" <> attrName "reminderRemovalItem"
        (None, True, _, True) -> attrName "list" <> attrName "reminderSelected"
        (None, True, _, False) -> attrName "list" <> attrName "reminderItem"
        (None, False, True, True) -> attrName "list" <> attrName "favoriteSelected"
        (None, False, True, False) -> attrName "list" <> attrName "favoriteItem"
        (None, False, False, True) -> attrName "list" <> attrName "listSelected"
        (None, False, False, False) -> attrName "list" <> attrName "unselectedItem"
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

reminderBlue :: Vty.Color
reminderBlue = Vty.rgbColor (100 :: Int) (150 :: Int) (200 :: Int)

favoriteYellow :: Vty.Color
favoriteYellow = Vty.rgbColor (200 :: Int) (180 :: Int) (100 :: Int)

whiteFg :: Vty.Attr
whiteFg = Vty.defAttr `Vty.withForeColor` Vty.white

flaggedRedFg :: Vty.Attr
flaggedRedFg = Vty.defAttr `Vty.withForeColor` flaggedRed

flaggedRedSelectedFg :: Vty.Attr
flaggedRedSelectedFg = boldBlackOnOrange

reminderBlueFg :: Vty.Attr
reminderBlueFg = Vty.defAttr `Vty.withForeColor` reminderBlue

reminderBlueSelectedFg :: Vty.Attr
reminderBlueSelectedFg = boldBlackOnOrange

reminderFlaggedFg :: Vty.Attr
reminderFlaggedFg = Vty.defAttr `Vty.withForeColor` reminderBlue

reminderFlaggedSelectedFg :: Vty.Attr
reminderFlaggedSelectedFg = boldBlackOnOrange

reminderRemovalFg :: Vty.Attr
reminderRemovalFg = Vty.defAttr `Vty.withForeColor` Vty.red

reminderRemovalSelectedFg :: Vty.Attr
reminderRemovalSelectedFg = boldBlackOnOrange

favoriteYellowFg :: Vty.Attr
favoriteYellowFg = Vty.defAttr `Vty.withForeColor` favoriteYellow

favoriteYellowSelectedFg :: Vty.Attr
favoriteYellowSelectedFg = boldBlackOnOrange

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

performSetReminders :: BookmarkCredentials -> [(BookmarkItem, UTCTime)] -> IO (Either HttpException [(BookmarkItem, Bool)])
performSetReminders cred itemsWithTimes = do
  tryHttpException $ runStdoutLoggingT $ do
    traverse
      ( \(item, reminderTime) -> do
          success <- raindrop cred (SetReminder (view biId item) reminderTime)
          pure (item, success)
      )
      itemsWithTimes

performRemoveReminders :: BookmarkCredentials -> [BookmarkItem] -> IO (Either HttpException [(BookmarkItem, Bool)])
performRemoveReminders cred items = do
  tryHttpException $ runStdoutLoggingT $ do
    traverse
      ( \item -> do
          success <- raindrop cred (RemoveReminder (view biId item))
          pure (item, success)
      )
      items

tryHttpException :: IO a -> IO (Either HttpException a)
tryHttpException = try @HttpException

txtDisplay :: BookmarkItem -> Widget Name
txtDisplay bit =
  txt (T.justifyRight 10 ' ' leftEdge)
    <+> txt favoriteIndicator
    <+> txt
      ( cleanUnicodeText $
          fromMaybe
            "<empty>"
            (find (not . T.null) [view biTitle bit, T.pack url])
      )
    <+> padLeft Max (hLimit horizontalUriLimit (txt trimmedUrl))
  where
    url = T.unpack (view biLink bit)
    added = view biCreated bit
    reminderDate = view biReminder bit
    favoriteIndicator = if view biImportant bit then "★" else " "
    leftEdge = case reminderDate of
      Just reminder -> T.pack (formatTime defaultTimeLocale "%Y-%m-%d" reminder <> ":")
      Nothing -> T.pack (formatTime defaultTimeLocale "%Y-%m-%d" added <> ":")
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

-- Helper functions to check action types without caring about parameters
isToBeReminded :: PendingAction -> Bool
isToBeReminded (ToBeReminded _) = True
isToBeReminded _ = False

isReminderAction :: PendingAction -> Bool
isReminderAction (ToBeReminded _) = True
isReminderAction ReminderToBeRemoved = True
isReminderAction _ = False

getItemsWithPendingAction :: PendingAction -> HocketState -> [BookmarkItem]
getItemsWithPendingAction targetAction s =
  [item | (action, item) <- Map.elems (s ^. hsContents), matchesAction action targetAction]
  where
    matchesAction (ToBeReminded _) (ToBeReminded _) = True
    matchesAction a b = a == b

getItemsToBeReminded :: HocketState -> [(BookmarkItem, UTCTime)]
getItemsToBeReminded s =
  [(item, time) | (ToBeReminded time, item) <- Map.elems (s ^. hsContents)]

clearToBeRemindedFlags :: [BookmarkItemId] -> HocketState -> HocketState
clearToBeRemindedFlags bids s =
  foldl'
    ( \st bid ->
        case st ^. hsContents . at bid of
          Just (action, item) | isToBeReminded action -> st & hsContents . at bid ?~ (None, item)
          _ -> st
    )
    s
    bids

-- Find next flagged item
findNextFlaggedItem :: HocketState -> Maybe Int
findNextFlaggedItem s = do
  list <- focusedList s
  currentIdx <- view L.listSelectedL list
  let items = V.toList $ view L.listElementsL list
      remainingItems = drop (currentIdx + 1) items
  case findIndex (\item -> let action = getPendingActionForItem (view biId item) s in action == ToBeArchived || isReminderAction action) remainingItems of
    Just relativeIdx -> Just (currentIdx + 1 + relativeIdx)
    Nothing -> Nothing

-- Find previous flagged item
findPrevFlaggedItem :: HocketState -> Maybe Int
findPrevFlaggedItem s = do
  list <- focusedList s
  currentIdx <- view L.listSelectedL list
  let items = V.toList $ view L.listElementsL list
      precedingItems = reverse $ take currentIdx items
  case findIndex (\item -> let action = getPendingActionForItem (view biId item) s in action == ToBeArchived || isReminderAction action) precedingItems of
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
