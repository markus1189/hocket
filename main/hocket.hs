{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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

import AgentClient (runAgentCommand)
import AgentServer (AgentEnv (..), resolveAgentSocketPath, runAgentServer)
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
import Brick.BChan (BChan, newBChan, writeBChan, writeBChanNonBlocking)
import qualified Brick.Focus as Focus
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.List (handleListEvent, handleListEventVi)
import qualified Brick.Widgets.List as L
import Control.Applicative ((<|>))
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO)
import Control.Exception (SomeException, finally)
import Control.Exception.Base (try)
import Control.Lens (at, makeLensesFor, view)
import Control.Lens.Combinators (use)
import Control.Lens.Operators
  ( (%=),
    (&),
    (.=),
    (.~),
    (?~),
    (^.),
    (^?),
  )
import Control.Monad (mfilter, unless, void, when)
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logErrorN, logInfoN, runStdoutLoggingT)
import Control.Monad.Loops (unfoldrM)
import qualified Data.CaseInsensitive as CI
import Data.Foldable (for_)
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import Data.List (find, findIndex, isPrefixOf)
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
import Data.Traversable (for)
import qualified Data.Vector as V
import Dhall (auto, input)
import Events
  ( AsyncCommand (..),
    FilterInput (..),
    HocketEvent (..),
    UiCommand (..),
    archivedItemsEvt,
    asyncActionFailedEvt,
    browseItemEvt,
    cancelFilterEvt,
    clearAllFlagsEvt,
    copyUrlEvt,
    editItemInBrowserEvt,
    executeBatchDoneEvt,
    executeBatchEvt,
    fetchItemsEvt,
    fetchedItemsEvt,
    filterBackspaceEvt,
    filterCharEvt,
    lockFilterEvt,
    remindersRemovedEvt,
    remindersSetEvt,
    setAgentErrorEvt,
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
import Graphics.Vty (Event (EvKey), Key (KChar, KDown, KUp))
import qualified Graphics.Vty as Vty
import Graphics.Vty.Input.Events (Key (KBS, KEnter, KEsc))
import Graphics.Vty.Platform.Unix (mkVty)
import Network.Bookmark.Agent.Protocol
  ( AgentCmd (..),
    FlagAction (..),
    ReadCmd (..),
    WriteCmd (..),
  )
import Network.Bookmark.Agent.Snapshot (AgentSnapshot, asVersion, emptySnapshot, takeSnapshot)
import Network.Bookmark.Types
  ( BookmarkCredentials,
    BookmarkItem,
    BookmarkItemBatch (..),
    BookmarkItemId (..),
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
  ( AsyncOp (..),
    BatchStep (..),
    HocketState,
    Name (..),
    VideoFilterMode (..),
    appendFilterChar,
    backspaceFilter,
    batchStepsWithWork,
    cancelFilter,
    clearAllFlags,
    clearFlagsForItems,
    completeAsyncOp,
    enterFilterMode,
    focusRing,
    hsAgentClients,
    hsAgentError,
    hsContents,
    hsCredentials,
    hsFilterActive,
    hsFilterQuery,
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
    lockFilter,
    removeItems,
    removeReminderFromItems,
    setAgentClients,
    setAgentError,
    setAllFlagsToArchive,
    setFilterQuery,
    setPendingAction,
    setShowFutureReminders,
    setVideoFilterMode,
    syncForRender,
    toggleInvertedVideoFilter,
    togglePendingAction,
    togglePendingActionToReminder,
    toggleShowFutureReminders,
    toggleVideoFilter,
    tryAcquireAsyncOp,
    updateItemsWithStoredReminderTimes,
  )
import Network.Bookmark.Ui.Widgets (sanitizeForDisplay)
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
    eitherReader,
    execParser,
    flag,
    flag',
    fullDesc,
    header,
    help,
    helper,
    hsubparser,
    info,
    long,
    many,
    metavar,
    option,
    optional,
    progDesc,
    showDefault,
    str,
    strOption,
    switch,
    value,
    (<**>),
  )
import qualified Options.Applicative as Opt
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory, removeFile, removePathForcibly)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, hPutStr, hPutStrLn, stderr)
import System.Process
  ( CreateProcess,
    createProcess,
    proc,
    shell,
    waitForProcess,
  )
import System.Process.Internals (StdStream (CreatePipe))
import Text.Printf (printf)

makeLensesFor [("std_in", "stdIn"), ("std_err", "stdErr"), ("std_out", "stdOut")] ''CreateProcess

data HocketCommand
  = RunTUI !(Maybe AgentSocketOpt)
  | AddBookmarkCmd !Text !(Maybe Text) ![Text]
  | -- | Speak one request to a running TUI's control socket and exit.
    AgentCall !(Maybe FilePath) !AgentCmd
  deriving (Show, Eq)

-- | Where to bind the agent control socket, when enabled at all.
data AgentSocketOpt = AgentSocketDefault | AgentSocketAt !FilePath
  deriving (Show, Eq)

tuiCommandParser :: Mod CommandFields HocketCommand
tuiCommandParser =
  command "tui" (info (RunTUI <$> agentSocketParser) (progDesc "Run the Hocket Terminal User Interface"))

agentSocketParser :: Parser (Maybe AgentSocketOpt)
agentSocketParser =
  ( Just . AgentSocketAt
      <$> strOption
        ( long "agent-socket-path"
            <> metavar "PATH"
            <> help "Enable the agent control socket at PATH"
        )
  )
    <|> flag
      Nothing
      (Just AgentSocketDefault)
      ( long "agent-socket"
          <> help "Enable the agent control socket at $XDG_RUNTIME_DIR/hocket/control.sock"
      )

addCommandParser :: Mod CommandFields HocketCommand
addCommandParser =
  command "add" (info addBookmarkParser (progDesc "Add a bookmark to Raindrop"))
  where
    addBookmarkParser =
      AddBookmarkCmd
        <$> argument str (metavar "URL" <> help "URL to bookmark")
        <*> optional (strOption (long "collection" <> help "Collection ID (defaults to -1 for unsorted)"))
        <*> many (strOption (long "tag" <> help "Tags to add"))

agentCommandParser :: Mod CommandFields HocketCommand
agentCommandParser =
  command
    "agent"
    ( info
        (AgentCall <$> socketPathParser <*> hsubparser agentMethods)
        (progDesc "Send one control-socket request to a running TUI and print the reply")
    )
  where
    socketPathParser =
      optional
        ( strOption
            ( long "socket-path"
                <> metavar "PATH"
                <> help "Control socket to talk to (default: $XDG_RUNTIME_DIR/hocket/control.sock)"
            )
        )

-- | One sub-subcommand per protocol method, named exactly as the method is
-- named on the wire, so anything read in docs/RPC.md can be typed verbatim.
-- Each parser yields an 'AgentCmd', which makes an ill-formed request
-- unrepresentable rather than a runtime "unknown method".
agentMethods :: Mod CommandFields AgentCmd
agentMethods =
  mconcat
    [ meth "get_state" "Header summary and items, from the snapshot mirror" (pure (ARead CmdGetState)),
      meth "list_items" "List items (visible ones only, unless --all)" listItemsP,
      meth "get_item" "Fetch one item by id (hidden items included)" (ARead . CmdGetItem <$> itemIdArg),
      meth "wait_version" "Long-poll until the state version passes --after" waitP,
      meth "set_flag" "Stage a pending action on one item" setFlagP,
      meth "clear_all_flags" "Drop every staged action (the 'u' key)" (pure (AWrite CmdClearFlags)),
      meth "flag_all_archive" "Stage archive on every item" (pure (AWrite CmdFlagAllArchive)),
      meth "execute" "Execute the staged batch (the 'X' key)" (pure (AWrite CmdExecute)),
      meth "refresh" "Resync from Raindrop (the 'r' key)" (pure (AWrite CmdRefresh)),
      meth "set_filter" "Replace the live filter query" setFilterP,
      meth "set_video_filter" "Set the video filter" (AWrite . CmdSetVideoFilter <$> videoModeArg),
      meth "set_show_future_reminders" "Show or hide future reminders" showFutureP,
      meth "select_item" "Move the TUI selection (item must be visible)" (AWrite . CmdSelectItem <$> itemIdArg),
      meth "open_item" "Open an item in the browser" (AWrite . CmdOpenItem <$> itemIdArg),
      meth "set_status" "Write 'agent: TEXT' to the shared status line" setStatusP
    ]
  where
    meth n d p = command n (info p (progDesc d))
    itemIdArg = BookmarkItemId <$> argument str (metavar "ID")
    listItemsP =
      fmap ARead $
        CmdListItems . not
          <$> switch (long "all" <> help "Include items hidden by the current filters")
          <*> switch (long "flagged-only" <> help "Only items with a staged action")
    waitP =
      AWait
        <$> option Opt.auto (long "after" <> metavar "N" <> help "Return once the version exceeds N")
        <*> option
          Opt.auto
          ( long "timeout-ms"
              <> metavar "MS"
              <> value 10000
              <> showDefault
              <> help "Server-side timeout, clamped to [0, 60000]"
          )
    setFlagP =
      fmap AWrite $
        CmdSetFlag
          <$> itemIdArg
          <*> option
            (eitherReader flagActionReader)
            ( long "action"
                <> metavar "ACTION"
                <> help "archive | reminder | remove_reminder | none"
            )
    setFilterP = AWrite . CmdSetFilter <$> argument str (metavar "QUERY")
    setStatusP = AWrite . CmdSetStatus <$> argument str (metavar "TEXT")
    showFutureP =
      AWrite . CmdSetShowFutureReminders
        <$> ( flag' True (long "show" <> help "Show future reminders")
                <|> flag' False (long "hide" <> help "Hide future reminders")
            )
    videoModeArg =
      argument
        (eitherReader videoModeReader)
        (metavar "MODE" <> help "none | only_videos | hide_videos")

flagActionReader :: String -> Either String FlagAction
flagActionReader = \case
  "archive" -> Right FlagArchive
  "reminder" -> Right FlagReminder
  "remove_reminder" -> Right FlagRemoveReminder
  "none" -> Right FlagNone
  other -> Left ("unknown flag action: " <> other <> " (want archive | reminder | remove_reminder | none)")

videoModeReader :: String -> Either String VideoFilterMode
videoModeReader = \case
  "none" -> Right NoVideoFilter
  "only_videos" -> Right ShowOnlyVideos
  "hide_videos" -> Right HideVideos
  other -> Left ("unknown video filter mode: " <> other <> " (want none | only_videos | hide_videos)")

hocketCommandParser :: Parser HocketCommand
hocketCommandParser = hsubparser (tuiCommandParser <> addCommandParser <> agentCommandParser)

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
vtyEventHandler es e = do
  s <- use id
  if s ^. hsFilterActive
    then case e of
      EvKey KEsc [] -> liftIO $ es `trigger` cancelFilterEvt
      EvKey KEnter [] -> liftIO $ es `trigger` lockFilterEvt
      EvKey KBS [] -> liftIO $ es `trigger` filterBackspaceEvt
      EvKey (KChar c) [] -> liftIO $ es `trigger` filterCharEvt c
      EvKey KUp [] -> zoom itemList (handleListEventVi handleListEvent e)
      EvKey KDown [] -> zoom itemList (handleListEventVi handleListEvent e)
      _ -> pure ()
    else vtyEventHandlerNormal es e

vtyEventHandlerNormal ::
  BChan HocketEvent ->
  Event ->
  EventM Name HocketState ()
vtyEventHandlerNormal es (EvKey (KChar ' ') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit -> es `trigger` browseItemEvt bit
vtyEventHandlerNormal es (EvKey KEnter []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit -> do
    es `trigger` browseItemEvt bit
    es `trigger` shiftItemEvt (view biId bit)
vtyEventHandlerNormal es (EvKey (KChar 'r') []) = do
  liftIO $ es `trigger` fetchItemsEvt
  pure ()
vtyEventHandlerNormal es (EvKey (KChar 'X') []) = do
  liftIO $ es `trigger` executeBatchEvt
  pure ()
vtyEventHandlerNormal es (EvKey (KChar 'a') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit ->
    unless (getPendingActionForItem (view biId bit) s == ToBeArchived) $
      es `trigger` shiftItemEvt (view biId bit)
vtyEventHandlerNormal es (EvKey (KChar 's') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit ->
    unless (isToBeReminded (getPendingActionForItem (view biId bit) s)) $
      es `trigger` shiftItemReminderEvt (view biId bit)
vtyEventHandlerNormal es (EvKey (KChar 'u') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit -> do
    let action = getPendingActionForItem (view biId bit) s
    when (action == ToBeArchived) $
      es `trigger` shiftItemEvt (view biId bit)
    when (isReminderAction action) $
      es `trigger` shiftItemReminderEvt (view biId bit)
vtyEventHandlerNormal _ (EvKey (KChar 'J') []) = do
  s <- use id
  case findNextFlaggedItem s of
    Just newIdx -> itemList %= L.listMoveTo newIdx
    Nothing -> pure ()
vtyEventHandlerNormal _ (EvKey (KChar 'K') []) = do
  s <- use id
  case findPrevFlaggedItem s of
    Just newIdx -> itemList %= L.listMoveTo newIdx
    Nothing -> pure ()
vtyEventHandlerNormal es (EvKey (KChar 'U') []) = do
  liftIO $ es `trigger` clearAllFlagsEvt
  pure ()
vtyEventHandlerNormal es (EvKey (KChar 'S') []) = do
  liftIO $ es `trigger` toggleRemindersEvt
  pure ()
vtyEventHandlerNormal es (EvKey (KChar 'v') []) = do
  liftIO $ es `trigger` toggleVideoFilterEvt
  pure ()
vtyEventHandlerNormal es (EvKey (KChar 'V') []) = do
  liftIO $ es `trigger` toggleInvertedVideoFilterEvt
  pure ()
vtyEventHandlerNormal es (EvKey (KChar 'e') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit -> es `trigger` editItemInBrowserEvt bit
vtyEventHandlerNormal es (EvKey (KChar 'y') []) = do
  s <- use id
  liftIO . for_ (focusedItem s) $ \bit -> es `trigger` copyUrlEvt bit
vtyEventHandlerNormal _ (EvKey (KChar '/') []) =
  -- Flip into filter mode synchronously so the very next keystroke is already
  -- seen under the editing guard; routing this through the BChan would leave a
  -- one-event window where a fast 'q'/paste leaks to normal mode and quits.
  id %= enterFilterMode
vtyEventHandlerNormal _ (EvKey (KChar 'q') []) = halt
vtyEventHandlerNormal _ e = do
  zoom itemList (handleListEventVi handleListEvent e)

internalEventHandler ::
  BChan HocketEvent ->
  HocketEvent ->
  EventM Name HocketState ()
internalEventHandler es (HocketAsync e) = asyncCommandEventHandler es e
internalEventHandler es (HocketUi e) = uiCommandEventHandler es e

-- | Run an async-op body only when the single slot is free; otherwise drop the
-- op (matching the old 'unlessAsyncRunning' drop-on-busy semantics).
withAsyncSlot :: AsyncOp -> EventM Name HocketState () -> EventM Name HocketState ()
withAsyncSlot op run = do
  s <- use id
  case tryAcquireAsyncOp op s of
    Nothing -> pure ()
    Just s' -> do
      id .= s'
      run

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

-- The three 'X' sub-op bodies, run sequentially by 'runBatchOp'.
runArchiveOp :: BChan HocketEvent -> HocketState -> IO ()
runArchiveOp es s = do
  es `trigger` setStatusEvt (Just "archiving")
  eitherErrorResults <-
    performArchive (s ^. hsCredentials) (getItemsWithPendingAction ToBeArchived s)
  case eitherErrorResults of
    Left e -> es `trigger` asyncActionFailedEvt (errorMessageFromException e)
    Right results -> do
      es
        `trigger` archivedItemsEvt
          ( mapMaybe
              (\(bit, successful) -> mfilter (const successful) (Just (view biId bit)))
              results
          )
      es `trigger` setStatusEvt Nothing

runSetRemindersOp :: BChan HocketEvent -> HocketState -> IO ()
runSetRemindersOp es s = do
  es `trigger` setStatusEvt (Just "setting reminders")
  eitherErrorResults <- performSetReminders (s ^. hsCredentials) (getItemsToBeReminded s)
  case eitherErrorResults of
    Left e -> es `trigger` asyncActionFailedEvt (errorMessageFromException e)
    Right results -> do
      es
        `trigger` remindersSetEvt
          ( mapMaybe
              (\(bit, successful) -> mfilter (const successful) (Just (view biId bit)))
              results
          )
      es `trigger` setStatusEvt Nothing

runRemoveRemindersOp :: BChan HocketEvent -> HocketState -> IO ()
runRemoveRemindersOp es s = do
  es `trigger` setStatusEvt (Just "removing reminders")
  eitherErrorResults <- performRemoveReminders (s ^. hsCredentials) (getItemsWithPendingAction ReminderToBeRemoved s)
  case eitherErrorResults of
    Left e -> es `trigger` asyncActionFailedEvt (errorMessageFromException e)
    Right results -> do
      es
        `trigger` remindersRemovedEvt
          ( mapMaybe
              (\(bit, successful) -> mfilter (const successful) (Just (view biId bit)))
              results
          )
      es `trigger` setStatusEvt Nothing

-- Run every bucket that has pending work, in order, in one async thread. The
-- slot is held for the whole batch and freed exactly once via
-- 'executeBatchDoneEvt', guarded by 'finally' so a sub-op failure can't leak it.
runBatchOp :: BChan HocketEvent -> HocketState -> IO ()
runBatchOp es s =
  for_
    (batchStepsWithWork s)
    ( \case
        StepArchive -> runArchiveOp es s
        StepSetReminders -> runSetRemindersOp es s
        StepRemoveReminders -> runRemoveRemindersOp es s
    )
    `finally` (es `trigger` executeBatchDoneEvt)

asyncCommandEventHandler ::
  BChan HocketEvent ->
  AsyncCommand ->
  EventM Name HocketState ()
asyncCommandEventHandler es FetchItems =
  withAsyncSlot OpFetchItems $ do
    s <- use id
    _ <- liftIO . async $ do
      let searchParam = case s ^. hsLastUpdated of
            Nothing -> Nothing
            Just lastTime -> Just ("lastUpdate:>" <> formatPOSIXTime (lastTime - 86400))
          isUpdateFetch = isJust (s ^. hsLastUpdated)
          collectionToFetch =
            if isUpdateFetch
              then RaindropCollectionId "0"
              else RaindropCollectionId "-1"
          suffix = maybe "" (\since -> " since: " <> formatPOSIXTime (since - 86400)) $ s ^. hsLastUpdated
      es `trigger` setStatusEvt (Just ("fetching" <> suffix))
      eitherErrorBis <- retrieveItems (s ^. hsCredentials) searchParam collectionToFetch
      case eitherErrorBis of
        Left e -> es `trigger` asyncActionFailedEvt (errorMessageFromException e)
        Right batches -> do
          es `trigger` setStatusEvt Nothing
          for_ batches $ \(BookmarkItemBatch ts bis _) -> do
            es `trigger` fetchedItemsEvt ts bis isUpdateFetch
    pure ()
asyncCommandEventHandler _ (FetchedItems ts bis wasAllCollectionsFetch) = do
  if wasAllCollectionsFetch
    then do
      let itemsToPotentiallyAdd = filter (\item -> item ^. biCollectionId == -1) bis
          itemIdsToRemove = map (view biId) $ filter (\item -> item ^. biCollectionId /= -1) bis
      unless (null itemIdsToRemove) $
        id %= removeItems itemIdsToRemove
      id %= insertItems itemsToPotentiallyAdd
    else id %= insertItems bis

  id %= completeAsyncOp
  currentLastUpdated <- use hsLastUpdated
  let newTimestampToConsider = if null bis then Nothing else Just ts
  case (currentLastUpdated, newTimestampToConsider) of
    (Nothing, Just newTs) -> hsLastUpdated .= Just newTs
    (Just oldTs, Just newTs) -> when (newTs >= oldTs) $ hsLastUpdated .= Just newTs
    _ -> pure ()
asyncCommandEventHandler es (AsyncActionFailed err) = do
  id %= completeAsyncOp
  liftIO (es `trigger` setStatusEvt (Just ("failed" <> maybe "<no err>" (": " <>) err)))
asyncCommandEventHandler es ExecuteBatch = do
  s <- use id
  case batchStepsWithWork s of
    [] -> pure ()
    _ -> withAsyncSlot OpExecuteBatch $ do
      _ <- liftIO $ async $ runBatchOp es s
      pure ()
asyncCommandEventHandler _ ExecuteBatchDone = do
  id %= completeAsyncOp
asyncCommandEventHandler _ (ArchivedItems bis) = do
  id %= removeItems bis
asyncCommandEventHandler _ (RemindersSet bis) = do
  id %= updateItemsWithStoredReminderTimes bis
  id %= clearToBeRemindedFlags bis
asyncCommandEventHandler _ (RemindersRemoved bis) = do
  id %= removeReminderFromItems bis
  id %= clearFlagsForItems ReminderToBeRemoved bis

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
uiCommandEventHandler _ (FilterInput fi) =
  id %= case fi of
    EnterFilter -> enterFilterMode
    LockFilter -> lockFilter
    DoCancelFilter -> cancelFilter
    FilterChar c -> appendFilterChar c
    FilterBackspace -> backspaceFilter
uiCommandEventHandler _ (SetPendingAction bid act) = id %= setPendingAction bid act
uiCommandEventHandler _ (SetFilterQuery q) = do
  id %= setFilterQuery q
  id %= syncForRender
uiCommandEventHandler _ (SetVideoFilterMode m) = do
  id %= setVideoFilterMode m
  id %= syncForRender
uiCommandEventHandler _ (SetShowFutureReminders b) = do
  id %= setShowFutureReminders b
  id %= syncForRender
uiCommandEventHandler _ (SelectItem bid) = do
  s <- use id
  let mIdx = V.findIndex (\bit -> view biId bit == bid) (s ^. itemList . L.listElementsL)
  for_ mIdx $ \i -> itemList %= L.listMoveTo i
uiCommandEventHandler es (OpenItemById bid) = do
  s <- use id
  liftIO . for_ (s ^. hsContents . at bid) $ \(_, bit) -> es `trigger` browseItemEvt bit
uiCommandEventHandler _ (SetAgentClients n) = id %= setAgentClients n
uiCommandEventHandler _ (SetAgentError e) = id %= setAgentError e
uiCommandEventHandler es (BrowseItem bit) = do
  res <- liftIO . try @SomeException $ browseItem "firefox '%s'" (URL . T.unpack $ view biLink bit)
  case res of
    Left e -> liftIO $ es `trigger` setStatusEvt (Just (T.pack $ show e))
    Right () -> pure ()
uiCommandEventHandler es (CopyUrl bit) = do
  let url = cleanUrl (T.unpack (view biLink bit))
  eitherClipError <- liftIO . try @SomeException $ copyToClipboard url
  case eitherClipError of
    Right () -> liftIO $ es `trigger` setStatusEvt (Just ("Copied: " <> T.pack url))
    Left e -> liftIO $ es `trigger` setStatusEvt (Just ("Copy failed: " <> T.pack (show e)))
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

runTuiApp :: Maybe AgentSocketOpt -> IO ()
runTuiApp mAgentOpt = do
  ensureSchemaFile
  configPath <- getConfigPath
  cred <- input auto (T.pack configPath)
  events <- newBChan 10
  tz <- getCurrentTimeZone
  mAgent <- for mAgentOpt (startAgentServer events)
  vty <- mkVty Vty.defaultConfig
  void
    ( customMain
        vty
        (mkVty Vty.defaultConfig)
        (Just events)
        (app tz events (snd <$> mAgent))
        (initialState cred)
    )
    `finally` cleanupAgentSocket mAgent

-- | On graceful exit, remove the socket file and then the (now-empty)
-- containing directory so we do not accumulate stale shells. Wrapped in
-- 'try' so a failed cleanup can never change the exit outcome. All removal
-- here is best-effort: unlike the collision path in 'runAgentServer', this
-- runs after our own socket is gone, so there is no live-socket danger.
cleanupAgentSocket :: Maybe (FilePath, TVar AgentSnapshot) -> IO ()
cleanupAgentSocket mAgent =
  for_ mAgent $ \(path, _) ->
    void . try @SomeException $ do
      removeFile path
      removePathForcibly (takeDirectory path)

-- | Bind the agent control socket and keep serving it in the background.
-- Server failures surface in the TUI status bar instead of killing the app.
startAgentServer :: BChan HocketEvent -> AgentSocketOpt -> IO (FilePath, TVar AgentSnapshot)
startAgentServer events opt = do
  path <- case opt of
    AgentSocketDefault -> resolveAgentSocketPath
    AgentSocketAt p -> pure p
  snapVar <- newTVarIO emptySnapshot
  let env =
        AgentEnv
          { aeSnapshot = snapVar,
            aeInject = writeBChanNonBlocking events,
            aeReminderTime = nextDayAt7AM
          }
  _ <- async $ do
    r <- try @SomeException (runAgentServer path env)
    case r of
      Left e -> do
        -- The status line is single-lane and gets overwritten by the very
        -- next status write (the startup fetch, typically within the same
        -- second), so also latch the failure into the header.
        void
          ( writeBChanNonBlocking
              events
              (setStatusEvt (Just ("agent socket failed: " <> T.pack (show e))))
          )
        void (writeBChanNonBlocking events (setAgentErrorEvt (Just (T.pack (show e)))))
      Right () -> pure ()
  pure (path, snapVar)

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
    RunTUI mAgentOpt -> runTuiApp mAgentOpt
    AddBookmarkCmd url mCollection tags -> runAddCommand url mCollection tags
    AgentCall mpath agentCmd -> runAgentCommand mpath agentCmd

app :: TimeZone -> BChan HocketEvent -> Maybe (TVar AgentSnapshot) -> App HocketState HocketEvent Name
app tz events mSnapVar =
  App
    { appDraw = drawGui tz,
      appChooseCursor = Focus.focusRingCursor (view focusRing),
      appHandleEvent = \e -> do
        myEventHandler events e
        id %= syncForRender
        -- Mirror the post-render state for the agent socket. Persistent
        -- data sharing keeps this cheap; the item projection is only fully
        -- forced by a server thread that actually serves it.
        for_ mSnapVar $ \snapVar -> do
          s <- use id
          liftIO . atomically . modifyTVar' snapVar $ \old ->
            takeSnapshot (asVersion old + 1) s,
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

getDisplayContent :: BookmarkItem -> Text
getDisplayContent item =
  let noteText = item ^. biNote
      excerptText = item ^. biExcerpt
      reminderDate = item ^. biReminder
      hasNote = not (T.null noteText)
      hasExcerpt = not (T.null excerptText)
      hasReminder = isJust reminderDate
      formattedNote = if hasNote then "NOTE " <> sanitizeForDisplay noteText else T.empty
      formattedReminder = case reminderDate of
        Just reminder -> "REMINDER " <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d" reminder) <> " "
        Nothing -> T.empty
      formattedExcerpt = if hasExcerpt then "EXCERPT " <> sanitizeForDisplay excerptText else T.empty
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
            ( ( case (s ^. hsAgentError, s ^. hsAgentClients > 0) of
                  (Just _, _) -> "[agent: socket failed] "
                  (Nothing, True) -> "[agent] "
                  (Nothing, False) -> ""
              )
                <> "Hocket"
                <> ( case s ^. hsVideoFilter of
                       NoVideoFilter -> ""
                       ShowOnlyVideos -> " (+V)"
                       HideVideos -> " (-V)"
                   )
                <> ( if T.null (s ^. hsFilterQuery)
                       then ""
                       else " (/" <> sanitizeForDisplay (s ^. hsFilterQuery) <> ")"
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
            "spc:Browse ent:Browse+flag e:Edit y:Copied URL r:Refresh S:Toggle future reminders v:Video filter V:Hide videos /:Filter X:Execute Flags a:Archive flag s:Reminder flag u:Unflag J/K:Jump U:Unflag all q:Quit",
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
          if s ^. hsFilterActive
            then hBar (sanitizeForDisplay ("/" <> s ^. hsFilterQuery <> "_"))
            else txt (fromMaybe " " (s ^. hsStatus))
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
      ( sanitizeForDisplay $
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

-- | Copy text to the system clipboard via the native tool: wl-copy on
-- Wayland, else xclip.
copyToClipboard :: String -> IO ()
copyToClipboard text = do
  wayland <- isJust <$> lookupEnv "WAYLAND_DISPLAY"
  if wayland
    then runClipboardCmd "wl-copy" [] text
    else runClipboardCmd "xclip" ["-selection", "clipboard"] text

-- | Spawn a tool, feed @text@ on stdin, close it for EOF, and wait; fail on
-- a nonzero exit.
runClipboardCmd :: String -> [String] -> String -> IO ()
runClipboardCmd cmd args text = do
  (mIn, _, _, ph) <- createProcess (proc cmd args & stdIn .~ CreatePipe)
  for_ mIn $ \h -> hPutStr h text >> hClose h
  exitCode <- waitForProcess ph
  unless (exitCode == ExitSuccess) $
    ioError (userError (cmd <> " exited with code " <> show exitCode))

errorMessageFromException :: HttpException -> Maybe Text
errorMessageFromException (HttpExceptionRequest _ (StatusCodeException resp _)) = msg
  where
    msg = xError <|> code
    xError = T.decodeUtf8 . snd <$> find (\(k, _) -> k == CI.mk "x-error") (responseHeaders resp)
    code = Just . T.pack $ "Got status: " <> (show . responseStatus $ resp)
errorMessageFromException e = Just . T.pack $ show e
