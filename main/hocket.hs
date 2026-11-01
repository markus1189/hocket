{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Main
  ( main,
  )
where

import AgentClient (runAgentCommand)
import AgentServer (AgentEnv (..), BoundSocket, cleanupAgentSocket, newBoundSocket, resolveAgentSocketPath, runAgentServer, socketDirIsOurs)
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
    emptyWidget,
    hLimit,
    halt,
    padLeft,
    padRight,
    showCursorNamed,
    txt,
    vBox,
    withAttr,
    zoom,
    (<+>),
  )
import Brick.BChan (BChan, newBChan, writeBChan, writeBChanNonBlocking)
import qualified Brick.Focus as Focus
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Edit (applyEdit, editFocusedAttr, handleEditorEvent, renderEditor)
import Brick.Widgets.List (handleListEvent, handleListEventVi)
import qualified Brick.Widgets.List as L
import Control.Applicative ((<|>))
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO)
import Control.Exception (SomeException, finally)
import Control.Exception.Base (try)
import Control.Lens (at, view)
import Control.Lens.Combinators (use)
import Control.Lens.Operators
  ( (%=),
    (.=),
    (^.),
  )
import Control.Monad (unless, void)
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, defaultOutput, logErrorN, logInfoN, runLoggingT, runStdoutLoggingT)
import Control.Monad.Loops (unfoldrM)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef)
import Signals (unwindOnTermination)
#if MIN_VERSION_base(4,20,0)
import Data.List (find, findIndex, isPrefixOf)
#else
import Data.List (find, findIndex, foldl', isPrefixOf)
#endif
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Zipper.Generic.Words (deletePrevWord)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime
  ( TimeZone,
    getCurrentTimeZone,
    utcToLocalTime,
  )
import Data.Traversable (for)
import qualified Data.Vector as V
import Dhall (auto, input)
import Events
  ( AsyncCommand (..),
    HocketEvent (..),
    UiCommand (..),
    archivedItemsEvt,
    browseItemEvt,
    clearAllFlagsEvt,
    copyUrlEvt,
    editItemInBrowserEvt,
    executeBatchDoneEvt,
    executeBatchEvt,
    fetchItemsEvt,
    remindersRemovedEvt,
    remindersSetEvt,
    setAgentErrorEvt,
    setStatusEvt,
    toggleInvertedVideoFilterEvt,
    toggleRemindersEvt,
    toggleVideoFilterEvt,
  )
import Fixture (FixtureOpts (..), fixtureParser, fixtureStore)
import Formatting (sformat, (%))
import qualified Formatting as F
import qualified Formatting.Time as F
import Graphics.Vty (Event (EvKey), Key (KChar, KDown, KUp), Modifier (MCtrl))
import qualified Graphics.Vty as Vty
import Graphics.Vty.Input.Events (Key (KEnter, KEsc))
import Graphics.Vty.Platform.Unix (mkVty)
import Keys (ItemKey (..), itemKeyEvents)
import Network.Bookmark.Agent.Protocol
  ( AgentCmd (..),
    FlagAction (..),
    ReadCmd (..),
    WriteCmd (..),
  )
-- Qualified while the old event constructors of the same names still exist.

import Network.Bookmark.Agent.Snapshot (AgentSnapshot, asVersion, emptySnapshot, takeSnapshot)
import Network.Bookmark.App.Effect (Effect (..))
import Network.Bookmark.App.Msg (Msg (..))
import qualified Network.Bookmark.App.Msg as I
import Network.Bookmark.App.Update (update)
import Network.Bookmark.Batch (StepOutcome, concatOutcome, soOk)
import Network.Bookmark.Fake (encodeFixtureDump, fakePort)
import Network.Bookmark.Port (RaindropPort (..))
import Network.Bookmark.Reminder (reminderPolicy)
import Network.Bookmark.Types
  ( BookmarkItem,
    BookmarkItemBatch (..),
    BookmarkItemId (..),
    BookmarkRequest (AddBookmark, BatchArchiveBookmarks, RemoveReminder, RetrieveBookmarks, SetReminder),
    PendingAction (..),
    RaindropCollectionId,
    URL (..),
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
    archiveItem,
    batchStepsWithWork,
    cancelFilter,
    clearAllFlags,
    completeAsyncOp,
    enterFilterMode,
    focusRing,
    focusedItem,
    focusedList,
    getPendingActionForItem,
    hsAgentClients,
    hsAgentError,
    hsContents,
    hsFilterActive,
    hsFilterEditor,
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
    isReminderAction,
    itemList,
    lockFilter,
    setAgentClients,
    setAgentError,
    setAllFlagsToArchive,
    setFilterQuery,
    setPendingAction,
    setShowFutureReminders,
    setVideoFilterMode,
    shiftItem,
    shiftItemReminder,
    syncForRender,
    toggleInvertedVideoFilter,
    toggleShowFutureReminders,
    toggleVideoFilter,
    tryAcquireAsyncOp,
  )
import Network.Bookmark.Ui.Sync
  ( SyncKind (..),
    applyArchived,
    applyFetchCompleted,
    applyRemindersRemoved,
    applyRemindersSet,
    formatPOSIXTime,
    planSync,
    spCollection,
    spKind,
    spSearch,
  )
import Network.Bookmark.Ui.Widgets (sanitizeForDisplay)
import Network.Raindrop (servantPort)
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
import Servant.Client (ClientError (..))
import System (browseItem, cleanUrl, copyToClipboard)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), IOMode (AppendMode), hPutStrLn, hSetBuffering, stderr, withFile)
import Worker (errorMessageFromException, exceptionMessage, fetchResultEvents, runBatch, runCalls, runStep, trySync)

data HocketCommand
  = RunTUI !(Maybe AgentSocketOpt) !(Maybe FixtureOpts)
  | AddBookmarkCmd !Text !(Maybe Text) ![Text]
  | -- | Speak one request to a running TUI's control socket and exit.
    AgentCall !(Maybe FilePath) !AgentCmd
  deriving (Show, Eq)

-- | Where to bind the agent control socket, when enabled at all.
data AgentSocketOpt = AgentSocketDefault | AgentSocketAt !FilePath
  deriving (Show, Eq)

tuiCommandParser :: Mod CommandFields HocketCommand
tuiCommandParser =
  command "tui" (info (RunTUI <$> agentSocketParser <*> optional fixtureParser) (progDesc "Run the Hocket Terminal User Interface"))

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
      meth "open_and_flag" "Open in browser AND stage for archive (deterministic set)" (AWrite . CmdOpenAndFlag <$> itemIdArg),
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

-- | What the async handlers need from outside: where to post events, and
-- Raindrop. The keyboard and UI handlers only post, so they take the channel.
data Env = Env {envEvents :: !(BChan HocketEvent), envPort :: !RaindropPort}

trigger :: BChan HocketEvent -> HocketEvent -> IO ()
trigger = writeBChan

vtyEventHandler ::
  Env ->
  Event ->
  EventM Name HocketState ()
vtyEventHandler env e = do
  s <- use id
  if s ^. hsFilterActive
    then case e of
      -- Handled here, not via the BChan, so they stay ordered with the
      -- keystrokes the editor already applied synchronously.
      EvKey KEsc [] -> id %= cancelFilter
      EvKey KEnter [] -> id %= lockFilter
      EvKey KUp [] -> zoom itemList (handleListEventVi handleListEvent e)
      EvKey KDown [] -> zoom itemList (handleListEventVi handleListEvent e)
      -- Brick's editor has no Ctrl-W.
      EvKey (KChar 'w') [MCtrl] -> hsFilterEditor %= applyEdit deletePrevWord
      _ -> zoom hsFilterEditor (handleEditorEvent (VtyEvent e))
    else case e of
      EvKey (KChar 'Q') [] -> applyMsg env (Intent I.Quit)
      _ -> vtyEventHandlerNormal (envEvents env) e

-- | Run one message through 'update' and its effects through the shell.
applyMsg :: Env -> Msg -> EventM Name HocketState ()
applyMsg _ m = do
  s <- use id
  for_ (update m s) $ \(s', effects) -> do
    id .= s'
    for_ effects $ \case
      Halt -> halt

vtyEventHandlerNormal ::
  BChan HocketEvent ->
  Event ->
  EventM Name HocketState ()
vtyEventHandlerNormal es (EvKey (KChar ' ') []) = do
  s <- use id
  liftIO $ for_ (itemKeyEvents KeySpace s) (trigger es)
vtyEventHandlerNormal es (EvKey KEnter []) = do
  s <- use id
  liftIO $ for_ (itemKeyEvents KeyEnter s) (trigger es)
vtyEventHandlerNormal es (EvKey (KChar 'r') []) = do
  liftIO $ es `trigger` fetchItemsEvt
  pure ()
vtyEventHandlerNormal es (EvKey (KChar 'X') []) = do
  liftIO $ es `trigger` executeBatchEvt
  pure ()
vtyEventHandlerNormal es (EvKey (KChar 'a') []) = do
  s <- use id
  liftIO $ for_ (itemKeyEvents KeyArchive s) (trigger es)
vtyEventHandlerNormal es (EvKey (KChar 's') []) = do
  s <- use id
  liftIO $ for_ (itemKeyEvents KeyRemind s) (trigger es)
vtyEventHandlerNormal es (EvKey (KChar 'u') []) = do
  s <- use id
  liftIO $ for_ (itemKeyEvents KeyUnflag s) (trigger es)
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
  -- one-event window where a fast 'Q'/paste leaks to normal mode and quits.
  id %= enterFilterMode
vtyEventHandlerNormal _ e = do
  zoom itemList (handleListEventVi handleListEvent e)

internalEventHandler ::
  Env ->
  HocketEvent ->
  EventM Name HocketState ()
internalEventHandler env (HocketAsync e) = asyncCommandEventHandler env e
internalEventHandler env (HocketUi e) = uiCommandEventHandler (envEvents env) e
internalEventHandler env (HocketMsg m) = applyMsg env m

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

nextDayAt7AM :: IO UTCTime
nextDayAt7AM = reminderPolicy <$> getCurrentTimeZone <*> getCurrentTime

-- The three 'X' sub-op bodies, run sequentially by 'runBatchOp'.
runArchiveOp :: Env -> HocketState -> IO (StepOutcome () Text)
runArchiveOp env s =
  runStep (trigger (envEvents env)) "archiving" archivedItemsEvt $
    performArchive (envPort env) (getItemsWithPendingAction ToBeArchived s)

runSetRemindersOp :: Env -> HocketState -> IO (StepOutcome () Text)
runSetRemindersOp env s =
  runStep (trigger (envEvents env)) "setting reminders" remindersSetEvt $
    first (first (view biId)) <$> performSetReminders (envPort env) (getItemsToBeReminded s)

runRemoveRemindersOp :: Env -> HocketState -> IO (StepOutcome () Text)
runRemoveRemindersOp env s =
  runStep (trigger (envEvents env)) "removing reminders" remindersRemovedEvt $
    first (view biId) <$> performRemoveReminders (envPort env) (getItemsWithPendingAction ReminderToBeRemoved s)

-- Run every bucket that has pending work, in order, in one async thread. The
-- slot is held for the whole batch and freed exactly once via
-- 'executeBatchDoneEvt', guarded by 'finally' so a sub-op failure can't leak it.
runBatchOp :: Env -> HocketState -> IO ()
runBatchOp env s =
  runBatch
    (trigger (envEvents env))
    ( \case
        StepArchive -> runArchiveOp env s
        StepSetReminders -> runSetRemindersOp env s
        StepRemoveReminders -> runRemoveRemindersOp env s
    )
    (batchStepsWithWork s)
    `finally` (envEvents env `trigger` executeBatchDoneEvt)

asyncCommandEventHandler ::
  Env ->
  AsyncCommand ->
  EventM Name HocketState ()
asyncCommandEventHandler (Env es port) FetchItems =
  withAsyncSlot OpFetchItems $ do
    s <- use id
    _ <- liftIO . async $ do
      let plan = planSync (s ^. hsLastUpdated)
          isUpdateFetch = plan ^. spKind == IncrementalAll
          suffix = maybe "" (\since -> " since: " <> formatPOSIXTime (since - 86400)) $ s ^. hsLastUpdated
      es `trigger` setStatusEvt (Just ("fetching" <> suffix))
      result <- trySync (retrieveItems port (plan ^. spSearch) (plan ^. spCollection))
      let rendered = either (Left . Just . exceptionMessage) (first errorMessageFromException) result
      for_ (fetchResultEvents isUpdateFetch rendered) (trigger es)
    pure ()
asyncCommandEventHandler _ (FetchCompleted wasAllCollectionsFetch batches) =
  id %= applyFetchCompleted wasAllCollectionsFetch batches
asyncCommandEventHandler (Env es _) (FetchFailed err) = do
  id %= completeAsyncOp
  liftIO (es `trigger` setStatusEvt (Just ("failed" <> maybe "<no err>" (": " <>) err)))
asyncCommandEventHandler env ExecuteBatch = do
  s <- use id
  case batchStepsWithWork s of
    [] -> pure ()
    _ -> withAsyncSlot OpExecuteBatch $ do
      _ <- liftIO $ async $ runBatchOp env s
      pure ()
asyncCommandEventHandler _ ExecuteBatchDone = do
  id %= completeAsyncOp
asyncCommandEventHandler _ (ArchivedItems outcome) =
  id %= applyArchived (outcome ^. soOk)
asyncCommandEventHandler _ (RemindersSet outcome) =
  id %= applyRemindersSet (outcome ^. soOk)
asyncCommandEventHandler _ (RemindersRemoved outcome) =
  id %= applyRemindersRemoved (outcome ^. soOk)

uiCommandEventHandler ::
  BChan HocketEvent ->
  UiCommand ->
  EventM Name HocketState ()
uiCommandEventHandler _ (ShiftItem bid) =
  id %= shiftItem bid
uiCommandEventHandler _ (ArchiveItem bid) =
  id %= archiveItem bid
uiCommandEventHandler _ (ShiftItemReminder bid) = do
  reminderTime <- liftIO nextDayAt7AM
  id %= shiftItemReminder bid reminderTime
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
uiCommandEventHandler es (OpenAndFlagItem bid) = do
  s <- use id
  case s ^. hsContents . at bid of
    Just (_, bit) -> do
      id %= setPendingAction bid ToBeArchived
      id %= syncForRender
      liftIO (es `trigger` browseItemEvt bit)
    Nothing -> pure ()
uiCommandEventHandler _ (SetAgentClients n) = id %= setAgentClients n
uiCommandEventHandler _ (SetAgentError e) = id %= setAgentError e
uiCommandEventHandler es (BrowseItem bit) = do
  res <- liftIO . try @SomeException $ browseItem "firefox" (URL . T.unpack $ view biLink bit)
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
  res <- liftIO . try @SomeException $ browseItem "xdg-open" (URL editUrl)
  case res of
    Left e -> liftIO $ es `trigger` setStatusEvt (Just (T.pack $ show e))
    Right () -> pure ()

myEventHandler ::
  Env ->
  BrickEvent Name HocketEvent ->
  EventM Name HocketState ()
myEventHandler env (VtyEvent e) = vtyEventHandler env e
myEventHandler env (AppEvent e) = internalEventHandler env e
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

runTuiApp :: Maybe AgentSocketOpt -> Maybe FixtureOpts -> IO ()
runTuiApp mAgentOpt Nothing = do
  ensureSchemaFile
  configPath <- getConfigPath
  cred <- input auto (T.pack configPath)
  withTuiLog $ \runLog -> runTui mAgentOpt False (servantPort runLog cred) initialState
-- Fixture mode never reads the config or credentials, so it cannot reach
-- the real account.
runTuiApp mAgentOpt (Just fo) = do
  seed <- LBS.readFile (foSeed fo)
  store <- either (\e -> hPutStrLn stderr ("hocket: bad fixture " <> foSeed fo <> ": " <> e) >> exitFailure) pure (fixtureStore fo seed)
  ref <- newIORef store
  runTui mAgentOpt True (fakePort getCurrentTime ref) initialState
    `finally` for_ (foDump fo) (\out -> readIORef ref >>= LBS.writeFile out . encodeFixtureDump)

runTui :: Maybe AgentSocketOpt -> Bool -> RaindropPort -> HocketState -> IO ()
runTui mAgentOpt isFixture port s0 = do
  events <- newBChan 10
  tz <- getCurrentTimeZone
  mAgent <- for mAgentOpt (startAgentServer events)
  unwindOnTermination
  vty <- mkVty Vty.defaultConfig
  void
    ( customMain
        vty
        (mkVty Vty.defaultConfig)
        (Just events)
        (app tz isFixture (Env events port) (snd <$> mAgent))
        s0
    )
    `finally` for_ mAgent (\((path, ours, bound), _) -> cleanupAgentSocket bound ours path)

-- | Bind the agent control socket and keep serving it in the background.
-- Server failures surface in the TUI status bar instead of killing the app.
-- The path comes back with what cleanup needs: whether hocket owns its
-- directory, and the socket it bound.
startAgentServer :: BChan HocketEvent -> AgentSocketOpt -> IO ((FilePath, Bool, BoundSocket), TVar AgentSnapshot)
startAgentServer events opt = do
  path <- case opt of
    AgentSocketDefault -> resolveAgentSocketPath
    AgentSocketAt p -> pure p
  ours <- socketDirIsOurs path
  bound <- newBoundSocket
  snapVar <- newTVarIO emptySnapshot
  let env =
        AgentEnv
          { aeSnapshot = snapVar,
            aeInject = writeBChanNonBlocking events,
            aeReminderTime = nextDayAt7AM
          }
  _ <- async $ do
    r <- try @SomeException (runAgentServer bound path env)
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
  pure ((path, ours, bound), snapVar)

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
      liftIO (runRaindrop (servantPort runStdoutLoggingT cred) (AddBookmark url mCollection tags)) `Catch.catch` \(e :: SomeException) -> do
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
    RunTUI mAgentOpt mFixture -> runTuiApp mAgentOpt mFixture
    AddBookmarkCmd url mCollection tags -> runAddCommand url mCollection tags
    AgentCall mpath agentCmd -> runAgentCommand mpath agentCmd

app :: TimeZone -> Bool -> Env -> Maybe (TVar AgentSnapshot) -> App HocketState HocketEvent Name
app tz isFixture env mSnapVar =
  App
    { appDraw = drawGui tz isFixture,
      appChooseCursor = \s ->
        if s ^. hsFilterActive
          then showCursorNamed FilterEditorName
          else Focus.focusRingCursor (view focusRing) s,
      appHandleEvent = \e -> do
        myEventHandler env e
        id %= syncForRender
        -- Mirror the post-render state for the agent socket. Persistent
        -- data sharing keeps this cheap; the item projection is only fully
        -- forced by a server thread that actually serves it.
        for_ mSnapVar $ \snapVar -> do
          s <- use id
          liftIO . atomically . modifyTVar' snapVar $ \old ->
            takeSnapshot (asVersion old + 1) s,
      appStartEvent = liftIO (envEvents env `trigger` fetchItemsEvt),
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
      (attrName "bar", barAttr),
      (attrName "bar" <> attrName "fixture", fixtureAttr),
      -- The editor sets its own attr, which would drop the bar's colours.
      (editFocusedAttr, barAttr)
    ]

-- | Loud on purpose: a fixture session must not pass for the real account.
fixtureAttr :: Vty.Attr
fixtureAttr = Vty.defAttr `Vty.withBackColor` flaggedRed `Vty.withForeColor` Vty.white `Vty.withStyle` Vty.bold

barAttr :: Vty.Attr
barAttr = Vty.defAttr `Vty.withBackColor` Vty.black `Vty.withForeColor` Vty.white

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

drawGui :: TimeZone -> Bool -> HocketState -> [Widget Name]
drawGui tz isFixture s = [w]
  where
    fixtureTag = if isFixture then withAttr (attrName "bar" <> attrName "fixture") (txt " FIXTURE ") else emptyWidget
    w =
      vBox
        [ fixtureTag
            <+> hBarWithHints
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
              "spc:Browse ent:Browse+flag e:Edit y:Copied URL r:Refresh S:Toggle future reminders v:Video filter V:Hide videos /:Filter X:Execute Flags a:Archive flag s:Reminder flag u:Unflag J/K:Jump U:Unflag all Q:Quit",
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
            then
              withAttr (attrName "bar") . padRight Max $
                txt "/" <+> renderEditor (txt . sanitizeForDisplay . T.concat) True (s ^. hsFilterEditor)
            else txt (fromMaybe " " (s ^. hsStatus))
        ]

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

retrieveItems :: RaindropPort -> Maybe Text -> RaindropCollectionId -> IO (Either ClientError [BookmarkItemBatch])
retrieveItems port searchParam collectionId = do
  tryClientError $
    unfoldrM
      ( \currentPage -> do
          (_, items) <- runRaindrop port (RetrieveBookmarks currentPage collectionId searchParam)

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

-- | One batch call, so its outcome applies to every id.
performArchive :: RaindropPort -> [BookmarkItem] -> IO (StepOutcome BookmarkItemId Text)
performArchive port items =
  concatOutcome <$> runCalls (runRaindrop port . BatchArchiveBookmarks) [map (view biId) items]

performSetReminders :: RaindropPort -> [(BookmarkItem, UTCTime)] -> IO (StepOutcome (BookmarkItem, UTCTime) Text)
performSetReminders port = runCalls (\(item, reminderTime) -> runRaindrop port (SetReminder (view biId item) reminderTime))

performRemoveReminders :: RaindropPort -> [BookmarkItem] -> IO (StepOutcome BookmarkItem Text)
performRemoveReminders port = runCalls (runRaindrop port . RemoveReminder . view biId)

-- | One log handle for the TUI's lifetime (vty owns the terminal, so the log
-- goes to a file). An unwritable state dir fails here, before the TUI starts.
withTuiLog :: ((forall x. LoggingT IO x -> IO x) -> IO a) -> IO a
withTuiLog k = do
  dir <- getXdgDirectory XdgState "hocket"
  createDirectoryIfMissing True dir
  withFile (dir </> "hocket.log") AppendMode $ \h -> do
    hSetBuffering h LineBuffering
    k (`runLoggingT` defaultOutput h)

tryClientError :: IO a -> IO (Either ClientError a)
tryClientError = try @ClientError

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

getItemsWithPendingAction :: PendingAction -> HocketState -> [BookmarkItem]
getItemsWithPendingAction targetAction s =
  [item | (action, item) <- Map.elems (s ^. hsContents), matchesAction action targetAction]
  where
    matchesAction (ToBeReminded _) (ToBeReminded _) = True
    matchesAction a b = a == b

getItemsToBeReminded :: HocketState -> [(BookmarkItem, UTCTime)]
getItemsToBeReminded s =
  [(item, time) | (ToBeReminded time, item) <- Map.elems (s ^. hsContents)]

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
