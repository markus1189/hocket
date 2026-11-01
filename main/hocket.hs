{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Main
  ( main,
  )
where

import AgentClient (runAgentCommand)
import AgentServer (resolveAgentSocketPath, withAgentServer)
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
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Edit (applyEdit, editFocusedAttr, handleEditorEvent, renderEditor)
import qualified Brick.Widgets.List as L
import Control.Applicative ((<|>))
import Control.Concurrent.STM (TVar, atomically)
import Control.Exception (SomeException, finally)
import Control.Lens (makeLenses, uses, view)
import Control.Lens.Combinators (use)
import Control.Lens.Operators
  ( (%=),
    (.=),
    (^.),
  )
import Control.Monad (unless, void, when)
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, defaultOutput, logErrorN, logInfoN, runLoggingT, runStdoutLoggingT)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (for_, traverse_)
import Data.IORef (newIORef, readIORef)
import Signals (exitOnTerminated, unwindOnTermination)
#if MIN_VERSION_base(4,20,0)
import Data.List (find, isPrefixOf)
#else
import Data.List (find, foldl', isPrefixOf)
#endif
import Brick.Main (lookupViewport)
import Brick.Types (vpSize)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Zipper.Generic.Words (deletePrevWord)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime
  ( TimeZone,
    getCurrentTimeZone,
    utcToLocalTime,
  )
import Data.Traversable (for)
import Dhall (auto, input)
import Fixture (FixtureOpts (..), fixtureParser, fixtureStore)
import Formatting (sformat, (%))
import qualified Formatting as F
import qualified Formatting.Time as F
import Graphics.Vty (Event (EvKey), Key (KChar, KDown, KUp), Modifier (MCtrl))
import qualified Graphics.Vty as Vty
import Graphics.Vty.Input.Events (Key (KEnter, KEsc))
import Graphics.Vty.Platform.Unix (mkVty)
import Interpret (Env (..), newEnv, publish, step, tookMsg)
import Keys (keyIntents)
import Network.Bookmark.Agent.Protocol
  ( AgentCmd (..),
    FlagAction (..),
    ReadCmd (..),
    WriteCmd (..),
  )
import Network.Bookmark.Agent.Snapshot (AgentSnapshot)
import Network.Bookmark.App.Msg (Intent (FilterCancel, FilterLock, FilterText, Navigate, Refresh), Msg (..))
import Network.Bookmark.App.Nav (Nav (..))
import Network.Bookmark.Fake (encodeFixtureDump, fakePort)
import Network.Bookmark.Port (RaindropPort (..))
import Network.Bookmark.Types
  ( BookmarkItem,
    BookmarkItemId (..),
    BookmarkRequest (AddBookmark),
    PendingAction (..),
    biCreated,
    biExcerpt,
    biId,
    biImportant,
    biLink,
    biNote,
    biReminder,
    biTitle,
    _BookmarkItemId,
  )
import Network.Bookmark.Ui.Reconcile (UiState, editorQuery, initialUi, reconcile, uiEditor, uiList)
import Network.Bookmark.Ui.State
  ( HocketState,
    Name (..),
    VideoFilterMode (..),
    focusedItem,
    getPendingActionForItem,
    hsAgentClients,
    hsAgentError,
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
    settle,
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
import System (systemNow, systemPort)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), IOMode (AppendMode), hPutStrLn, hSetBuffering, stderr, withFile)

-- | The model, and what the shell derives from it for brick (plan §4.3).
data AppState = AppState {_model :: !HocketState, _ui :: !UiState}

makeLenses ''AppState

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
      meth "clear_all_flags" "Drop every staged action (the 'U' key)" (pure (AWrite CmdClearFlags)),
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

vtyEventHandler ::
  Env ->
  Event ->
  EventM Name AppState ()
vtyEventHandler env e = do
  s <- use model
  if s ^. hsFilterActive
    then case e of
      -- Handled here, not via the BChan, so they stay ordered with the
      -- keystrokes the editor already applied synchronously.
      EvKey KEsc [] -> applyMsg env (Intent FilterCancel)
      EvKey KEnter [] -> applyMsg env (Intent FilterLock)
      EvKey KUp [] -> listHeight >>= \h -> applyMsg env (Intent (Navigate h Up))
      EvKey KDown [] -> listHeight >>= \h -> applyMsg env (Intent (Navigate h Down))
      -- Brick's editor has no Ctrl-W.
      EvKey (KChar 'w') [MCtrl] -> (ui . uiEditor %= applyEdit deletePrevWord) >> reportEditor env
      _ -> zoom (ui . uiEditor) (handleEditorEvent (VtyEvent e)) >> reportEditor env
    else do
      now <- liftIO (envNow env)
      height <- listHeight
      -- Keys keyIntents does not know are none of hocket's.
      for_ (keyIntents now height e s) (traverse_ (applyMsg env . Intent))

-- | One message through the 'step' the headless loop runs too.
applyMsg :: Env -> Msg -> EventM Name AppState ()
applyMsg env m = do
  (s', halted) <- liftIO . step env m =<< use model
  model .= s'
  when halted halt

-- | The editor applied a key: tell update, which owns the query (plan D3).
reportEditor :: Env -> EventM Name AppState ()
reportEditor env = do
  typed <- uses ui editorQuery
  query <- use (model . hsFilterQuery)
  unless (typed == query) $ applyMsg env (Intent (FilterText typed))

-- | The list viewport's height in rows, as brick's page moves measure it;
-- 0 before the list was first drawn, which makes a page move no move.
listHeight :: EventM Name AppState Int
listHeight = maybe 0 (snd . view vpSize) <$> lookupViewport ItemListName

myEventHandler ::
  Env ->
  BrickEvent Name Msg ->
  EventM Name AppState ()
myEventHandler env (VtyEvent e) = vtyEventHandler env e
myEventHandler env (AppEvent m) = applyMsg env m
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
  env <- newEnv port systemPort systemNow
  tz <- getCurrentTimeZone
  unwindOnTermination
  mPath <- for mAgentOpt $ \case
    AgentSocketDefault -> resolveAgentSocketPath
    AgentSocketAt p -> pure p
  -- The vty starts inside the server's bracket: a signal during mkVty must
  -- still remove the socket.
  maybe ($ Nothing) (\path -> withAgentServer env path . (. Just)) mPath $ \mSnapVar -> do
    vty <- mkVty Vty.defaultConfig
    void $
      customMain
        vty
        (mkVty Vty.defaultConfig)
        (Just (envEvents env))
        (app tz isFixture env mSnapVar)
        (AppState s0 initialUi)

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
main = exitOnTerminated $ do
  cmd <- execParser opts
  case cmd of
    RunTUI mAgentOpt mFixture -> runTuiApp mAgentOpt mFixture
    AddBookmarkCmd url mCollection tags -> runAddCommand url mCollection tags
    AgentCall mpath agentCmd -> runAgentCommand mpath agentCmd

app :: TimeZone -> Bool -> Env -> Maybe (TVar AgentSnapshot) -> App AppState Msg Name
app tz isFixture env mSnapVar =
  App
    { appDraw = drawGui tz isFixture,
      appChooseCursor = \s ->
        if s ^. model . hsFilterActive
          then showCursorNamed FilterEditorName
          else const Nothing,
      appHandleEvent = \e -> do
        myEventHandler env e
        model %= settle
        settled <- use model
        ui %= reconcile settled
        for_ mSnapVar $ \snapVar -> liftIO . publish snapVar =<< use model
        case e of
          AppEvent _ -> liftIO (atomically (tookMsg env))
          _ -> pure (),
      appStartEvent = applyMsg env (Intent Refresh),
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

drawGui :: TimeZone -> Bool -> AppState -> [Widget Name]
drawGui tz isFixture (AppState s u) = [w]
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
            (u ^. uiList),
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
                txt "/" <+> renderEditor (txt . sanitizeForDisplay . T.concat) True (u ^. uiEditor)
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

-- | One log handle for the TUI's lifetime (vty owns the terminal, so the log
-- goes to a file). An unwritable state dir fails here, before the TUI starts.
withTuiLog :: ((forall x. LoggingT IO x -> IO x) -> IO a) -> IO a
withTuiLog k = do
  dir <- getXdgDirectory XdgState "hocket"
  createDirectoryIfMissing True dir
  withFile (dir </> "hocket.log") AppendMode $ \h -> do
    hSetBuffering h LineBuffering
    k (`runLoggingT` defaultOutput h)

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
