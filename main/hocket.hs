{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, finally)
import Control.Lens.Operators ((^.))
import Control.Monad (unless)
import qualified Control.Monad.Catch as Catch (MonadCatch (catch))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
  ( LoggingT,
    defaultOutput,
    logErrorN,
    logInfoN,
    runLoggingT,
    runStdoutLoggingT,
  )
import qualified Data.ByteString.Lazy as LBS
  ( readFile,
    writeFile,
  )
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Time (getCurrentTime)
import Dhall (auto, input)
import Hocket.Adapter.Fake (encodeFixtureDump, fakePort)
import Hocket.Adapter.Raindrop (servantPort)
import Hocket.Agent.Protocol
  ( AgentCmd (..),
    FlagAction (..),
    ReadCmd (..),
    WriteCmd (..),
  )
import Hocket.App.Model (VideoFilterMode (..), initialModel)
import Hocket.Domain.Bookmark
  ( BookmarkItemId (BookmarkItemId),
    BookmarkRequest (AddBookmark),
    _BookmarkItemId,
  )
import Hocket.Port (RaindropPort (..))
import Hocket.Shell.Agent.Client (runAgentCommand)
import Hocket.Shell.Agent.Server (resolveAgentSocketPath)
import Hocket.Shell.Config (ensureSchemaFile, getConfigPath)
import Hocket.Shell.Fixture
  ( FixtureOpts (..),
    fixtureParser,
    fixtureStore,
  )
import Hocket.Shell.Signals (exitOnTerminated)
import Hocket.Shell.Tui.App (runTui)
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
import qualified Options.Applicative as Opt (auto)
import System.Directory
  ( XdgDirectory (XdgState),
    createDirectoryIfMissing,
    getXdgDirectory,
  )
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO
  ( BufferMode (LineBuffering),
    IOMode (AppendMode),
    hPutStrLn,
    hSetBuffering,
    stderr,
    withFile,
  )

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

runTuiApp :: Maybe AgentSocketOpt -> Maybe FixtureOpts -> IO ()
runTuiApp mAgentOpt Nothing = do
  mPath <- agentSocketPath mAgentOpt
  ensureSchemaFile
  configPath <- getConfigPath
  cred <- input auto (T.pack configPath)
  withTuiLog $ \runLog -> runTui mPath False (servantPort runLog cred) initialModel
-- Fixture mode never reads the config or credentials, so it cannot reach
-- the real account.
runTuiApp mAgentOpt (Just fo) = do
  mPath <- agentSocketPath mAgentOpt
  seed <- LBS.readFile (foSeed fo)
  store <- either (\e -> hPutStrLn stderr ("hocket: bad fixture " <> foSeed fo <> ": " <> e) >> exitFailure) pure (fixtureStore fo seed)
  ref <- newIORef store
  runTui mPath True (fakePort getCurrentTime ref) initialModel
    `finally` for_ (foDump fo) (\out -> readIORef ref >>= LBS.writeFile out . encodeFixtureDump)

agentSocketPath :: Maybe AgentSocketOpt -> IO (Maybe FilePath)
agentSocketPath = traverse $ \case
  AgentSocketDefault -> resolveAgentSocketPath
  AgentSocketAt p -> pure p

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

-- | One log handle for the TUI's lifetime (vty owns the terminal, so the log
-- goes to a file). An unwritable state dir fails here, before the TUI starts.
withTuiLog :: ((forall x. LoggingT IO x -> IO x) -> IO a) -> IO a
withTuiLog k = do
  dir <- getXdgDirectory XdgState "hocket"
  createDirectoryIfMissing True dir
  withFile (dir </> "hocket.log") AppendMode $ \h -> do
    hSetBuffering h LineBuffering
    k (`runLoggingT` defaultOutput h)
