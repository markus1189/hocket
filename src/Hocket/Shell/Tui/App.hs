{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Hocket.Shell.Tui.App
  ( runTui,
  )
where

import Brick
  ( App (..),
    BrickEvent (AppEvent, VtyEvent),
    EventM,
    customMain,
    halt,
    showCursorNamed,
    zoom,
  )
import Brick.Main (lookupViewport)
import Brick.Types (vpSize)
import Brick.Widgets.Edit (applyEdit, handleEditorEvent)
import Control.Concurrent.STM (TVar, atomically)
import Control.Lens (makeLenses, uses, view)
import Control.Lens.Combinators (use)
import Control.Lens.Operators ((%=), (.=), (^.))
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_, traverse_)
import Data.Text.Zipper.Generic.Words (deletePrevWord)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone)
import Graphics.Vty
  ( Event (EvKey),
    Key (KChar, KDown, KUp),
    Modifier (MCtrl),
  )
import qualified Graphics.Vty as Vty (defaultConfig)
import Graphics.Vty.Input.Events (Key (KEnter, KEsc))
import Graphics.Vty.Platform.Unix (mkVty)
import Hocket.Adapter.System (systemNow, systemPort)
import Hocket.Agent.Snapshot (AgentSnapshot)
import Hocket.App.Model
  ( Model,
    Name (..),
    mFilterActive,
    mFilterQuery,
    settle,
  )
import Hocket.App.Msg
  ( Intent (FilterCancel, FilterLock, FilterText, Navigate, Refresh),
    Msg (..),
  )
import Hocket.App.Nav (Nav (..))
import Hocket.Port (RaindropPort (..))
import Hocket.Shell.Agent.Server (withAgentServer)
import Hocket.Shell.Interpret
  ( Env (..),
    newEnv,
    publish,
    step,
    tookMsg,
  )
import Hocket.Shell.Signals (unwindOnTermination)
import Hocket.Shell.Tui.Keymap (keyIntents)
import Hocket.Shell.Tui.Reconcile
  ( UiState,
    editorQuery,
    initialUi,
    reconcile,
    uiEditor,
  )
import Hocket.Shell.Tui.View (drawGui, hocketAttrMap)

-- | The model, and what the shell derives from it for brick (plan §4.3).
data AppState = AppState {_model :: !Model, _ui :: !UiState}

makeLenses ''AppState

vtyEventHandler ::
  Env ->
  Event ->
  EventM Name AppState ()
vtyEventHandler env e = do
  s <- use model
  if s ^. mFilterActive
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
  query <- use (model . mFilterQuery)
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

-- | The TUI, serving the agent socket at this path if one is given.
runTui :: Maybe FilePath -> Bool -> RaindropPort -> Model -> IO ()
runTui mPath isFixture port s0 = do
  env <- newEnv port systemPort systemNow
  tz <- getCurrentTimeZone
  unwindOnTermination
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

app :: TimeZone -> Bool -> Env -> Maybe (TVar AgentSnapshot) -> App AppState Msg Name
app tz isFixture env mSnapVar =
  App
    { appDraw = \(AppState s u) -> drawGui tz isFixture s u,
      appChooseCursor = \s ->
        if s ^. model . mFilterActive
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
