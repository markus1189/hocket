{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The shell's view state (plan §4.3): brick's list and the filter editor.
-- 'reconcile' derives both from the model after every event; nothing reads
-- them back except the editor's text, which the shell reports as
-- 'Hocket.App.Msg.FilterText'.
module Hocket.Shell.Tui.Reconcile
  ( UiState,
    uiList,
    uiEditor,
    initialUi,
    editorQuery,
    reconcile,
  )
where

import Brick.Widgets.Edit (Editor, applyEdit, editorText, getEditContents)
import qualified Brick.Widgets.List as L
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import qualified Data.Vector as V
import Hocket.App.Model
import Hocket.Domain.Bookmark

data UiState = UiState
  { _uiList :: !(L.List Name BookmarkItem),
    _uiEditor :: !(Editor Text Name)
  }

makeLenses ''UiState

initialUi :: UiState
initialUi = UiState (L.list ItemListName V.empty 1) (editorText FilterEditorName (Just 1) "")

-- | What the filter editor holds now.
editorQuery :: UiState -> Text
editorQuery = T.concat . getEditContents . view uiEditor

-- | The list shows what is visible with the selection on 'mSelected'. The
-- editor gets the query only when the two differ (an agent's set_filter, a
-- cancel), cursor at the end, so typing keeps its cursor and typing after
-- set_filter appends.
reconcile :: Model -> UiState -> UiState
reconcile s ui =
  ui
    & uiList . L.listElementsL .~ visible
    & uiList . L.listSelectedL .~ (s ^. mSelected >>= \bid -> V.findIndex ((== bid) . view biId) visible)
    & editor
  where
    visible = s ^. mVisible
    editor u
      | editorQuery u == s ^. mFilterQuery = u
      | otherwise = u & uiEditor %~ applyEdit (const (Z.gotoEOL (Z.textZipper [s ^. mFilterQuery] (Just 1))))
