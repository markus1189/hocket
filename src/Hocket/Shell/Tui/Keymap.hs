module Hocket.Shell.Tui.Keymap
  ( keyIntents,
  )
where

import Control.Lens ((^.))
import Graphics.Vty (Event (EvKey), Key (..), Modifier (MCtrl))
import Hocket.App.Model
import Hocket.App.Msg (Intent, Now)
import qualified Hocket.App.Msg as I
import Hocket.App.Nav (Nav (..))

-- | Normal-mode keys as Intents. 'Nothing' is a key the shell keeps:
-- navigation (j/k/arrows, J/K to the next flagged item), @/@ (enters filter
-- mode synchronously), and every key while the filter editor has focus, so
-- @Q@ typed into a filter never quits. Flag guards such as @a@ on an archived
-- item are 'update''s, so action keys always yield their Intent.
keyIntents :: Now -> Int -> Event -> Model -> Maybe [Intent]
keyIntents now height e s
  | s ^. mFilterActive = Nothing
  | otherwise = case e of
      EvKey (KChar ' ') [] -> Just [I.Open I.Selected]
      EvKey KEnter [] -> Just [I.Open I.Selected, I.ToggleArchive I.Selected]
      EvKey (KChar 'a') [] -> Just [I.SetArchive I.Selected]
      EvKey (KChar 's') [] -> Just [I.ToggleReminder I.Selected now]
      EvKey (KChar 'u') [] -> Just [I.Unflag I.Selected]
      EvKey (KChar 'e') [] -> Just [I.EditInRaindrop I.Selected]
      EvKey (KChar 'y') [] -> Just [I.CopyUrl I.Selected]
      EvKey (KChar 'r') [] -> Just [I.Refresh]
      EvKey (KChar 'X') [] -> Just [I.Execute]
      EvKey (KChar 'U') [] -> Just [I.ClearAllFlags]
      EvKey (KChar 'S') [] -> Just [I.ToggleFutureReminders]
      EvKey (KChar 'v') [] -> Just [I.ToggleVideoOnly]
      EvKey (KChar 'V') [] -> Just [I.ToggleHideVideos]
      EvKey (KChar 'Q') [] -> Just [I.Quit]
      EvKey (KChar '/') [] -> Just [I.FilterEnter]
      -- brick's list keys (handleListEvent, handleListEventVi), plus J/K.
      EvKey (KChar 'k') [] -> nav Up
      EvKey KUp [] -> nav Up
      EvKey (KChar 'j') [] -> nav Down
      EvKey KDown [] -> nav Down
      EvKey (KChar 'g') [] -> nav Top
      EvKey KHome [] -> nav Top
      EvKey (KChar 'G') [] -> nav Bottom
      EvKey KEnd [] -> nav Bottom
      EvKey (KChar 'f') [MCtrl] -> nav (Page 1)
      EvKey KPageDown [] -> nav (Page 1)
      EvKey (KChar 'b') [MCtrl] -> nav (Page (-1))
      EvKey KPageUp [] -> nav (Page (-1))
      EvKey (KChar 'd') [MCtrl] -> nav (HalfPage 1)
      EvKey (KChar 'u') [MCtrl] -> nav (HalfPage (-1))
      EvKey (KChar 'J') [] -> nav NextFlagged
      EvKey (KChar 'K') [] -> nav PrevFlagged
      _ -> Nothing
  where
    nav n = Just [I.Navigate height n]
