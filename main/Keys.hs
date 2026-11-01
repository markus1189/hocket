module Keys
  ( ItemKey (..),
    itemKeyEvents,
    keyIntents,
  )
where

import Control.Lens (view, (^.))
import Events
import Graphics.Vty (Event (EvKey), Key (KChar, KEnter))
import Network.Bookmark.App.Msg (Intent (Open), Msg (Intent), Now, Target (ById))
import qualified Network.Bookmark.App.Msg as I
import Network.Bookmark.Types (PendingAction (..), biId)
import Network.Bookmark.Ui.State

-- | The normal-mode keys that act on the focused item.
data ItemKey = KeyArchive | KeyRemind | KeyUnflag | KeyEnter | KeySpace
  deriving (Show, Eq, Enum, Bounded)

-- | What a key posts, decided on the state at key time; the flag change
-- itself happens when the posted 'ShiftItem' / 'ArchiveItem' /
-- 'ShiftItemReminder' arrives.
itemKeyEvents :: ItemKey -> HocketState -> [HocketEvent]
itemKeyEvents key s = case focusedItem s of
  Nothing -> []
  Just bit ->
    let bid = view biId bit
        action = getPendingActionForItem bid s
        -- By id: the item focused when the key was pressed.
        open = HocketMsg (Intent (Open (ById bid)))
     in case key of
          KeySpace -> [open]
          KeyEnter -> [open, shiftItemEvt bid]
          KeyArchive -> [archiveItemEvt bid | action /= ToBeArchived]
          KeyRemind -> [shiftItemReminderEvt bid | not (isToBeReminded action)]
          KeyUnflag ->
            [shiftItemEvt bid | action == ToBeArchived]
              <> [shiftItemReminderEvt bid | isReminderAction action]

-- | Normal-mode keys as Intents. 'Nothing' is a key the shell keeps:
-- navigation (j/k/arrows, J/K to the next flagged item), @/@ (enters filter
-- mode synchronously), and every key while the filter editor has focus, so
-- @Q@ typed into a filter never quits. Flag guards such as @a@ on an archived
-- item are 'update''s, so action keys always yield their Intent.
keyIntents :: Now -> Event -> HocketState -> Maybe [Intent]
keyIntents now e s
  | s ^. hsFilterActive = Nothing
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
      _ -> Nothing
