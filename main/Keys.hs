module Keys
  ( ItemKey (..),
    itemKeyEvents,
  )
where

import Control.Lens (view)
import Events
import Network.Bookmark.App.Msg (Intent (Open), Msg (Intent), Target (ById))
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
