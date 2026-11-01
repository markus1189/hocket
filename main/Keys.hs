module Keys
  ( ItemKey (..),
    itemKeyEvents,
  )
where

import Control.Lens (view)
import Events
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
     in case key of
          KeySpace -> [browseItemEvt bit]
          KeyEnter -> [browseItemEvt bit, shiftItemEvt bid]
          KeyArchive -> [archiveItemEvt bid | action /= ToBeArchived]
          KeyRemind -> [shiftItemReminderEvt bid | not (isToBeReminded action)]
          KeyUnflag ->
            [shiftItemEvt bid | action == ToBeArchived]
              <> [shiftItemReminderEvt bid | isReminderAction action]
