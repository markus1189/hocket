{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The TUI core as one pure function. Until the end of Phase 3 it only
-- covers the handler groups that have moved; 'Nothing' means a message is
-- still handled the old way (its producer still posts the old event).
module Network.Bookmark.App.Update (update) where

import qualified Brick.Widgets.List as L
import Control.Lens
import Data.Time (UTCTime)
import qualified Data.Vector as V
import Network.Bookmark.Agent.Protocol (FlagAction (..))
import Network.Bookmark.App.Effect
import Network.Bookmark.App.Msg
import Network.Bookmark.Reminder (reminderPolicy)
import Network.Bookmark.Types
import Network.Bookmark.Ui.State

update :: Msg -> HocketState -> Maybe (HocketState, [Effect])
update msg s = case msg of
  Intent Quit -> Just (s, [Halt])
  Intent (Open t) -> Just (s, [OpenUrl Firefox (bit ^. biLink) | bit <- target t])
  Intent (EditInRaindrop t) -> Just (s, [OpenUrl XdgOpen (editUrl bit) | bit <- target t])
  Intent (CopyUrl t) -> Just (s, [CopyText (bit ^. biLink) | bit <- target t])
  Intent (OpenAndFlag bid) -> Just $ case s ^? hsContents . ix bid . _2 of
    Just bit -> (syncForRender (setPendingAction bid ToBeArchived s), [OpenUrl Firefox (bit ^. biLink)])
    Nothing -> (s, [])
  -- The key intents carry no guard (keyIntents); the no-ops of the old key
  -- handlers live here, and a no-op does not move the cursor either.
  Intent (SetArchive t) -> Just (onTarget t $ \bid flag -> [archiveItem bid | flag /= ToBeArchived])
  Intent (ToggleArchive t) -> Just (onTarget t $ \bid _ -> [shiftItem bid])
  Intent (ToggleReminder t now) -> Just (onTarget t $ \bid flag -> [shiftItemReminder bid (reminderAt now) | not (isToBeReminded flag)])
  Intent (Unflag t) -> Just (onTarget t $ \bid flag -> [unflagItem bid | flag /= None])
  Intent (SetFlag bid act now) -> Just (setPendingAction bid (flagFor now act) s, [])
  Intent ClearAllFlags -> Just (clearAllFlags s, [])
  Intent FlagAllArchive -> Just (setAllFlagsToArchive s, [])
  Intent (Select bid) -> Just (s & itemList %~ selectById bid, [])
  Intent (SetFilter q) -> refilter (setFilterQuery q)
  Intent (SetVideoFilter m) -> refilter (setVideoFilterMode m)
  Intent ToggleVideoOnly -> refilter toggleVideoFilter
  Intent ToggleHideVideos -> refilter toggleInvertedVideoFilter
  Intent ToggleFutureReminders -> refilter toggleShowFutureReminders
  Intent (SetShowFutureReminders b) -> refilter (setShowFutureReminders b)
  Intent (SetStatus t) -> Just (s & hsStatus ?~ t, [])
  AgentClients n -> Just (setAgentClients n s, [])
  -- The status line is overwritten within a frame, so the header keeps it.
  AgentServerDied err -> Just (s & hsStatus ?~ ("agent socket failed: " <> err) & setAgentError (Just err), [])
  SystemFailed err -> Just (s & hsStatus ?~ err, [])
  Copied url -> Just (s & hsStatus ?~ ("Copied: " <> url), [])
  _ -> Nothing
  where
    -- What is visible changes, so the list follows at once, as before.
    refilter f = Just (syncForRender (f s), [])
    target = maybe [] pure . targetItem s
    -- Apply the change the item's flag calls for, if any.
    onTarget t change = case targetItem s t of
      Just bit -> case change (bit ^. biId) (getPendingActionForItem (bit ^. biId) s) of
        f : _ -> (f s, [])
        [] -> (s, [])
      Nothing -> (s, [])
    editUrl bit = "https://app.raindrop.io/my/-1/item/" <> bit ^. biId . _BookmarkItemId <> "/edit"

-- | Agent @select_item@: only an item the list shows can be selected.
selectById :: BookmarkItemId -> L.List Name BookmarkItem -> L.List Name BookmarkItem
selectById bid l = maybe l (`L.listMoveTo` l) (V.findIndex ((== bid) . (^. biId)) (l ^. L.listElementsL))

reminderAt :: Now -> UTCTime
reminderAt now = reminderPolicy (nowZone now) (nowUtc now)

-- | The agent's idempotent flags.
flagFor :: Now -> FlagAction -> PendingAction
flagFor now = \case
  FlagArchive -> ToBeArchived
  FlagReminder -> ToBeReminded (reminderAt now)
  FlagRemoveReminder -> ReminderToBeRemoved
  FlagNone -> None

-- | Keys act on the focused item; an unknown id is no item.
targetItem :: HocketState -> Target -> Maybe BookmarkItem
targetItem s = \case
  Selected -> focusedItem s
  ById bid -> s ^? hsContents . ix bid . _2
