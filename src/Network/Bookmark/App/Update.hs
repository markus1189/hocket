{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The TUI core as one pure function: every message, from the keyboard,
-- the agent or a worker, and what the shell should do about it.
module Network.Bookmark.App.Update (update) where

import Control.Lens
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Vector as V
import Network.Bookmark.Agent.Protocol (FlagAction (..))
import Network.Bookmark.App.Effect
import Network.Bookmark.App.Msg
import Network.Bookmark.App.Plan (planBatch)
import Network.Bookmark.Batch (soOk)
import Network.Bookmark.Reminder (reminderPolicy)
import Network.Bookmark.Types
import Network.Bookmark.Ui.State
import Network.Bookmark.Ui.Sync (applyArchived, applyFetchCompleted, applyRemindersRemoved, applyRemindersSet, formatPOSIXTime, planSync)

update :: Msg -> HocketState -> (HocketState, [Effect])
update msg s = case msg of
  Intent Quit -> (s, [Halt])
  Intent (Open t) -> (s, [OpenUrl Firefox (bit ^. biLink) | bit <- target t])
  Intent (EditInRaindrop t) -> (s, [OpenUrl XdgOpen (editUrl bit) | bit <- target t])
  Intent (CopyUrl t) -> (s, [CopyText (bit ^. biLink) | bit <- target t])
  Intent (OpenAndFlag bid) -> case s ^? hsContents . ix bid . _2 of
    Just bit -> (settle (setPendingAction bid ToBeArchived s), [OpenUrl Firefox (bit ^. biLink)])
    Nothing -> (s, [])
  -- The key intents carry no guard (keyIntents); the no-ops of the old key
  -- handlers live here, and a no-op does not move the cursor either.
  Intent (SetArchive t) -> onTarget t $ \bid flag -> [archiveItem bid | flag /= ToBeArchived]
  Intent (ToggleArchive t) -> onTarget t $ \bid _ -> [shiftItem bid]
  Intent (ToggleReminder t now) -> onTarget t $ \bid flag -> [shiftItemReminder bid (reminderAt now) | not (isToBeReminded flag)]
  Intent (Unflag t) -> onTarget t $ \bid flag -> [unflagItem bid | flag /= None]
  Intent (SetFlag bid act now) -> (setPendingAction bid (flagFor now act) s, [])
  Intent ClearAllFlags -> (clearAllFlags s, [])
  Intent FlagAllArchive -> (setAllFlagsToArchive s, [])
  -- Agent @select_item@: only an item the list shows can be selected.
  Intent (Select bid) -> (s & hsSelected %~ \cur -> if V.any ((== bid) . (^. biId)) (s ^. hsVisible) then Just bid else cur, [])
  Intent (SetFilter q) -> refilter (setFilterQuery q)
  Intent (SetVideoFilter m) -> refilter (setVideoFilterMode m)
  Intent ToggleVideoOnly -> refilter toggleVideoFilter
  Intent ToggleHideVideos -> refilter toggleInvertedVideoFilter
  Intent ToggleFutureReminders -> refilter toggleShowFutureReminders
  Intent (SetShowFutureReminders b) -> refilter (setShowFutureReminders b)
  Intent (SetStatus t) -> (s & hsStatus ?~ t, [])
  AgentClients n -> (setAgentClients n s, [])
  -- The status line is overwritten within a frame, so the header keeps it.
  AgentServerDied err -> (s & hsStatus ?~ ("agent socket failed: " <> err) & setAgentError (Just err), [])
  -- One async op at a time: a refresh while one runs is dropped, as before.
  Intent Refresh -> case tryAcquireAsyncOp OpFetchItems s of
    Nothing -> (s, [])
    Just fetching -> (fetching & hsStatus ?~ ("fetching" <> since), [StartFetch (planSync lastUpdated)])
  FetchCompleted wasAllCollectionsFetch batches -> (applyFetchCompleted wasAllCollectionsFetch batches (s & hsStatus .~ Nothing), [])
  FetchFailed err -> (completeAsyncOp s & hsStatus ?~ ("failed" <> maybe "<no err>" (": " <>) err), [])
  -- Nothing to send, or an async op already running: dropped, as before.
  Intent Execute -> case (planBatch s, tryAcquireAsyncOp OpExecuteBatch s) of
    (Just plan, Just running) -> (running, [StartBatch plan])
    _ -> (s, [])
  BatchStepStarted step -> (s & hsStatus ?~ stepLabel step, [])
  StepDone result -> (applyStep result s, [])
  BatchFinished summary -> (completeAsyncOp s & hsStatus .~ summary, [])
  SystemFailed err -> (s & hsStatus ?~ err, [])
  Copied url -> (s & hsStatus ?~ ("Copied: " <> url), [])
  where
    lastUpdated = s ^. hsLastUpdated
    since = maybe "" (\t -> " since: " <> formatPOSIXTime (t - 86400)) lastUpdated
    -- What is visible changes, so the list follows at once, as before.
    refilter f = (settle (f s), [])
    target = maybe [] pure . targetItem s
    -- Apply the change the item's flag calls for, if any.
    onTarget t change = case targetItem s t of
      Just bit -> case change (bit ^. biId) (getPendingActionForItem (bit ^. biId) s) of
        f : _ -> (f s, [])
        [] -> (s, [])
      Nothing -> (s, [])
    editUrl bit = "https://app.raindrop.io/my/-1/item/" <> bit ^. biId . _BookmarkItemId <> "/edit"

stepLabel :: BatchStep -> Text
stepLabel = \case
  StepArchive -> "archiving"
  StepSetReminders -> "setting reminders"
  StepRemoveReminders -> "removing reminders"

-- | Only what the server accepted changes locally; failed and skipped items
-- keep their flags.
applyStep :: StepResult -> HocketState -> HocketState
applyStep = \case
  Archived o -> applyArchived (o ^. soOk)
  RemindersSetAt o -> applyRemindersSet (o ^. soOk)
  RemindersRemoved o -> applyRemindersRemoved (o ^. soOk)

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
