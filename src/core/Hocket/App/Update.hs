{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The TUI core as one pure function: every message, from the keyboard,
-- the agent or a worker, and what the shell should do about it.
module Hocket.App.Update (update, batchStatus) where

import Control.Applicative ((<|>))
import Control.Lens
import Data.Bifunctor (first)
import Data.List (nub)
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Vector as V
import Hocket.Agent.Protocol (FlagAction (..))
import Hocket.App.Batch (Failure (..), StepOutcome, soAborted, soFailed, soOk, soSkipped)
import Hocket.App.Effect
import Hocket.App.Model
import Hocket.App.Msg
import Hocket.App.Nav (navigate)
import Hocket.App.Plan (planBatch, planSteps)
import Hocket.App.Sync (applyArchived, applyFetchCompleted, applyRemindersRemoved, applyRemindersSet, formatPOSIXTime, planSync)
import Hocket.Domain.Bookmark
import Hocket.Domain.Reminder (reminderPolicy)

update :: Msg -> Model -> (Model, [Effect])
update msg s = case msg of
  Intent Quit -> (s, [Halt])
  Intent (Open t) -> (s, [OpenUrl Firefox (bit ^. biLink) | bit <- target t])
  Intent (EditInRaindrop t) -> (s, [OpenUrl XdgOpen (editUrl bit) | bit <- target t])
  Intent (CopyUrl t) -> (s, [CopyText (bit ^. biLink) | bit <- target t])
  Intent (OpenAndFlag bid) -> case s ^? mContents . ix bid . _2 of
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
  Intent (Select bid) -> (s & mSelected %~ \cur -> if V.any ((== bid) . (^. biId)) (s ^. mVisible) then Just bid else cur, [])
  Intent FilterEnter -> (enterFilterMode s, [])
  Intent (FilterText t) -> refilter (mFilterQuery .~ t)
  Intent FilterLock -> (lockFilter s, [])
  Intent FilterCancel -> refilter cancelFilter
  Intent (SetFilter q) -> refilter (setFilterQuery q)
  Intent (SetVideoFilter m) -> refilter (setVideoFilterMode m)
  Intent ToggleVideoOnly -> refilter toggleVideoFilter
  Intent ToggleHideVideos -> refilter toggleInvertedVideoFilter
  Intent ToggleFutureReminders -> refilter toggleShowFutureReminders
  Intent (SetShowFutureReminders b) -> refilter (setShowFutureReminders b)
  Intent (SetStatus t) -> (s & mStatus ?~ t, [])
  Intent (Navigate height nav) -> (s & mSelected .~ navigate height nav (s ^. mVisible) (`getPendingActionForItem` s) (s ^. mSelected), [])
  AgentWrite n i -> first (mAppliedSeq .~ n) (update (Intent i) s)
  AgentClients n -> (setAgentClients n s, [])
  -- The status line is overwritten within a frame, so the header keeps it.
  AgentServerDied err -> (s & mStatus ?~ ("agent socket failed: " <> err) & setAgentError (Just err), [])
  -- One async op at a time: a refresh while one runs is dropped, as before.
  Intent Refresh -> case tryAcquireAsyncOp OpFetchItems s of
    Nothing -> (s, [])
    Just fetching -> (fetching & mStatus ?~ ("fetching" <> since), [StartFetch (planSync lastUpdated)])
  FetchCompleted wasAllCollectionsFetch batches -> (applyFetchCompleted wasAllCollectionsFetch batches (s & mStatus .~ Nothing), [])
  FetchFailed err -> (completeAsyncOp s & mStatus ?~ ("failed" <> maybe "<no err>" (": " <>) err), [])
  -- Nothing to send, or an async op already running: dropped, as before.
  Intent Execute -> case (planBatch s, tryAcquireAsyncOp OpExecuteBatch s) of
    (Just plan, Just running) -> (running & mBatch ?~ BatchProgress (planSteps plan) [], [StartBatch plan])
    _ -> (s, [])
  BatchStepStarted step -> (s & mStatus ?~ stepLabel step, [])
  StepDone result -> (applyStep result s & mBatch . _Just . bpRan <>~ [ranStep result], [])
  -- An error that ended the batch early replaces the summary, as before.
  BatchFinished err -> (completeAsyncOp s & mBatch .~ Nothing & mStatus .~ (err <|> (s ^. mBatch >>= summary)), [])
  SystemFailed err -> (s & mStatus ?~ err, [])
  Copied url -> (s & mStatus ?~ ("Copied: " <> url), [])
  where
    lastUpdated = s ^. mLastUpdated
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
    summary p = batchStatus (p ^. bpPlanned) (p ^. bpRan)
    editUrl bit = "https://app.raindrop.io/my/-1/item/" <> bit ^. biId . _BookmarkItemId <> "/edit"

stepLabel :: BatchStep -> Text
stepLabel = \case
  StepArchive -> "archiving"
  StepSetReminders -> "setting reminders"
  StepRemoveReminders -> "removing reminders"

-- | A step's outcome as the summary counts it.
ranStep :: StepResult -> (BatchStep, StepOutcome () Text)
ranStep = \case
  Archived o -> (StepArchive, erase o)
  RemindersSetAt o -> (StepSetReminders, erase o)
  RemindersRemoved o -> (StepRemoveReminders, erase o)
  where
    erase = first (const ())

-- | The D6 status line, e.g.
-- @archive ok · reminders 3/5 (2 failed: 404) · remove-reminders skipped: offline@;
-- 'Nothing' when every step went through cleanly.
batchStatus :: [BatchStep] -> [(BatchStep, StepOutcome () Text)] -> Maybe Text
batchStatus planned ran
  | all (clean . snd) ran = Nothing
  | otherwise = Just (T.intercalate " · " (map ranText ran <> map skippedText (drop (length ran) planned)))
  where
    clean o = null (o ^. soFailed) && isNothing (o ^. soAborted)
    abortReason = fromMaybe "" (listToMaybe (mapMaybe (view soAborted . snd) ran))
    skippedText step = stepName step <> " skipped: " <> abortReason
    ranText (step, o)
      | clean o = stepName step <> " ok"
      | otherwise = stepName step <> " " <> count (o ^. soOk) <> "/" <> count (total o) <> " (" <> T.intercalate "; " (problems o) <> ")"
    total o = (o ^. soOk) <> map fst (o ^. soFailed) <> (o ^. soSkipped)
    problems o =
      [count fs <> " failed: " <> T.intercalate ", " (nub (map (failureText . snd) fs)) | let fs = o ^. soFailed, not (null fs)]
        <> ["aborted: " <> e | Just e <- [o ^. soAborted]]
    count :: [x] -> Text
    count = T.pack . show . length
    failureText ResultFalse = "result: false"
    failureText (CallFailed err) = err

stepName :: BatchStep -> Text
stepName = \case
  StepArchive -> "archive"
  StepSetReminders -> "reminders"
  StepRemoveReminders -> "remove-reminders"

-- | Only what the server accepted changes locally; failed and skipped items
-- keep their flags.
applyStep :: StepResult -> Model -> Model
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
targetItem :: Model -> Target -> Maybe BookmarkItem
targetItem s = \case
  Selected -> focusedItem s
  ById bid -> s ^? mContents . ix bid . _2
