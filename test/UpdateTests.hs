{-# LANGUAGE OverloadedStrings #-}

module UpdateTests (updateTests) where

import qualified Brick.Widgets.List as L
import CharacterizationTests (emptyState, item)
import Control.Lens (ix, (&), (.~), (?~), (^.), (^..), (^?), _1, _2)
import Data.Foldable (for_)
import Data.Functor.Identity (runIdentity)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.LocalTime (minutesToTimeZone)
import qualified Graphics.Vty as Vty
import Keys (keyIntents)
import Network.Bookmark.Agent.Intent (toIntent)
import Network.Bookmark.Agent.Protocol (FlagAction (..), WriteCmd (..))
import Network.Bookmark.App.Effect
import Network.Bookmark.App.Msg
import Network.Bookmark.App.Nav (Nav (..))
import Network.Bookmark.App.Plan (BatchPlan (..))
import Network.Bookmark.App.Update
import Network.Bookmark.Batch (ErrorScope (..), StepOutcome, runItems)
import Network.Bookmark.Types
import Network.Bookmark.Ui.State
import Network.Bookmark.Ui.Sync (planSync)
import Test.Tasty
import Test.Tasty.HUnit

-- | One case per Intent constructor once Phase 3 is done (plan §6).
updateTests :: TestTree
updateTests =
  testGroup
    "update"
    [ testCase "Quit halts and changes nothing else" $
        snd (update (Intent Quit) emptyState) @?= [Halt],
      testCase "Open: firefox on the focused item, or the one named" $ do
        effects (Intent (Open Selected)) @?= [OpenUrl Firefox "https://example.com/a"]
        effects (Intent (Open (ById (BookmarkItemId "b")))) @?= [OpenUrl Firefox "https://example.com/b"],
      testCase "an unknown id or no focus opens nothing" $ do
        effects (Intent (Open (ById (BookmarkItemId "zz")))) @?= []
        snd (update (Intent (Open Selected)) emptyState) @?= [],
      testCase "EditInRaindrop: Raindrop's edit page with the desktop default" $
        effects (Intent (EditInRaindrop Selected)) @?= [OpenUrl XdgOpen "https://app.raindrop.io/my/-1/item/a/edit"],
      testCase "CopyUrl: the raw link; the clipboard adapter cleans it" $
        effects (Intent (CopyUrl (ById (BookmarkItemId "b")))) @?= [CopyText "https://example.com/b"],
      testCase "OpenAndFlag: flags archive and opens, an unknown id does neither" $ do
        let (s', fx) = update (Intent (OpenAndFlag (BookmarkItemId "b"))) twoItems
        (fx, s' ^? hsContents . ix (BookmarkItemId "b") . _1) @?= ([OpenUrl Firefox "https://example.com/b"], Just ToBeArchived)
        let (s'', fx') = update (Intent (OpenAndFlag (BookmarkItemId "zz"))) twoItems
        (fx', s'' ^. hsContents == twoItems ^. hsContents) @?= ([], True),
      testCase "SetFlag sets the agent's flag as is, with no move" $
        for_ [(FlagArchive, ToBeArchived), (FlagRemoveReminder, ReminderToBeRemoved), (FlagNone, None)] $ \(act, flag) -> do
          let (s', fx) = update (Intent (SetFlag (BookmarkItemId "b") act now0)) twoItems
          (act, fx, flagOf "b" s', s' ^. hsSelected) @?= (act, [], Just flag, twoItems ^. hsSelected),
      testCase "SetFlag reminder: 07:00 next local day at the request's offset" $ do
        -- The evening before the Europe/Berlin change, still at CEST (+02:00).
        let evening = Now (read "2026-10-24 21:00:00 UTC") (minutesToTimeZone 120)
            (s', _) = update (Intent (SetFlag (BookmarkItemId "b") FlagReminder evening)) twoItems
        flagOf "b" s' @?= Just (ToBeReminded (read "2026-10-25 05:00:00 UTC")),
      testCase "SetFlag on an unknown id changes nothing" $
        (after (Intent (SetFlag (BookmarkItemId "zz") FlagArchive now0)) twoItems ^. hsContents) @?= (twoItems ^. hsContents),
      testCase "ClearAllFlags and FlagAllArchive set every flag" $ do
        let flagged = twoItems & setPendingAction (BookmarkItemId "a") ReminderToBeRemoved
            flags i = map fst (Map.elems (after (Intent i) flagged ^. hsContents))
        flags ClearAllFlags @?= [None, None]
        flags FlagAllArchive @?= [ToBeArchived, ToBeArchived],
      testCase "Select moves to a visible item; a hidden or unknown one changes nothing" $ do
        let selected s' = s' ^. hsSelected
        selected (after (Intent (Select (BookmarkItemId "b"))) twoItems) @?= Just (BookmarkItemId "b")
        -- Items with a reminder are hidden unless future reminders are shown.
        let hidden = settle (twoItems & hsContents . ix (BookmarkItemId "b") . _2 . biReminder ?~ t0)
        selected (after (Intent (Select (BookmarkItemId "b"))) hidden) @?= selected hidden
        selected (after (Intent (Select (BookmarkItemId "zz"))) twoItems) @?= Just (BookmarkItemId "a"),
      testCase "SetFilter replaces the query, locks the editor and refilters the list" $ do
        let (s', fx) = update (Intent (SetFilter "title b")) (twoItems & enterFilterMode)
        (fx, s' ^. hsFilterQuery, s' ^. hsFilterActive, visibleIds s') @?= ([], "title b", False, [BookmarkItemId "b"]),
      testCase "the video and future-reminder settings refilter the list" $ do
        let setting i f = f (after (Intent i) twoItems)
        setting (SetVideoFilter HideVideos) (^. hsVideoFilter) @?= HideVideos
        setting ToggleVideoOnly (^. hsVideoFilter) @?= ShowOnlyVideos
        setting ToggleHideVideos (^. hsVideoFilter) @?= HideVideos
        setting ToggleFutureReminders (^. hsShowFutureReminders) @?= True
        setting (SetShowFutureReminders True) (^. hsShowFutureReminders) @?= True
        -- Only-videos hides both items, and the list shows it right away.
        setting ToggleVideoOnly visibleIds @?= [],
      testCase "SetStatus shows the text as given" $
        after (Intent (SetStatus "agent: hi")) twoItems ^. hsStatus @?= Just "agent: hi",
      testCase "AgentClients and AgentServerDied go to the header" $ do
        after (AgentClients 2) twoItems ^. hsAgentClients @?= 2
        let died = after (AgentServerDied "bind: busy") twoItems
        (died ^. hsAgentError, died ^. hsStatus) @?= (Just "bind: busy", Just "agent socket failed: bind: busy"),
      testCase "Refresh takes the slot and starts the first fetch from Unsorted" $ do
        let (s', fx) = update (Intent Refresh) emptyState
        (fx, asyncOpRunning s', s' ^. hsStatus) @?= ([StartFetch (planSync Nothing)], True, Just "fetching"),
      testCase "Refresh after a sync fetches what changed since a day before it" $ do
        let synced = emptyState & hsLastUpdated ?~ 1761393600 -- 2025-10-25 12:00 UTC
            (s', fx) = update (Intent Refresh) synced
        (fx, s' ^. hsStatus) @?= ([StartFetch (planSync (Just 1761393600))], Just "fetching since: 2025-10-24"),
      testCase "Refresh while an async op runs is dropped" $ do
        let Just busy = tryAcquireAsyncOp OpExecuteBatch emptyState
            (s', fx) = update (Intent Refresh) busy
        (fx, s' ^. hsAsyncOp, s' ^. hsStatus) @?= ([], Just OpExecuteBatch, Nothing),
      testCase "FetchCompleted merges, clears the status and frees the slot" $ do
        let fetching = after (Intent Refresh) emptyState
            (s', fx) = update (FetchCompleted False [BookmarkItemBatch 5 [item "n" (-1)] 1]) fetching
        (fx, asyncOpRunning s', s' ^. hsStatus, Map.keys (s' ^. hsContents)) @?= ([], False, Nothing, [BookmarkItemId "n"]),
      testCase "FetchFailed frees the slot and names the error" $ do
        let fetching = after (Intent Refresh) emptyState
            failed err = (\(s', fx) -> (fx, asyncOpRunning s', s' ^. hsStatus)) (update (FetchFailed err) fetching)
        failed (Just "offline") @?= ([], False, Just "failed: offline")
        failed Nothing @?= ([], False, Just "failed<no err>"),
      testCase "Execute takes the slot and starts the planned batch" $ do
        let flagged = twoItems & setPendingAction (BookmarkItemId "a") ToBeArchived & setPendingAction (BookmarkItemId "b") (ToBeReminded t0)
            (s', fx) = update (Intent Execute) flagged
        (fx, s' ^. hsAsyncOp) @?= ([StartBatch (BatchPlan [BookmarkItemId "a"] [(BookmarkItemId "b", t0)] [])], Just OpExecuteBatch),
      testCase "Execute with nothing flagged, or while an async op runs, does nothing" $ do
        let executed s = (\(s', fx) -> (fx, s' ^. hsAsyncOp)) (update (Intent Execute) s)
            Just fetching = tryAcquireAsyncOp OpFetchItems (twoItems & setPendingAction (BookmarkItemId "a") ToBeArchived)
        executed twoItems @?= ([], Nothing)
        executed fetching @?= ([], Just OpFetchItems),
      testCase "BatchStepStarted names the step in the status line" $
        for_ [(StepArchive, "archiving"), (StepSetReminders, "setting reminders"), (StepRemoveReminders, "removing reminders")] $ \(step, label) ->
          after (BatchStepStarted step) twoItems ^. hsStatus @?= Just label,
      testCase "StepDone applies only what the server accepted" $ do
        let flagged = twoItems & setPendingAction (BookmarkItemId "a") ToBeArchived & setPendingAction (BookmarkItemId "b") ToBeArchived
            (s', fx) = update (StepDone (Archived (outcome [("a", Right True), ("b", Left "404")]))) flagged
        (fx, Map.keys (s' ^. hsContents), flagOf "b" s') @?= ([], [BookmarkItemId "b"], Just ToBeArchived),
      testCase "BatchFinished frees the slot and shows the summary, or clears the status" $ do
        let Just running = tryAcquireAsyncOp OpExecuteBatch (twoItems & hsStatus ?~ "archiving")
            finished summary = (\(s', fx) -> (fx, s' ^. hsAsyncOp, s' ^. hsStatus)) (update (BatchFinished summary) running)
        finished (Just "archive 0/1 (1 failed: 404)") @?= ([], Nothing, Just "archive 0/1 (1 failed: 404)")
        finished Nothing @?= ([], Nothing, Nothing),
      testCase "Navigate moves the selection by id over what is visible" $ do
        let five = settle (insertItems [item i (-1) & biCreated .~ read ("2026-01-0" <> show n <> " 12:00:00 UTC") | (i, n) <- zip ["a", "b", "c", "d", "e"] [9 :: Int, 8, 7, 6, 5]] emptyState)
            moved h n s' = after (Intent (Navigate h n)) s' ^. hsSelected
            onB = five & hsSelected ?~ BookmarkItemId "b"
        moved 3 Down five @?= Just (BookmarkItemId "b")
        moved 3 Up five @?= Just (BookmarkItemId "a")
        moved 3 Bottom five @?= Just (BookmarkItemId "e")
        moved 3 (Page 1) onB @?= Just (BookmarkItemId "e")
        -- Nothing drawn yet: a page move is no move.
        moved 0 (Page 1) onB @?= Just (BookmarkItemId "b")
        let flaggedD = onB & setPendingAction (BookmarkItemId "d") ToBeArchived
        moved 3 NextFlagged flaggedD @?= Just (BookmarkItemId "d"),
      testCase "the filter editor: enter, type, lock, cancel" $ do
        let editing = after (Intent FilterEnter) twoItems
            typed = after (Intent (FilterText "title b")) editing
        (editing ^. hsFilterActive, editing ^. hsFilterQuery) @?= (True, "")
        -- Filters as you type, and stays in the editor.
        (typed ^. hsFilterActive, visibleIds typed) @?= (True, [BookmarkItemId "b"])
        let locked = after (Intent FilterLock) typed
        (locked ^. hsFilterActive, locked ^. hsFilterQuery, visibleIds locked) @?= (False, "title b", [BookmarkItemId "b"])
        let cancelled = after (Intent FilterCancel) typed
        (cancelled ^. hsFilterActive, cancelled ^. hsFilterQuery, length (visibleIds cancelled)) @?= (False, "", 2),
      testCase "SystemFailed and Copied go to the status line" $ do
        after (SystemFailed "no firefox") emptyState ^. hsStatus @?= Just "no firefox"
        after (Copied "https://example.com/a") emptyState ^. hsStatus @?= Just "Copied: https://example.com/a"
    ]
  where
    effects m = snd (update m twoItems)
    after m = fst . update m

now0 :: Now
now0 = Now (read "2026-11-01 12:00:00 UTC") (minutesToTimeZone 60)

t0 :: UTCTime
t0 = read "2026-11-02 06:00:00 UTC"

visibleIds :: HocketState -> [BookmarkItemId]
visibleIds s = s ^.. hsVisible . traverse . biId

-- | A step outcome from a script; errors starting with "5" are global.
outcome :: [(Text, Either Text Bool)] -> StepOutcome BookmarkItemId Text
outcome script = runIdentity (runItems scope (\(BookmarkItemId i) -> pure (fromMaybe (Right True) (lookup i script))) [BookmarkItemId i | (i, _) <- script])
  where
    scope e = if "5" `T.isPrefixOf` e then Global else ItemScoped

flagOf :: Text -> HocketState -> Maybe PendingAction
flagOf i s = s ^? hsContents . ix (BookmarkItemId i) . _1

-- | "a" sorts first by its later creation date, so it holds the focus.
twoItems :: HocketState
twoItems = settle (insertItems [item "a" (-1) & biCreated .~ read "2030-01-01 00:00:00 UTC", item "b" (-1)] emptyState)
