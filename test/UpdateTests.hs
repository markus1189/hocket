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
    [ testCase "every intent a producer can post today is handled" $
        for_ migrated $
          \i -> (i, isJust (update (Intent i) twoItems)) @?= (i, True),
      testCase "every key the shell routes to update is handled, in every flag state" $
        for_ routedKeys $ \c -> for_ [None, ToBeArchived, ToBeReminded t0, ReminderToBeRemoved] $ \flag -> do
          let s = twoItems & setPendingAction (BookmarkItemId "a") flag
          for_ (concat (keyIntents now0 (Vty.EvKey (keyOf c) []) s)) $ \i ->
            (c, i, isJust (update (Intent i) s)) @?= (c, i, True),
      testCase "every agent write routed to update is handled" $
        for_ routedWrites $
          \w -> (w, isJust (update (Intent (toIntent now0 w)) twoItems)) @?= (w, True),
      testCase "Quit halts and changes nothing else" $
        snd <$> update (Intent Quit) emptyState @?= Just [Halt],
      testCase "Open: firefox on the focused item, or the one named" $ do
        effects (Intent (Open Selected)) @?= Just [OpenUrl Firefox "https://example.com/a"]
        effects (Intent (Open (ById (BookmarkItemId "b")))) @?= Just [OpenUrl Firefox "https://example.com/b"],
      testCase "an unknown id or no focus opens nothing" $ do
        effects (Intent (Open (ById (BookmarkItemId "zz")))) @?= Just []
        snd <$> update (Intent (Open Selected)) emptyState @?= Just [],
      testCase "EditInRaindrop: Raindrop's edit page with the desktop default" $
        effects (Intent (EditInRaindrop Selected)) @?= Just [OpenUrl XdgOpen "https://app.raindrop.io/my/-1/item/a/edit"],
      testCase "CopyUrl: the raw link; the clipboard adapter cleans it" $
        effects (Intent (CopyUrl (ById (BookmarkItemId "b")))) @?= Just [CopyText "https://example.com/b"],
      testCase "OpenAndFlag: flags archive and opens, an unknown id does neither" $ do
        let Just (s', fx) = update (Intent (OpenAndFlag (BookmarkItemId "b"))) twoItems
        (fx, s' ^? hsContents . ix (BookmarkItemId "b") . _1) @?= ([OpenUrl Firefox "https://example.com/b"], Just ToBeArchived)
        fmap (\(s'', fx') -> (fx', s'' ^. hsContents == twoItems ^. hsContents)) (update (Intent (OpenAndFlag (BookmarkItemId "zz"))) twoItems)
          @?= Just ([], True),
      testCase "SetFlag sets the agent's flag as is, with no move" $
        for_ [(FlagArchive, ToBeArchived), (FlagRemoveReminder, ReminderToBeRemoved), (FlagNone, None)] $ \(act, flag) -> do
          let Just (s', fx) = update (Intent (SetFlag (BookmarkItemId "b") act now0)) twoItems
          (act, fx, flagOf "b" s', s' ^. itemList . L.listSelectedL) @?= (act, [], Just flag, twoItems ^. itemList . L.listSelectedL),
      testCase "SetFlag reminder: 07:00 next local day at the request's offset" $ do
        -- The evening before the Europe/Berlin change, still at CEST (+02:00).
        let evening = Now (read "2026-10-24 21:00:00 UTC") (minutesToTimeZone 120)
            Just (s', _) = update (Intent (SetFlag (BookmarkItemId "b") FlagReminder evening)) twoItems
        flagOf "b" s' @?= Just (ToBeReminded (read "2026-10-25 05:00:00 UTC")),
      testCase "SetFlag on an unknown id changes nothing" $
        fmap ((^. hsContents) . fst) (update (Intent (SetFlag (BookmarkItemId "zz") FlagArchive now0)) twoItems)
          @?= Just (twoItems ^. hsContents),
      testCase "ClearAllFlags and FlagAllArchive set every flag" $ do
        let flagged = twoItems & setPendingAction (BookmarkItemId "a") ReminderToBeRemoved
        (map fst . Map.elems . (^. hsContents) . fst <$> update (Intent ClearAllFlags) flagged) @?= Just [None, None]
        (map fst . Map.elems . (^. hsContents) . fst <$> update (Intent FlagAllArchive) flagged) @?= Just [ToBeArchived, ToBeArchived],
      testCase "Select moves to a visible item; a hidden or unknown one changes nothing" $ do
        let selected s' = s' ^. itemList . L.listSelectedL
        (selected . fst <$> update (Intent (Select (BookmarkItemId "b"))) twoItems) @?= Just (Just 1)
        -- Items with a reminder are hidden unless future reminders are shown.
        let hidden = syncForRender (twoItems & hsContents . ix (BookmarkItemId "b") . _2 . biReminder ?~ t0)
        (selected . fst <$> update (Intent (Select (BookmarkItemId "b"))) hidden) @?= Just (selected hidden)
        (selected . fst <$> update (Intent (Select (BookmarkItemId "zz"))) twoItems) @?= Just (Just 0),
      testCase "SetFilter replaces the query, locks the editor and refilters the list" $ do
        let Just (s', fx) = update (Intent (SetFilter "title b")) (twoItems & enterFilterMode)
        (fx, s' ^. hsFilterQuery, s' ^. hsFilterActive, visibleIds s') @?= ([], "title b", False, [BookmarkItemId "b"]),
      testCase "the video and future-reminder settings refilter the list" $ do
        let setting i f = f . fst <$> update (Intent i) twoItems
        setting (SetVideoFilter HideVideos) (^. hsVideoFilter) @?= Just HideVideos
        setting ToggleVideoOnly (^. hsVideoFilter) @?= Just ShowOnlyVideos
        setting ToggleHideVideos (^. hsVideoFilter) @?= Just HideVideos
        setting ToggleFutureReminders (^. hsShowFutureReminders) @?= Just True
        setting (SetShowFutureReminders True) (^. hsShowFutureReminders) @?= Just True
        -- Only-videos hides both items, and the list shows it right away.
        setting ToggleVideoOnly visibleIds @?= Just [],
      testCase "SetStatus shows the text as given" $
        (^. hsStatus) . fst <$> update (Intent (SetStatus "agent: hi")) twoItems @?= Just (Just "agent: hi"),
      testCase "AgentClients and AgentServerDied go to the header" $ do
        (^. hsAgentClients) . fst <$> update (AgentClients 2) twoItems @?= Just 2
        ((\s' -> (s' ^. hsAgentError, s' ^. hsStatus)) . fst <$> update (AgentServerDied "bind: busy") twoItems)
          @?= Just (Just "bind: busy", Just "agent socket failed: bind: busy"),
      testCase "Refresh takes the slot and starts the first fetch from Unsorted" $ do
        let Just (s', fx) = update (Intent Refresh) emptyState
        (fx, asyncOpRunning s', s' ^. hsStatus) @?= ([StartFetch (planSync Nothing)], True, Just "fetching"),
      testCase "Refresh after a sync fetches what changed since a day before it" $ do
        let synced = emptyState & hsLastUpdated ?~ 1761393600 -- 2025-10-25 12:00 UTC
            Just (s', fx) = update (Intent Refresh) synced
        (fx, s' ^. hsStatus) @?= ([StartFetch (planSync (Just 1761393600))], Just "fetching since: 2025-10-24"),
      testCase "Refresh while an async op runs is dropped" $ do
        let Just busy = tryAcquireAsyncOp OpExecuteBatch emptyState
        fmap (\(s', fx) -> (fx, s' ^. hsAsyncOp, s' ^. hsStatus)) (update (Intent Refresh) busy) @?= Just ([], Just OpExecuteBatch, Nothing),
      testCase "FetchCompleted merges, clears the status and frees the slot" $ do
        let Just (fetching, _) = update (Intent Refresh) emptyState
            Just (s', fx) = update (FetchCompleted False [BookmarkItemBatch 5 [item "n" (-1)] 1]) fetching
        (fx, asyncOpRunning s', s' ^. hsStatus, Map.keys (s' ^. hsContents)) @?= ([], False, Nothing, [BookmarkItemId "n"]),
      testCase "FetchFailed frees the slot and names the error" $ do
        let Just (fetching, _) = update (Intent Refresh) emptyState
            failed err = (\(s', fx) -> (fx, asyncOpRunning s', s' ^. hsStatus)) <$> update (FetchFailed err) fetching
        failed (Just "offline") @?= Just ([], False, Just "failed: offline")
        failed Nothing @?= Just ([], False, Just "failed<no err>"),
      testCase "Execute takes the slot and starts the planned batch" $ do
        let flagged = twoItems & setPendingAction (BookmarkItemId "a") ToBeArchived & setPendingAction (BookmarkItemId "b") (ToBeReminded t0)
            Just (s', fx) = update (Intent Execute) flagged
        (fx, s' ^. hsAsyncOp) @?= ([StartBatch (BatchPlan [BookmarkItemId "a"] [(BookmarkItemId "b", t0)] [])], Just OpExecuteBatch),
      testCase "Execute with nothing flagged, or while an async op runs, does nothing" $ do
        fmap (\(s', fx) -> (fx, s' ^. hsAsyncOp)) (update (Intent Execute) twoItems) @?= Just ([], Nothing)
        let Just fetching = tryAcquireAsyncOp OpFetchItems (twoItems & setPendingAction (BookmarkItemId "a") ToBeArchived)
        fmap (\(s', fx) -> (fx, s' ^. hsAsyncOp)) (update (Intent Execute) fetching) @?= Just ([], Just OpFetchItems),
      testCase "BatchStepStarted names the step in the status line" $
        for_ [(StepArchive, "archiving"), (StepSetReminders, "setting reminders"), (StepRemoveReminders, "removing reminders")] $ \(step, label) ->
          (^. hsStatus) . fst <$> update (BatchStepStarted step) twoItems @?= Just (Just label),
      testCase "StepDone applies only what the server accepted" $ do
        let flagged = twoItems & setPendingAction (BookmarkItemId "a") ToBeArchived & setPendingAction (BookmarkItemId "b") ToBeArchived
            Just (s', fx) = update (StepDone (Archived (outcome [("a", Right True), ("b", Left "404")]))) flagged
        (fx, Map.keys (s' ^. hsContents), flagOf "b" s') @?= ([], [BookmarkItemId "b"], Just ToBeArchived),
      testCase "BatchFinished frees the slot and shows the summary, or clears the status" $ do
        let Just running = tryAcquireAsyncOp OpExecuteBatch (twoItems & hsStatus ?~ "archiving")
            finished summary = (\(s', fx) -> (fx, s' ^. hsAsyncOp, s' ^. hsStatus)) <$> update (BatchFinished summary) running
        finished (Just "archive 0/1 (1 failed: 404)") @?= Just ([], Nothing, Just "archive 0/1 (1 failed: 404)")
        finished Nothing @?= Just ([], Nothing, Nothing),
      testCase "SystemFailed and Copied go to the status line" $ do
        (^. hsStatus) . fst <$> update (SystemFailed "no firefox") emptyState @?= Just (Just "no firefox")
        (^. hsStatus) . fst <$> update (Copied "https://example.com/a") emptyState @?= Just (Just "Copied: https://example.com/a")
    ]
  where
    effects m = snd <$> update m twoItems

-- | The intents of the groups moved so far; grows with every Phase 3 step.
migrated :: [Intent]
migrated =
  [ Quit,
    Open Selected,
    Open (ById (BookmarkItemId "b")),
    EditInRaindrop Selected,
    CopyUrl Selected,
    OpenAndFlag (BookmarkItemId "b"),
    SetArchive Selected,
    ToggleArchive Selected,
    ToggleReminder Selected now0,
    Unflag Selected,
    SetFlag (BookmarkItemId "b") FlagReminder now0,
    ClearAllFlags,
    FlagAllArchive,
    Select (BookmarkItemId "b"),
    SetFilter "x",
    SetVideoFilter ShowOnlyVideos,
    ToggleVideoOnly,
    ToggleHideVideos,
    ToggleFutureReminders,
    SetShowFutureReminders True,
    SetStatus "agent: hi",
    Refresh,
    Execute
  ]

-- | The keys hocket.hs sends through keyIntents and update so far.
routedKeys :: [Char]
routedKeys = "asu\n eyUQSvVrX"

keyOf :: Char -> Vty.Key
keyOf '\n' = Vty.KEnter
keyOf c = Vty.KChar c

-- | The agent writes AgentServer.toEvent sends through toIntent so far.
routedWrites :: [WriteCmd]
routedWrites =
  [ CmdSetFlag (BookmarkItemId "b") FlagArchive,
    CmdClearFlags,
    CmdFlagAllArchive,
    CmdOpenItem (BookmarkItemId "b"),
    CmdOpenAndFlag (BookmarkItemId "b"),
    CmdSetFilter "x",
    CmdSetVideoFilter HideVideos,
    CmdSetShowFutureReminders True,
    CmdSelectItem (BookmarkItemId "b"),
    CmdSetStatus "hi",
    CmdRefresh,
    CmdExecute
  ]

now0 :: Now
now0 = Now (read "2026-11-01 12:00:00 UTC") (minutesToTimeZone 60)

t0 :: UTCTime
t0 = read "2026-11-02 06:00:00 UTC"

visibleIds :: HocketState -> [BookmarkItemId]
visibleIds s = s ^.. itemList . L.listElementsL . traverse . biId

-- | A step outcome from a script; errors starting with "5" are global.
outcome :: [(Text, Either Text Bool)] -> StepOutcome BookmarkItemId Text
outcome script = runIdentity (runItems scope (\(BookmarkItemId i) -> pure (fromMaybe (Right True) (lookup i script))) [BookmarkItemId i | (i, _) <- script])
  where
    scope e = if "5" `T.isPrefixOf` e then Global else ItemScoped

flagOf :: Text -> HocketState -> Maybe PendingAction
flagOf i s = s ^? hsContents . ix (BookmarkItemId i) . _1

-- | "a" sorts first by its later creation date, so it holds the focus.
twoItems :: HocketState
twoItems = syncForRender (insertItems [item "a" (-1) & biCreated .~ read "2030-01-01 00:00:00 UTC", item "b" (-1)] emptyState)
