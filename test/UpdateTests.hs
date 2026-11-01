{-# LANGUAGE OverloadedStrings #-}

module UpdateTests (updateTests) where

import qualified Brick.Widgets.List as L
import CharacterizationTests (emptyState, item)
import Control.Lens (ix, (&), (.~), (^.), (^?), _1)
import Data.Foldable (for_)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.LocalTime (minutesToTimeZone)
import qualified Graphics.Vty as Vty
import Keys (keyIntents)
import Network.Bookmark.Agent.Intent (toIntent)
import Network.Bookmark.Agent.Protocol (FlagAction (..), WriteCmd (..))
import Network.Bookmark.App.Effect
import Network.Bookmark.App.Msg
import Network.Bookmark.App.Update
import Network.Bookmark.Types
import Network.Bookmark.Ui.State
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
    FlagAllArchive
  ]

-- | The keys hocket.hs sends through keyIntents and update so far.
routedKeys :: [Char]
routedKeys = "asu\n eyUQ"

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
    CmdOpenAndFlag (BookmarkItemId "b")
  ]

now0 :: Now
now0 = Now (read "2026-11-01 12:00:00 UTC") (minutesToTimeZone 60)

t0 :: UTCTime
t0 = read "2026-11-02 06:00:00 UTC"

flagOf :: Text -> HocketState -> Maybe PendingAction
flagOf i s = s ^? hsContents . ix (BookmarkItemId i) . _1

-- | "a" sorts first by its later creation date, so it holds the focus.
twoItems :: HocketState
twoItems = syncForRender (insertItems [item "a" (-1) & biCreated .~ read "2030-01-01 00:00:00 UTC", item "b" (-1)] emptyState)
