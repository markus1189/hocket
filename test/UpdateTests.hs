{-# LANGUAGE OverloadedStrings #-}

module UpdateTests (updateTests) where

import CharacterizationTests (emptyState, item)
import Control.Lens (ix, (&), (.~), (^.), (^?), _1)
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Events (HocketEvent (HocketMsg))
import Keys (itemKeyEvents)
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
      testCase "every message a key posts is handled, for every key and flag" $
        for_ [minBound .. maxBound] $ \k -> for_ [None, ToBeArchived, ReminderToBeRemoved] $ \flag ->
          for_ [m | HocketMsg m <- itemKeyEvents k (twoItems & setPendingAction (BookmarkItemId "a") flag)] $ \m ->
            (k, m, isJust (update m twoItems)) @?= (k, m, True),
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
      testCase "SystemFailed and Copied go to the status line" $ do
        (^. hsStatus) . fst <$> update (SystemFailed "no firefox") emptyState @?= Just (Just "no firefox")
        (^. hsStatus) . fst <$> update (Copied "https://example.com/a") emptyState @?= Just (Just "Copied: https://example.com/a")
    ]
  where
    effects m = snd <$> update m twoItems

-- | The intents of the groups moved so far; grows with every Phase 3 step.
migrated :: [Intent]
migrated =
  [Quit, Open Selected, Open (ById (BookmarkItemId "b")), EditInRaindrop Selected, CopyUrl Selected, OpenAndFlag (BookmarkItemId "b")]

-- | "a" sorts first by its later creation date, so it holds the focus.
twoItems :: HocketState
twoItems = syncForRender (insertItems [item "a" (-1) & biCreated .~ read "2030-01-01 00:00:00 UTC", item "b" (-1)] emptyState)
