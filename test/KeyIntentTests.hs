module KeyIntentTests (keyIntentTests) where

import Control.Lens ((&), (.~))
import Data.Foldable (for_)
import Data.Time (utc)
import Graphics.Vty (Event (EvKey), Key (..))
import Keys (keyIntents)
import Network.Bookmark.App.Msg
import Network.Bookmark.Ui.State
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

now :: Now
now = Now (read "2026-11-01 10:00:00 UTC") utc

key :: Char -> Event
key c = EvKey (KChar c) []

filtering :: HocketState
filtering = initialState & hsFilterActive .~ True

rows :: [(Event, [Intent])]
rows =
  [ (key ' ', [Open Selected]),
    (EvKey KEnter [], [Open Selected, ToggleArchive Selected]),
    (key 'a', [SetArchive Selected]),
    (key 's', [ToggleReminder Selected now]),
    (key 'u', [Unflag Selected]),
    (key 'e', [EditInRaindrop Selected]),
    (key 'y', [CopyUrl Selected]),
    (key 'r', [Refresh]),
    (key 'X', [Execute]),
    (key 'U', [ClearAllFlags]),
    (key 'S', [ToggleFutureReminders]),
    (key 'v', [ToggleVideoOnly]),
    (key 'V', [ToggleHideVideos]),
    (key 'Q', [Quit])
  ]

keyIntentTests :: TestTree
keyIntentTests =
  testGroup
    "keyIntents"
    [ testCase "every action key yields its Intents" $
        for_ rows $
          \(e, is) -> (e, keyIntents now e initialState) @?= (e, Just is),
      testCase "navigation, J/K and / stay with the shell" $
        for_ ([key 'j', key 'k', key 'J', key 'K', key 'g', key 'G', key '/'] <> map (`EvKey` []) [KUp, KDown, KPageUp, KPageDown, KHome, KEnd]) $ \e ->
          (e, keyIntents now e initialState) @?= (e, Nothing),
      testCase "the keymap is not asked while the filter editor has focus" $
        for_ (map fst rows) $
          \e -> (e, keyIntents now e filtering) @?= (e, Nothing),
      testCase "Q typed into a filter is never Quit" $
        keyIntents now (key 'Q') filtering @?= Nothing
    ]
