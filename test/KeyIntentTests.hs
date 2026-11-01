module KeyIntentTests (keyIntentTests) where

import Control.Lens ((&), (.~))
import Data.Foldable (for_)
import Data.Time (utc)
import Graphics.Vty (Event (EvKey), Key (..), Modifier (MCtrl))
import Keys (keyIntents)
import Network.Bookmark.App.Msg
import Network.Bookmark.App.Nav (Nav (..))
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
    (key 'Q', [Quit]),
    (key '/', [FilterEnter]),
    (key 'k', [Navigate height Up]),
    (EvKey KUp [], [Navigate height Up]),
    (key 'j', [Navigate height Down]),
    (EvKey KDown [], [Navigate height Down]),
    (key 'g', [Navigate height Top]),
    (EvKey KHome [], [Navigate height Top]),
    (key 'G', [Navigate height Bottom]),
    (EvKey KEnd [], [Navigate height Bottom]),
    (EvKey (KChar 'f') [MCtrl], [Navigate height (Page 1)]),
    (EvKey KPageDown [], [Navigate height (Page 1)]),
    (EvKey (KChar 'b') [MCtrl], [Navigate height (Page (-1))]),
    (EvKey KPageUp [], [Navigate height (Page (-1))]),
    (EvKey (KChar 'd') [MCtrl], [Navigate height (HalfPage 1)]),
    (EvKey (KChar 'u') [MCtrl], [Navigate height (HalfPage (-1))]),
    (key 'J', [Navigate height NextFlagged]),
    (key 'K', [Navigate height PrevFlagged])
  ]

-- | The list viewport's rows, as the shell would pass them.
height :: Int
height = 7

keyIntentTests :: TestTree
keyIntentTests =
  testGroup
    "keyIntents"
    [ testCase "every action key yields its Intents" $
        for_ rows $
          \(e, is) -> (e, keyIntents now height e initialState) @?= (e, Just is),
      testCase "unknown keys stay with the shell" $
        for_ [key 'x', EvKey (KChar 'j') [MCtrl]] $ \e ->
          (e, keyIntents now height e initialState) @?= (e, Nothing),
      testCase "the keymap is not asked while the filter editor has focus" $
        for_ (map fst rows) $
          \e -> (e, keyIntents now height e filtering) @?= (e, Nothing),
      testCase "Q typed into a filter is never Quit" $
        keyIntents now height (key 'Q') filtering @?= Nothing
    ]
