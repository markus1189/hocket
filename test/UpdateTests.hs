module UpdateTests (updateTests) where

import CharacterizationTests (emptyState)
import Network.Bookmark.App.Effect
import Network.Bookmark.App.Msg
import Network.Bookmark.App.Update
import Test.Tasty
import Test.Tasty.HUnit

-- | One case per Intent constructor once Phase 3 is done (plan §6).
updateTests :: TestTree
updateTests =
  testGroup
    "update"
    [ testCase "Quit halts and changes nothing else" $
        snd <$> update (Intent Quit) emptyState @?= Just [Halt]
    ]
