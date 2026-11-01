-- | The TUI core as one pure function. Until the end of Phase 3 it only
-- covers the handler groups that have moved; 'Nothing' means a message is
-- still handled the old way (its producer still posts the old event).
module Network.Bookmark.App.Update (update) where

import Network.Bookmark.App.Effect
import Network.Bookmark.App.Msg
import Network.Bookmark.Ui.State (HocketState)

update :: Msg -> HocketState -> Maybe (HocketState, [Effect])
update (Intent Quit) s = Just (s, [Halt])
update _ _ = Nothing
