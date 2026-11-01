{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The TUI core as one pure function. Until the end of Phase 3 it only
-- covers the handler groups that have moved; 'Nothing' means a message is
-- still handled the old way (its producer still posts the old event).
module Network.Bookmark.App.Update (update) where

import Control.Lens
import Network.Bookmark.App.Effect
import Network.Bookmark.App.Msg
import Network.Bookmark.Types
import Network.Bookmark.Ui.State

update :: Msg -> HocketState -> Maybe (HocketState, [Effect])
update msg s = case msg of
  Intent Quit -> Just (s, [Halt])
  Intent (Open t) -> Just (s, [OpenUrl Firefox (bit ^. biLink) | bit <- target t])
  Intent (EditInRaindrop t) -> Just (s, [OpenUrl XdgOpen (editUrl bit) | bit <- target t])
  Intent (CopyUrl t) -> Just (s, [CopyText (bit ^. biLink) | bit <- target t])
  Intent (OpenAndFlag bid) -> Just $ case s ^? hsContents . ix bid . _2 of
    Just bit -> (syncForRender (setPendingAction bid ToBeArchived s), [OpenUrl Firefox (bit ^. biLink)])
    Nothing -> (s, [])
  SystemFailed err -> Just (s & hsStatus ?~ err, [])
  Copied url -> Just (s & hsStatus ?~ ("Copied: " <> url), [])
  _ -> Nothing
  where
    target = maybe [] pure . targetItem s
    editUrl bit = "https://app.raindrop.io/my/-1/item/" <> bit ^. biId . _BookmarkItemId <> "/edit"

-- | Keys act on the focused item; an unknown id is no item.
targetItem :: HocketState -> Target -> Maybe BookmarkItem
targetItem s = \case
  Selected -> focusedItem s
  ById bid -> s ^? hsContents . ix bid . _2
