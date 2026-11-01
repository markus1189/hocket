module Events (HocketEvent (..)) where

import Network.Bookmark.App.Msg (Msg)

-- | Brick's custom event: every message goes through
-- 'Network.Bookmark.App.Update.update' now.
newtype HocketEvent = HocketMsg Msg
  deriving (Show, Eq)
