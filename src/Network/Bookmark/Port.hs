{-# LANGUAGE RankNTypes #-}

-- | The one way the app reaches Raindrop (plan §11 #16): any
-- 'BookmarkRequest', answered in IO. The Servant adapter and the fixture
-- store both implement it.
module Network.Bookmark.Port (RaindropPort (..)) where

import Network.Bookmark.Types (BookmarkRequest)

-- | Failures are thrown as 'Servant.Client.ClientError'; a @result: false@
-- answer comes back as 'False'.
newtype RaindropPort = RaindropPort {runRaindrop :: forall a. BookmarkRequest a -> IO a}
