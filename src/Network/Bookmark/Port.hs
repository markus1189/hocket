{-# LANGUAGE RankNTypes #-}

-- | The one way the app reaches Raindrop (plan §11 #16): any
-- 'BookmarkRequest', answered in IO. The Servant adapter and the fixture
-- store both implement it.
module Network.Bookmark.Port (RaindropPort (..), SystemPort (..)) where

import Network.Bookmark.Types (BookmarkRequest, URL)

-- | Failures are thrown as 'Servant.Client.ClientError'; a @result: false@
-- answer comes back as 'False'.
newtype RaindropPort = RaindropPort {runRaindrop :: forall a. BookmarkRequest a -> IO a}

-- | The desktop (plan Q4), so tests and the headless loop never start a
-- browser or touch the clipboard.
data SystemPort = SystemPort
  { -- | The program, and the URL as Raindrop has it.
    sysOpenUrl :: FilePath -> URL -> IO (),
    sysCopy :: String -> IO ()
  }
