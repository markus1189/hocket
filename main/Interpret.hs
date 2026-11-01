{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The shell's only way from an 'Effect' to IO (plan §4.3).
module Interpret
  ( Env (..),
    interpret,
  )
where

import Brick.BChan (BChan)
import Control.Exception (SomeException, try)
import qualified Data.Text as T
import Events (HocketEvent)
import Network.Bookmark.App.Effect
import Network.Bookmark.App.Msg
import Network.Bookmark.Port (RaindropPort)
import Network.Bookmark.Types (URL (..))
import System (browseItem, cleanUrl, copyToClipboard)

-- | Where to post events, and Raindrop.
data Env = Env {envEvents :: !(BChan HocketEvent), envPort :: !RaindropPort}

-- | Run one effect. What the TUI waits for (browser, clipboard) answers
-- with messages for the same event, so the status line is as quick as the
-- old handlers'.
interpret :: Env -> Effect -> IO [Msg]
interpret _ = \case
  -- The shell halts the event loop itself.
  Halt -> pure []
  OpenUrl opener url -> failed <$> try @SomeException (browseItem (program opener) (URL (T.unpack url)))
  CopyText link -> do
    let url = cleanUrl (T.unpack link)
    r <- try @SomeException (copyToClipboard url)
    pure [either (SystemFailed . ("Copy failed: " <>) . T.pack . show) (const (Copied (T.pack url))) r]
  where
    failed = either (\e -> [SystemFailed (T.pack (show e))]) (const [])
    program = \case
      Firefox -> "firefox"
      XdgOpen -> "xdg-open"
