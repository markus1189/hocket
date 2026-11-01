{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The shell's only way from an 'Effect' to IO (plan §4.3).
module Interpret
  ( Env (..),
    interpret,
    copyCleaned,
  )
where

import Brick.BChan (BChan, writeBChan)
import Control.Concurrent.Async (async)
import Control.Exception (SomeException, try)
import Control.Lens (view, (^.))
import Control.Monad.Loops (unfoldrM)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Events (HocketEvent (HocketMsg))
import Network.Bookmark.App.Effect
import Network.Bookmark.App.Msg
import Network.Bookmark.Port (RaindropPort (..))
import Network.Bookmark.Types (BookmarkItemBatch (..), BookmarkRequest (RetrieveBookmarks), RaindropCollectionId, URL (..), biLastUpdate)
import Network.Bookmark.Ui.Sync (SyncKind (IncrementalAll), spCollection, spKind, spSearch)
import Servant.Client.Core (ClientError)
import System (browseItem, cleanUrl, copyToClipboard)
import Worker (errorMessageFromException, exceptionMessage, fetchResult, trySync)

-- | Where to post events, and Raindrop.
data Env = Env {envEvents :: !(BChan HocketEvent), envPort :: !RaindropPort}

-- | Run one effect. What the TUI waits for (browser, clipboard) answers
-- with messages for the same event, so the status line is as quick as the
-- old handlers'.
interpret :: Env -> Effect -> IO [Msg]
interpret env = \case
  -- The shell halts the event loop itself.
  Halt -> pure []
  OpenUrl opener url -> failed <$> try @SomeException (browseItem (program opener) (URL (T.unpack url)))
  StartFetch plan -> [] <$ async (fetch plan)
  CopyText link -> copyCleaned copyToClipboard link
  where
    -- The fetch worker: exactly one terminal message, whatever happens short
    -- of a cancel (trySync rethrows asynchronous exceptions).
    fetch plan = do
      result <- trySync (retrieveItems (envPort env) (plan ^. spSearch) (plan ^. spCollection))
      let rendered = either (Left . Just . exceptionMessage) (first errorMessageFromException) result
      writeBChan (envEvents env) (HocketMsg (fetchResult (plan ^. spKind == IncrementalAll) rendered))
    failed = either (\e -> [SystemFailed (T.pack (show e))]) (const [])
    program = \case
      Firefox -> "firefox"
      XdgOpen -> "xdg-open"

retrieveItems :: RaindropPort -> Maybe Text -> RaindropCollectionId -> IO (Either ClientError [BookmarkItemBatch])
retrieveItems port searchParam collectionId = do
  tryClientError $
    unfoldrM
      ( \currentPage -> do
          (_, items) <- runRaindrop port (RetrieveBookmarks currentPage collectionId searchParam)

          let mostRecentUpdate =
                if null items
                  then 0
                  else maximum (map (utcTimeToPOSIXSeconds . view biLastUpdate) items)

          pure $
            if null items
              then Nothing
              else Just (BookmarkItemBatch mostRecentUpdate items (fromIntegral $ length items), currentPage + 1)
      )
      0

-- | Copy a link the way @y@ always did: cleaned first ('cleanUrl'), and the
-- cleaned text is what the status line reports.
copyCleaned :: (String -> IO ()) -> Text -> IO [Msg]
copyCleaned copy link = do
  let url = cleanUrl (T.unpack link)
  r <- try @SomeException (copy url)
  pure [either (SystemFailed . ("Copy failed: " <>) . T.pack . show) (const (Copied (T.pack url))) r]

tryClientError :: IO a -> IO (Either ClientError a)
tryClientError = try @ClientError
