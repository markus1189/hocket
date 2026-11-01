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
import Control.Exception (SomeException, onException, try)
import Control.Lens (view, (^.))
import Control.Monad.Loops (unfoldrM)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Events (HocketEvent (HocketMsg))
import Network.Bookmark.App.Effect
import Network.Bookmark.App.Msg
import Network.Bookmark.App.Plan (BatchPlan (..), planSteps)
import Network.Bookmark.Batch (StepOutcome, concatOutcome)
import Network.Bookmark.Port (RaindropPort (..))
import Network.Bookmark.Types (BookmarkItemBatch (..), BookmarkItemId, BookmarkRequest (..), RaindropCollectionId, URL (..), biLastUpdate)
import Network.Bookmark.Ui.State (BatchStep (..))
import Network.Bookmark.Ui.Sync (SyncKind (IncrementalAll), spCollection, spKind, spSearch)
import Servant.Client.Core (ClientError)
import System (browseItem, cleanUrl, copyToClipboard)
import Worker (errorMessageFromException, exceptionMessage, fetchResult, runBatch, runCalls, runStep, trySync)

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
  StartBatch plan -> [] <$ async (batch plan)
  CopyText link -> copyCleaned copyToClipboard link
  where
    -- The fetch worker: exactly one terminal message, whatever happens short
    -- of a cancel (trySync rethrows asynchronous exceptions).
    fetch plan = do
      result <- trySync (retrieveItems (envPort env) (plan ^. spSearch) (plan ^. spCollection))
      let rendered = either (Left . Just . exceptionMessage) (first errorMessageFromException) result
      post (fetchResult (plan ^. spKind == IncrementalAll) rendered)
    post = writeBChan (envEvents env) . HocketMsg
    port = envPort env
    -- The batch worker: one BatchFinished at the end, also if the steps
    -- throw (runCalls already catches per call) or the thread is cancelled.
    batch plan = do
      let run = \case
            StepArchive -> runStep post StepArchive Archived (performArchive port (bpArchive plan))
            StepSetReminders -> runStep post StepSetReminders RemindersSetAt (performSetReminders port (bpSetReminders plan))
            StepRemoveReminders -> runStep post StepRemoveReminders RemindersRemoved (performRemoveReminders port (bpRemoveReminders plan))
      r <- trySync (runBatch run (planSteps plan)) `onException` post (BatchFinished (Just "batch interrupted"))
      post (BatchFinished (either (Just . exceptionMessage) id r))
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

-- | One batch call, so its outcome applies to every id.
performArchive :: RaindropPort -> [BookmarkItemId] -> IO (StepOutcome BookmarkItemId Text)
performArchive port ids = concatOutcome <$> runCalls (runRaindrop port . BatchArchiveBookmarks) [ids]

performSetReminders :: RaindropPort -> [(BookmarkItemId, UTCTime)] -> IO (StepOutcome (BookmarkItemId, UTCTime) Text)
performSetReminders port = runCalls (\(bid, reminderTime) -> runRaindrop port (SetReminder bid reminderTime))

performRemoveReminders :: RaindropPort -> [BookmarkItemId] -> IO (StepOutcome BookmarkItemId Text)
performRemoveReminders port = runCalls (runRaindrop port . RemoveReminder)

tryClientError :: IO a -> IO (Either ClientError a)
tryClientError = try @ClientError
