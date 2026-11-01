{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The shell's only way from an 'Effect' to IO (plan §4.3).
module Hocket.Shell.Interpret
  ( Env (..),
    newEnv,
    postMsg,
    injectMsg,
    tookMsg,
    step,
    publish,
    publishSTM,
    interpret,
    copyCleaned,
    batchWorker,
  )
where

import Brick.BChan (BChan, newBChan, writeBChan, writeBChanNonBlocking)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO)
import Control.Exception (onException, try)
import Control.Lens (view, (^.))
import Control.Monad (foldM, unless)
import Control.Monad.Loops (unfoldrM)
import Data.Bifunctor (first, second)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Hocket.Adapter.System (cleanUrl)
import Hocket.Agent.Snapshot (AgentSnapshot, asVersion, takeSnapshot)
import Hocket.App.Batch (StepOutcome, concatOutcome)
import Hocket.App.Effect
import Hocket.App.Model (BatchStep (..), HocketState)
import Hocket.App.Msg
import Hocket.App.Plan (BatchPlan (..), planSteps)
import Hocket.App.Sync (SyncKind (IncrementalAll), spCollection, spKind, spSearch)
import Hocket.App.Update (update)
import Hocket.Domain.Bookmark (BookmarkItemBatch (..), BookmarkItemId, BookmarkRequest (..), RaindropCollectionId, URL (..), biLastUpdate)
import Hocket.Port (RaindropPort (..), SystemPort (..))
import Hocket.Shell.Worker (errorMessageFromException, exceptionMessage, fetchResult, runCalls, runStep, runSteps, trySync)
import Servant.Client.Core (ClientError)

-- | Where to post events, Raindrop, the desktop and the clock.
data Env = Env
  { -- | Write only through 'postMsg' and 'injectMsg': they count what is
    -- in flight, which the headless loop's idle check needs and brick's
    -- channel cannot tell.
    envEvents :: !(BChan Msg),
    -- | Posted and not yet taken by the event loop.
    envPending :: !(TVar Int),
    envPort :: !RaindropPort,
    envSystem :: !SystemPort,
    envNow :: !(IO Now)
  }

newEnv :: RaindropPort -> SystemPort -> IO Now -> IO Env
newEnv port system now = Env <$> newBChan 10 <*> newTVarIO 0 <*> pure port <*> pure system <*> pure now

-- | Post, waiting while the channel is full (the workers). Counts before
-- writing, so a message in the channel is always pending: loopIdle relies
-- on it, and brick's channel hides its queue, so the count and the write
-- cannot share a transaction.
postMsg :: Env -> Msg -> IO ()
postMsg env m = atomically (modifyTVar' (envPending env) (+ 1)) >> writeBChan (envEvents env) m

-- | Post unless the channel is full (the agent socket); whether it was.
-- Counts before writing, as 'postMsg' does.
injectMsg :: Env -> Msg -> IO Bool
injectMsg env m = do
  atomically (modifyTVar' (envPending env) (+ 1))
  posted <- writeBChanNonBlocking (envEvents env) m
  unless posted (atomically (modifyTVar' (envPending env) (subtract 1)))
  pure posted

-- | The event loop took a message off the channel.
tookMsg :: Env -> STM ()
tookMsg env = modifyTVar' (envPending env) (subtract 1)

-- | One message as both event loops run it: update, then its effects in
-- order, each effect's immediate answers through 'step' again, depth first.
-- True when an effect was 'Halt'; the effects after it still run, as brick
-- only stops after the event.
step :: Env -> Msg -> HocketState -> IO (HocketState, Bool)
step env m s0 = foldM run (s1, False) effects
  where
    (s1, effects) = update m s0
    run (s, halted) = \case
      Halt -> pure (s, True)
      effect -> interpret env effect >>= foldM (\(s', h) reply -> second (|| h) <$> step env reply s') (s, halted)

-- | Mirror the settled model for the agent socket, one version per event.
-- Persistent data sharing keeps this cheap; the item projection is only
-- fully forced by a server thread that actually serves it.
publish :: TVar AgentSnapshot -> HocketState -> IO ()
publish snapVar = atomically . publishSTM snapVar

-- | 'publish' inside a transaction, for a loop that must change more in
-- the same step.
publishSTM :: TVar AgentSnapshot -> HocketState -> STM ()
publishSTM snapVar s = modifyTVar' snapVar $ \old -> takeSnapshot (asVersion old + 1) s

-- | Run one effect. What the TUI waits for (browser, clipboard) answers
-- with messages for the same event, so the status line is as quick as the
-- old handlers'.
interpret :: Env -> Effect -> IO [Msg]
interpret env = \case
  -- The shell halts the event loop itself.
  Halt -> pure []
  OpenUrl opener url -> failed <$> trySync (sysOpenUrl (envSystem env) (program opener) (URL (T.unpack url)))
  StartFetch plan -> [] <$ async (fetch plan)
  StartBatch plan -> [] <$ async (batchWorker post (envPort env) plan)
  CopyText link -> copyCleaned (sysCopy (envSystem env)) link
  where
    -- The fetch worker: exactly one terminal message, whatever happens short
    -- of a cancel (trySync rethrows asynchronous exceptions).
    fetch plan = do
      result <- trySync (retrieveItems (envPort env) (plan ^. spSearch) (plan ^. spCollection))
      let rendered = either (Left . Just . exceptionMessage) (first errorMessageFromException) result
      post (fetchResult (plan ^. spKind == IncrementalAll) rendered)
    post = postMsg env
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
  r <- trySync (copy url)
  pure [either (SystemFailed . ("Copy failed: " <>) . T.pack . show) (const (Copied (T.pack url))) r]

-- | One batch call, so its outcome applies to every id.
performArchive :: RaindropPort -> [BookmarkItemId] -> IO (StepOutcome BookmarkItemId Text)
performArchive port ids = concatOutcome <$> runCalls (runRaindrop port . BatchArchiveBookmarks) [ids]

performSetReminders :: RaindropPort -> [(BookmarkItemId, UTCTime)] -> IO (StepOutcome (BookmarkItemId, UTCTime) Text)
performSetReminders port = runCalls (\(bid, reminderTime) -> runRaindrop port (SetReminder bid reminderTime))

performRemoveReminders :: RaindropPort -> [BookmarkItemId] -> IO (StepOutcome BookmarkItemId Text)
performRemoveReminders port = runCalls (runRaindrop port . RemoveReminder)

-- | The batch worker: one BatchFinished at the end, also if the steps throw
-- (runCalls already catches per call) or the thread is cancelled, as a
-- headless loop's or a test's teardown does.
batchWorker :: (Msg -> IO ()) -> RaindropPort -> BatchPlan -> IO ()
batchWorker post port plan = do
  let run = \case
        StepArchive -> runStep post StepArchive Archived (performArchive port (bpArchive plan))
        StepSetReminders -> runStep post StepSetReminders RemindersSetAt (performSetReminders port (bpSetReminders plan))
        StepRemoveReminders -> runStep post StepRemoveReminders RemindersRemoved (performRemoveReminders port (bpRemoveReminders plan))
  r <- trySync (runSteps run (planSteps plan)) `onException` post (BatchFinished (Just "batch interrupted"))
  post (BatchFinished (either (Just . exceptionMessage) (const Nothing) r))

tryClientError :: IO a -> IO (Either ClientError a)
tryClientError = try @ClientError
