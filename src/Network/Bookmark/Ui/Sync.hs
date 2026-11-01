{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The fetch and batch-result policies of the TUI handlers, as pure
-- functions. Bodies are copied from the handlers unchanged, so the tests pin
-- today's behaviour, bugs included.
module Network.Bookmark.Ui.Sync
  ( SyncKind (..),
    SyncPlan,
    spKind,
    spCollection,
    spSearch,
    planSync,
    applyFetched,
    applyStepResult,
    formatPOSIXTime,
  )
where

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.State.Strict (execState)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.Bookmark.Types
import Network.Bookmark.Ui.State

data SyncKind = InitialUnsorted | IncrementalAll deriving (Show, Eq)

data SyncPlan = SyncPlan
  { _spKind :: !SyncKind,
    _spCollection :: !RaindropCollectionId,
    _spSearch :: !(Maybe Text)
  }
  deriving (Show, Eq)

makeLenses ''SyncPlan

formatPOSIXTime :: POSIXTime -> Text
formatPOSIXTime t = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" (posixSecondsToUTCTime t)

planSync :: Maybe POSIXTime -> SyncPlan
planSync lastUpdated =
  let searchParam = case lastUpdated of
        Nothing -> Nothing
        Just lastTime -> Just ("lastUpdate:>" <> formatPOSIXTime (lastTime - 86400))
      isUpdateFetch = isJust lastUpdated
      collectionToFetch =
        if isUpdateFetch
          then RaindropCollectionId "0"
          else RaindropCollectionId "-1"
   in SyncPlan (if isUpdateFetch then IncrementalAll else InitialUnsorted) collectionToFetch searchParam

applyFetched :: POSIXTime -> [BookmarkItem] -> Bool -> HocketState -> HocketState
applyFetched ts bis wasAllCollectionsFetch = execState $ do
  if wasAllCollectionsFetch
    then do
      let itemsToPotentiallyAdd = filter (\item -> item ^. biCollectionId == -1) bis
          itemIdsToRemove = map (view biId) $ filter (\item -> item ^. biCollectionId /= -1) bis
      unless (null itemIdsToRemove) $
        id %= removeItems itemIdsToRemove
      id %= insertItems itemsToPotentiallyAdd
    else id %= insertItems bis

  id %= completeAsyncOp
  currentLastUpdated <- use hsLastUpdated
  let newTimestampToConsider = if null bis then Nothing else Just ts
  case (currentLastUpdated, newTimestampToConsider) of
    (Nothing, Just newTs) -> hsLastUpdated .= Just newTs
    (Just oldTs, Just newTs) -> when (newTs >= oldTs) $ hsLastUpdated .= Just newTs
    _ -> pure ()

-- | The local effect of the ids a batch step reported as done.
applyStepResult :: BatchStep -> [BookmarkItemId] -> HocketState -> HocketState
applyStepResult StepArchive bis = removeItems bis
applyStepResult StepSetReminders bis = clearToBeRemindedFlags bis . updateItemsWithStoredReminderTimes bis
applyStepResult StepRemoveReminders bis = clearFlagsForItems ReminderToBeRemoved bis . removeReminderFromItems bis
