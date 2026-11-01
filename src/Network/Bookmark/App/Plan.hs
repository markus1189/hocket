-- | What @X@ will send, decided purely from the state (plan §11 #19).
module Network.Bookmark.App.Plan (BatchPlan (..), planBatch) where

import Control.Lens ((^.))
import Data.Map (elems)
import Data.Time (UTCTime)
import Network.Bookmark.Types
import Network.Bookmark.Ui.State (HocketState, hsContents)

data BatchPlan = BatchPlan
  { bpArchive :: [BookmarkItemId],
    bpSetReminders :: [(BookmarkItemId, UTCTime)],
    bpRemoveReminders :: [BookmarkItemId]
  }
  deriving (Eq, Show)

-- | In 'Map' order of the id, as the batch workers send them (E2E pins the
-- reminder order); 'Nothing' exactly when 'batchStepsWithWork' is empty.
planBatch :: HocketState -> Maybe BatchPlan
planBatch s
  | null archive && null setRem && null removeRem = Nothing
  | otherwise = Just (BatchPlan archive setRem removeRem)
  where
    flagged = elems (s ^. hsContents)
    archive = [item ^. biId | (ToBeArchived, item) <- flagged]
    setRem = [(item ^. biId, t) | (ToBeReminded t, item) <- flagged]
    removeRem = [item ^. biId | (ReminderToBeRemoved, item) <- flagged]
