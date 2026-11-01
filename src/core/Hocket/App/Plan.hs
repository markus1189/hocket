-- | What @X@ will send, decided purely from the state (plan §11 #19).
module Hocket.App.Plan (BatchPlan (..), planBatch, planSteps) where

import Control.Lens ((^.))
import Data.Map (elems)
import Data.Time (UTCTime)
import Hocket.App.Model (BatchStep (..), Model, mContents)
import Hocket.Domain.Bookmark

data BatchPlan = BatchPlan
  { bpArchive :: [BookmarkItemId],
    bpSetReminders :: [(BookmarkItemId, UTCTime)],
    bpRemoveReminders :: [BookmarkItemId]
  }
  deriving (Eq, Show)

-- | In 'Map' order of the id, as the batch workers send them (E2E pins the
-- reminder order); 'Nothing' exactly when 'batchStepsWithWork' is empty.
planBatch :: Model -> Maybe BatchPlan
planBatch s
  | null archive && null setRem && null removeRem = Nothing
  | otherwise = Just (BatchPlan archive setRem removeRem)
  where
    flagged = elems (s ^. mContents)
    archive = [item ^. biId | (ToBeArchived, item) <- flagged]
    setRem = [(item ^. biId, t) | (ToBeReminded t, item) <- flagged]
    removeRem = [item ^. biId | (ReminderToBeRemoved, item) <- flagged]

-- | The steps with work, in the order the batch runs them.
planSteps :: BatchPlan -> [BatchStep]
planSteps (BatchPlan archive setRem removeRem) =
  [StepArchive | not (null archive)] <> [StepSetReminders | not (null setRem)] <> [StepRemoveReminders | not (null removeRem)]
