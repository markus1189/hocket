{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | What the async workers post, as pure functions of what their calls
-- returned, so the posting rules can be tested without IO.
module Worker
  ( fetchResult,
    postOutcome,
    runSteps,
    runBatch,
    batchStatus,
    runCalls,
    runStep,
    describeError,
    shortClientError,
    errorMessageFromException,
    trySync,
    exceptionMessage,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (SomeAsyncException, SomeException (..), displayException, fromException)
import Control.Lens (view, (^.))
import Control.Monad (mfilter)
import Control.Monad.Catch (MonadCatch, throwM, try)
import Data.Bifunctor (first, second)
import qualified Data.CaseInsensitive as CI
import Data.Char (isControl)
import Data.List (find, nub)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Bookmark.App.Msg (Msg (..), StepResult)
import Network.Bookmark.Batch
import Network.Bookmark.Types (BookmarkItemBatch)
import Network.Bookmark.Ui.State (BatchStep (..))
import Network.HTTP.Types (statusCode)
import Network.Raindrop (classify)
import Servant.Client.Core (ClientError (..), ResponseF (..))

-- | The one message a finished fetch posts, given the rendered error or the
-- pages; one by type, so an empty fetch frees the slot too (B1).
fetchResult :: Bool -> Either (Maybe Text) [BookmarkItemBatch] -> Msg
fetchResult _ (Left err) = FetchFailed err
fetchResult isUpdateFetch (Right batches) = FetchCompleted isUpdateFetch batches

-- | Post a finished step's outcome; what is left for the batch status.
postOutcome :: (Monad m) => (Msg -> m ()) -> (StepOutcome a Text -> StepResult) -> StepOutcome a Text -> m (StepOutcome () Text)
postOutcome post done outcome = first (const ()) outcome <$ post (StepDone (done outcome))

-- | One batch step: announce it, run its calls, post their outcome. Nothing
-- runs after the calls, so a computed outcome always reaches update;
-- 'runCalls' already turns any synchronous exception into one.
runStep :: (Monad m) => (Msg -> m ()) -> BatchStep -> (StepOutcome a Text -> StepResult) -> m (StepOutcome a Text) -> m (StepOutcome () Text)
runStep post step done perform = do
  post (BatchStepStarted step)
  perform >>= postOutcome post done

-- | Run the planned steps in order and stop after one that aborted; the
-- outcomes of the steps that ran.
runSteps :: (Monad m) => (step -> m (StepOutcome () e)) -> [step] -> m [(step, StepOutcome () e)]
runSteps _ [] = pure []
runSteps run (step : rest) = do
  outcome <- run step
  if isJust (outcome ^. soAborted)
    then pure [(step, outcome)]
    else ((step, outcome) :) <$> runSteps run rest

-- | The batch worker without its IO: run the steps, which post their
-- outcomes, and answer the summary for the one 'BatchFinished' the caller
-- posts, the only message that frees the batch slot.
runBatch :: (Monad m) => (BatchStep -> m (StepOutcome () Text)) -> [BatchStep] -> m (Maybe Text)
runBatch run steps = batchStatus steps <$> runSteps run steps

-- | The D6 status line, e.g.
-- @archive ok · reminders 3/5 (2 failed: 404) · remove-reminders skipped: offline@;
-- 'Nothing' when every step went through cleanly.
batchStatus :: [BatchStep] -> [(BatchStep, StepOutcome () Text)] -> Maybe Text
batchStatus planned ran
  | all (clean . snd) ran = Nothing
  | otherwise = Just (T.intercalate " · " (map ranText ran <> map skippedText (drop (length ran) planned)))
  where
    clean o = null (o ^. soFailed) && isNothing (o ^. soAborted)
    abortReason = fromMaybe "" (listToMaybe (mapMaybe (view soAborted . snd) ran))
    skippedText step = stepName step <> " skipped: " <> abortReason
    ranText (step, o)
      | clean o = stepName step <> " ok"
      | otherwise = stepName step <> " " <> count (o ^. soOk) <> "/" <> count (total o) <> " (" <> T.intercalate "; " (problems o) <> ")"
    total o = (o ^. soOk) <> map fst (o ^. soFailed) <> (o ^. soSkipped)
    problems o =
      [count fs <> " failed: " <> T.intercalate ", " (nub (map (failureText . snd) fs)) | let fs = o ^. soFailed, not (null fs)]
        <> ["aborted: " <> e | Just e <- [o ^. soAborted]]
    count :: [x] -> Text
    count = T.pack . show . length
    failureText ResultFalse = "result: false"
    failureText (CallFailed err) = err

stepName :: BatchStep -> Text
stepName = \case
  StepArchive -> "archive"
  StepSetReminders -> "reminders"
  StepRemoveReminders -> "remove-reminders"

-- | Catch what the action itself threw, so a worker can still post its
-- terminal message; asynchronous exceptions (a cancel) are rethrown.
trySync :: (MonadCatch m) => m a -> m (Either SomeException a)
trySync act =
  try act >>= \case
    Left e -> case fromException e of
      Just (_ :: SomeAsyncException) -> throwM e
      Nothing -> pure (Left e)
    Right a -> pure (Right a)

-- | For the status line: since GHC 9.10, 'displayException' on a
-- 'SomeException' appends a multi-line backtrace, so render the inner one.
exceptionMessage :: SomeException -> Text
exceptionMessage (SomeException e) = T.pack (displayException e)

-- | One call per item through 'runItems'. A 'ClientError' is scoped by
-- 'classify'; any other synchronous exception is global, so the step stops
-- there but keeps what already succeeded.
runCalls :: (MonadCatch m) => (a -> m Bool) -> [a] -> m (StepOutcome a Text)
runCalls call = fmap (second snd) . runItems fst (fmap (first describeError) . trySync . call)

describeError :: SomeException -> (ErrorScope, Text)
describeError e = maybe (Global, exceptionMessage e) (\ce -> (classify ce, shortClientError ce)) (fromException e)

-- | An error as the batch summary names it (D6: "2 failed: 404"): the status
-- code, with Raindrop's x-error text when it fits on the line.
shortClientError :: ClientError -> Text
shortClientError = \case
  FailureResponse _ resp -> T.pack (show (statusCode (responseStatusCode resp))) <> maybe "" (" " <>) (shortXError resp)
  ConnectionError _ -> "offline"
  DecodeFailure _ _ -> "bad response"
  UnsupportedContentType _ _ -> "bad response"
  InvalidContentTypeHeader _ -> "bad response"
  where
    shortXError = mfilter ((<= 40) . T.length) . xErrorText

-- | Raindrop's @x-error@ text, unless it is blank, so callers fall back to the
-- status code, or carries control characters, which would garble the status
-- line.
xErrorText :: ResponseF a -> Maybe Text
xErrorText resp =
  mfilter (\t -> not (T.null t) && not (T.any isControl t)) $
    T.strip . T.decodeUtf8Lenient . snd <$> find ((== CI.mk "x-error") . fst) (responseHeaders resp)

errorMessageFromException :: ClientError -> Maybe Text
errorMessageFromException (FailureResponse _ resp) = xErrorText resp <|> code
  where
    code = Just . T.pack $ "Got status: " <> show (responseStatusCode resp)
errorMessageFromException (DecodeFailure msg _) = Just ("Unexpected response from Raindrop: " <> msg)
errorMessageFromException e = Just . T.pack $ show e
