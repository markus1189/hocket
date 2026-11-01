{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Running one batch step item by item, with failures handled by their
-- scope (plan D6). Pure and generic so it can be tested without HTTP.
module Hocket.App.Batch
  ( ErrorScope (..),
    Failure (..),
    StepOutcome,
    soOk,
    soFailed,
    soSkipped,
    soAborted,
    runItems,
    concatOutcome,
  )
where

import Control.Lens

-- | Whether a failed call says something about its item, or would hit every
-- following call the same way.
data ErrorScope = ItemScoped | Global deriving (Show, Eq)

-- | An item-scoped failure: Raindrop answered @result: false@, or the call
-- failed with an item-scoped error.
data Failure e = ResultFalse | CallFailed e deriving (Show, Eq, Functor)

data StepOutcome a e = StepOutcome
  { _soOk :: [a],
    -- | Item-scoped; these items keep their flags.
    _soFailed :: [(a, Failure e)],
    -- | The item that hit the global error, and every item after it.
    _soSkipped :: [a],
    _soAborted :: Maybe e
  }
  deriving (Show, Eq, Functor)

makeLenses ''StepOutcome

instance Bifunctor StepOutcome where
  bimap f g (StepOutcome ok failed skipped aborted) =
    StepOutcome (map f ok) [(f x, fmap g e) | (x, e) <- failed] (map f skipped) (fmap g aborted)

-- | Call for each item in order. Item-scoped failures are recorded and the
-- step goes on; the first global error stops it, so no further call is made
-- (which respects a 429).
runItems :: (Monad m) => (e -> ErrorScope) -> (a -> m (Either e Bool)) -> [a] -> m (StepOutcome a e)
runItems scope call = go [] []
  where
    finish ok failed = StepOutcome (reverse ok) (reverse failed)
    go ok failed [] = pure (finish ok failed [] Nothing)
    go ok failed (x : xs) =
      call x >>= \case
        Right True -> go (x : ok) failed xs
        Right False -> go ok ((x, ResultFalse) : failed) xs
        Left e -> case scope e of
          ItemScoped -> go ok ((x, CallFailed e) : failed) xs
          Global -> pure (finish ok failed (x : xs) (Just e))

-- | Spread the outcome of one call made for a group of items over the items.
concatOutcome :: StepOutcome [a] e -> StepOutcome a e
concatOutcome (StepOutcome ok failed skipped aborted) =
  StepOutcome (concat ok) [(x, f) | (xs, f) <- failed, x <- xs] (concat skipped) aborted
