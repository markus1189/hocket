{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | SM-core (plan §6 Phase 6 item 3, §11 #23): the real update and settle,
-- with effects delivered by stepEffect over the pure fake. The world is
-- pure, so the model replays it to generate meaningful commands; the checks
-- are P1-P7, relations between the world before and after a command and
-- the fake, not a comparison with a second implementation (plan §9).
module Shell.SMCoreTests (smCoreTests) where

import Control.Lens (ix, (%~), (&), (.~), (^.), (^?), _2)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as Set
import Data.Time (UTCTime, addUTCTime, utc)
import qualified Data.Vector as V
import Gen (genFake)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hocket.Adapter.Fake (FailRule (..), FailStatus (..), FakeRaindrop, frCalls, frItems, frRules)
import qualified Hocket.Adapter.Fake as F
import Hocket.Agent.Protocol (FlagAction (..))
import Hocket.App.Batch (ErrorScope (..), soAborted, soFailed, soOk, soSkipped)
import Hocket.App.Effect (Effect (..))
import Hocket.App.Model
import Hocket.App.Msg
import Hocket.App.Nav (Nav (..))
import Hocket.App.Plan (BatchPlan (..))
import Hocket.App.Update (update)
import Hocket.Domain.Bookmark
import Hocket.Shell.Simulate (stepEffect)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testPropertyNamed)

t0 :: UTCTime
t0 = read "2026-11-01 00:00:00 UTC"

clock :: Int -> UTCTime
clock n = addUTCTime (fromIntegral (n + 1)) t0

now :: Now
now = Now t0 utc

data World = World
  { wModel :: !Model,
    wQueue :: ![Effect],
    wFake :: !FakeRaindrop,
    wCount :: !Int,
    -- | Server events so far, for cover only.
    wMoved :: !Bool
  }

newtype State (v :: Type -> Type) = State World

data Cmd
  = CIntent Intent
  | ServerMove BookmarkItemId
  | ServerEdit BookmarkItemId
  | FailNext F.FakeStep ErrorScope
  | Quiesce
  deriving (Show)

newtype Input (v :: Type -> Type) = Input Cmd deriving (Show)

instance FunctorB Input where bmap _ (Input c) = Input c

instance TraversableB Input where btraverse _ (Input c) = pure (Input c)

-- | The world before and after, and the messages a Quiesce delivered with
-- the effect each answered.
data Step = Step {sBefore :: World, sAfter :: World, sDelivered :: [(Effect, [Msg])]}

instance Show Step where show _ = "<step>"

apply :: Msg -> World -> World
apply m w = let (m', effs) = update m (wModel w) in w {wModel = settle m', wQueue = wQueue w <> effs}

-- | Deliver queued effects, and what their messages queue, until none is left.
quiesce :: World -> (World, [(Effect, [Msg])])
quiesce w = case wQueue w of
  [] -> (w, [])
  e : rest ->
    let (msgs, (fr', n')) = stepEffect clock e (wFake w, wCount w)
        w' = foldl (flip apply) (w {wQueue = rest, wFake = fr', wCount = n'}) msgs
        (w'', delivered) = quiesce w'
     in (w'', (e, msgs) : delivered)

-- | A server-side change: the time is between two requests, after the last.
bump :: World -> BookmarkItemId -> (BookmarkItem -> BookmarkItem) -> World
bump w bid f = w {wFake = wFake w & frItems . ix bid %~ (f . (biLastUpdate .~ addUTCTime 0.5 (clock (wCount w - 1))))}

run :: Cmd -> World -> Step
run cmd w = case cmd of
  CIntent i -> Step w (apply (Intent i) w) []
  ServerMove bid -> Step w ((bump w bid (biCollectionId .~ 99)) {wMoved = True}) []
  ServerEdit bid -> Step w (bump w bid (biTitle %~ (<> " (edited)"))) []
  FailNext step scope ->
    let nth = 1 + Map.findWithDefault 0 step (wFake w ^. frCalls)
        rule = FailRule step (Just nth) (FailHttp (if scope == ItemScoped then 404 else 503))
     in Step w (w {wFake = wFake w & frRules %~ (<> [rule])}) []
  Quiesce -> let (w', d) = quiesce w in Step w w' d

-- | Seed lastUpdate before t0: one item "from the future" would make every
-- incremental search miss later server changes, a generator artefact.
genWorld :: Gen World
genWorld = do
  fr <- genFake
  let past i = i & biLastUpdate %~ min cutoff & biCreated %~ min cutoff
      cutoff = addUTCTime (-(2 * 86400)) t0
  pure (World (settle initialModel) [] (fr & frItems %~ fmap past) 0 False)

genCmd :: World -> Gen Cmd
genCmd w =
  Gen.frequency $
    -- Weights follow the world, so sequences reach fetch, flag, Execute,
    -- deliver instead of mostly dropping commands on a busy slot.
    [ (if null local && idle then 10 else 2, pure (CIntent Refresh)),
      (if flagged && idle then 10 else 2, pure (CIntent Execute)),
      (if null (wQueue w) then 2 else 10, pure Quiesce),
      (2, pure (CIntent ClearAllFlags)),
      (1, pure (CIntent FlagAllArchive)),
      (2, CIntent . Navigate 5 <$> Gen.element [Up, Down, Top, Bottom, Page 1, HalfPage (-1), NextFlagged, PrevFlagged]),
      (2, CIntent . SetFilter <$> Gen.element ["", "git", "you", "video"]),
      (2, FailNext <$> Gen.element [F.StepFetch, F.StepArchive, F.StepSetReminders, F.StepRemoveReminders] <*> Gen.element [ItemScoped, Global])
    ]
      <> [(if flagged then 3 else 8, CIntent <$> keyIntent) | not (null local)]
      <> [(if flagged then 3 else 8, CIntent <$> (SetFlag <$> Gen.element local <*> Gen.element [FlagArchive, FlagReminder, FlagRemoveReminder, FlagNone] <*> pure now)) | not (null local)]
      <> [(2, CIntent . Select <$> Gen.element visible) | not (null visible)]
      <> [(2, ServerMove <$> Gen.element unsorted) | not (null unsorted)]
      <> [(2, ServerEdit <$> Gen.element unsorted) | not (null unsorted)]
      -- A batch is waiting: fail one of its steps' next calls before Quiesce.
      <> [(6, FailNext <$> Gen.element steps <*> Gen.element [ItemScoped, Global]) | StartBatch plan <- wQueue w, let steps = planSteps plan, not (null steps)]
  where
    planSteps plan =
      [F.StepArchive | not (null (bpArchive plan))]
        <> [F.StepSetReminders | not (null (bpSetReminders plan))]
        <> [F.StepRemoveReminders | not (null (bpRemoveReminders plan))]
    local = Map.keys (wModel w ^. mContents)
    idle = isNothing (wModel w ^. mAsyncOp)
    flagged = any ((/= None) . fst) (wModel w ^. mContents)
    visible = map (^. biId) (V.toList (wModel w ^. mVisible))
    unsorted = [k | (k, i) <- Map.toList (wFake w ^. frItems), i ^. biCollectionId == -1]
    target = Gen.frequency [(2, pure Selected), (1, ById <$> Gen.element local)]
    keyIntent = Gen.choice [SetArchive <$> target, ToggleArchive <$> target, flip ToggleReminder now <$> target, Unflag <$> target]

starts :: [Effect] -> Int
starts = length . filter (\case StartFetch _ -> True; StartBatch _ -> True; _ -> False)

flagOf :: World -> BookmarkItemId -> Maybe PendingAction
flagOf w bid = fst <$> Map.lookup bid (wModel w ^. mContents)

localIds :: World -> Set.Set BookmarkItemId
localIds = Map.keysSet . (^. mContents) . wModel

checkStep :: Cmd -> Step -> Test ()
checkStep cmd (Step before after delivered) = do
  let m = wModel after
  -- P6: I4 after every command.
  case m ^. mSelected of
    Nothing -> V.null (m ^. mVisible) === True
    Just bid -> assert (V.any ((== bid) . (^. biId)) (m ^. mVisible))
  -- P1: one outstanding async op at most; after Quiesce the slot is free.
  assert (starts (wQueue after) <= 1)
  when (isQuiesce cmd) $ do
    wQueue after === []
    isNothing (m ^. mAsyncOp) === True
  -- P5: SetFlag twice is SetFlag once.
  case cmd of
    CIntent i@(SetFlag {}) -> (wModel (apply (Intent i) after) ^. mContents) === (m ^. mContents)
    _ -> pure ()
  cover 35 "Navigate" (case cmd of CIntent (Navigate _ _) -> True; _ -> False)
  forM_ delivered $ \(eff, msgs) -> case eff of
    StartFetch _ -> do
      let ok = any (\case FetchCompleted {} -> True; _ -> False) msgs
      cover 10 "Refresh after a ServerMove" (ok && wMoved before)
      -- P4: a completed fetch leaves exactly the server's Unsorted items.
      when ok $ localIds after === Set.fromList [k | (k, i) <- Map.toList (wFake after ^. frItems), i ^. biCollectionId == -1]
    StartBatch plan -> checkBatch before after plan msgs
    _ -> pure ()
  where
    isQuiesce Quiesce = True
    isQuiesce _ = False

checkBatch :: World -> World -> BatchPlan -> [Msg] -> Test ()
checkBatch before after plan msgs = do
  let results = [r | StepDone r <- msgs]
      failed = any hasProblem results
      inArchive bid = (wFake after ^? frItems . ix bid . biCollectionId) == Just 4711
      callsOf s = Map.findWithDefault 0 s (wFake after ^. frCalls) - Map.findWithDefault 0 s (wFake before ^. frCalls)
      stepOf = \case Archived _ -> F.StepArchive; RemindersSetAt _ -> F.StepSetReminders; RemindersRemoved _ -> F.StepRemoveReminders
      planned = [F.StepArchive | not (null (bpArchive plan))] <> [F.StepSetReminders | not (null (bpSetReminders plan))] <> [F.StepRemoveReminders | not (null (bpRemoveReminders plan))]
  cover 8 "Execute with a failure" failed
  -- P2: archive is one call; its items are gone locally exactly when the
  -- server has them in the archive, and keep their flag otherwise. Flags
  -- may change between Execute and delivery (B3), so "keep" is relative to
  -- the flag just before delivery.
  forM_ (bpArchive plan) $ \bid -> do
    Set.member bid (localIds after) === not (inArchive bid)
    when (Set.member bid (localIds after)) $ flagOf after bid === flagOf before bid
  -- Items no step touched keep their flag.
  let touched = Set.fromList (bpArchive plan <> map fst (bpSetReminders plan) <> bpRemoveReminders plan)
  forM_ (Map.toList (wModel before ^. mContents)) $ \(bid, (flag, _)) ->
    unless (Set.member bid touched) $ when (Set.member bid (localIds after)) $ flagOf after bid === Just flag
  -- P3 (B3, B5): what the server accepted is the local reminder, at the
  -- planned time; a failed item keeps its local reminder.
  let reminderOf w bid = w ^? to' . ix bid . _2 . biReminder where to' f w' = fmap (const w') (f (wModel w' ^. mContents))
  forM_ results $ \case
    RemindersSetAt o -> do
      forM_ (o ^. soOk) $ \(bid, t) -> do
        when (Set.member bid (localIds after)) $ reminderOf after bid === Just (Just t)
        (wFake after ^? frItems . ix bid . biReminder) === Just (Just t)
      forM_ (o ^. soFailed) $ \((bid, _), _) -> reminderOf after bid === reminderOf before bid
    RemindersRemoved o -> do
      forM_ (o ^. soOk) $ \bid -> do
        when (Set.member bid (localIds after)) $ reminderOf after bid === Just Nothing
        (wFake after ^? frItems . ix bid . biReminder) === Just Nothing
      forM_ (o ^. soFailed) $ \(bid, _) -> reminderOf after bid === reminderOf before bid
    Archived _ -> pure ()
  -- P7: after an abort no later step calls; the aborted step stops at the
  -- failing call; failed and skipped items keep their flags.
  let abortedAt = [stepOf r | r <- results, aborted r]
  case abortedAt of
    s : _ -> forM_ (drop 1 (dropWhile (/= s) planned)) $ \later -> callsOf later === 0
    [] -> pure ()
  forM_ results $ \r -> do
    let (okN, failedIds, skippedIds, ab) = parts r
    case r of
      Archived _ -> callsOf F.StepArchive === 1
      _ -> callsOf (stepOf r) === okN + length failedIds + (if ab then 1 else 0)
    forM_ (failedIds <> skippedIds) $ \bid -> flagOf after bid === flagOf before bid
  where
    hasProblem r = let (_, f, s, a) = parts r in not (null f) || not (null s) || a
    aborted r = let (_, _, _, a) = parts r in a
    parts = \case
      Archived o -> (length (o ^. soOk), map fst (o ^. soFailed), o ^. soSkipped, isJust (o ^. soAborted))
      RemindersSetAt o -> (length (o ^. soOk), map (fst . fst) (o ^. soFailed), map fst (o ^. soSkipped), isJust (o ^. soAborted))
      RemindersRemoved o -> (length (o ^. soOk), map fst (o ^. soFailed), o ^. soSkipped, isJust (o ^. soAborted))

command :: IORef World -> Command Gen (PropertyT IO) State
command ref =
  Command
    (\(State w) -> Just (Input <$> genCmd w))
    ( \(Input cmd) -> do
        w <- liftIO (readIORef ref)
        let s = run cmd w
        liftIO (writeIORef ref (sAfter s))
        pure s
    )
    [ Require (\(State w) (Input cmd) -> valid w cmd),
      Update (\(State w) (Input cmd) _ -> State (sAfter (run cmd w))),
      Ensure (\_ _ (Input cmd) s -> checkStep cmd s)
    ]

-- | Shrinking can drop the commands that made an id local or visible.
valid :: World -> Cmd -> Bool
valid w = \case
  CIntent (Select bid) -> V.any ((== bid) . (^. biId)) (wModel w ^. mVisible)
  CIntent (SetFlag bid _ _) -> Map.member bid (wModel w ^. mContents)
  CIntent (SetArchive (ById bid)) -> Map.member bid (wModel w ^. mContents)
  CIntent (ToggleArchive (ById bid)) -> Map.member bid (wModel w ^. mContents)
  CIntent (ToggleReminder (ById bid) _) -> Map.member bid (wModel w ^. mContents)
  CIntent (Unflag (ById bid)) -> Map.member bid (wModel w ^. mContents)
  ServerMove bid -> unsortedOnServer bid
  ServerEdit bid -> unsortedOnServer bid
  _ -> True
  where
    unsortedOnServer bid = (wFake w ^? frItems . ix bid . biCollectionId) == Just (-1)

smCore :: Property
smCore = withTests 200 . property $ do
  start <- forAllWith (show . wFake) genWorld
  ref <- liftIO (newIORef start)
  actions <- forAll (Gen.sequential (Range.linear 1 60) (State start) [command ref])
  liftIO (writeIORef ref start)
  executeSequential (State start) actions

smCoreTests :: TestTree
smCoreTests = testPropertyNamed "SM-core: update, settle and the real workers over the fake" "smCore" smCore
