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
import Data.Maybe (isJust, isNothing, listToMaybe)
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
    wMoved :: !Bool,
    -- | The effect whose messages are being delivered one by one.
    wInFlight :: !(Maybe InFlight)
  }

-- | The worker ran in full when delivery began (the fake is at its end
-- state, as when the real worker runs ahead of the UI); its messages
-- arrive one per DeliverOne.
data InFlight = InFlight {ifEffect :: !Effect, ifAll :: ![Msg], ifLeft :: ![Msg], ifBefore :: World}

newtype State (v :: Type -> Type) = State World

data Cmd
  = CIntent Intent
  | ServerMove BookmarkItemId
  | ServerEdit BookmarkItemId
  | -- | Fail that step's call this many calls after its next one.
    FailNext F.FakeStep ErrorScope Int
  | DeliverOne
  | Quiesce
  deriving (Show)

newtype Input (v :: Type -> Type) = Input Cmd deriving (Show)

instance FunctorB Input where bmap _ (Input c) = Input c

instance TraversableB Input where btraverse _ (Input c) = pure (Input c)

-- | An effect whose last message arrived: its messages, and the world
-- before delivery began.
type Delivered = (Effect, [Msg], World)

-- | The world before and after, and the effects that finished delivering.
data Step = Step {sBefore :: World, sAfter :: World, sDelivered :: [Delivered]}

instance Show Step where show _ = "<step>"

apply :: Msg -> World -> World
apply m w = let (m', effs) = update m (wModel w) in w {wModel = settle m', wQueue = wQueue w <> effs}

-- | One message: of the effect in flight, or of the next queued one, whose
-- worker then runs.
deliverOne :: World -> (World, [Delivered])
deliverOne w = case wInFlight w of
  Just f -> case ifLeft f of
    m : rest ->
      let w' = apply m (w {wInFlight = Just f {ifLeft = rest}})
       in if null rest then finish w' f else (w', [])
    [] -> finish w f
  Nothing -> case wQueue w of
    [] -> (w, [])
    e : rest ->
      let (msgs, (fr', n')) = stepEffect clock e (wFake w, wCount w)
       in deliverOne (w {wQueue = rest, wFake = fr', wCount = n', wInFlight = Just (InFlight e msgs msgs w)})
  where
    finish w' f = (w' {wInFlight = Nothing}, [(ifEffect f, ifAll f, ifBefore f)])

-- | Deliver until nothing is in flight or queued.
quiesce :: World -> (World, [Delivered])
quiesce w
  | isNothing (wInFlight w) && null (wQueue w) = (w, [])
  | otherwise = let (w1, d1) = deliverOne w; (w2, d2) = quiesce w1 in (w2, d1 <> d2)

-- | Messages of an effect are still to come.
pending :: World -> Bool
pending = maybe False (not . null . ifLeft) . wInFlight

-- | A server-side change: the time is between two requests, after the last.
bump :: World -> BookmarkItemId -> (BookmarkItem -> BookmarkItem) -> World
bump w bid f = w {wFake = wFake w & frItems . ix bid %~ (f . (biLastUpdate .~ addUTCTime 0.5 (clock (wCount w - 1))))}

run :: Cmd -> World -> Step
run cmd w = case cmd of
  CIntent i -> Step w (apply (Intent i) w) []
  ServerMove bid -> Step w ((bump w bid (biCollectionId .~ 99)) {wMoved = True}) []
  ServerEdit bid -> Step w (bump w bid (biTitle %~ (<> " (edited)"))) []
  FailNext step scope offset ->
    let nth = 1 + offset + Map.findWithDefault 0 step (wFake w ^. frCalls)
        rule = FailRule step (Just nth) (FailHttp (if scope == ItemScoped then 404 else 503))
     in Step w (w {wFake = wFake w & frRules %~ (<> [rule])}) []
  DeliverOne -> let (w', d) = deliverOne w in Step w w' d
  Quiesce -> let (w', d) = quiesce w in Step w w' d

-- | Seed lastUpdate before t0: one item "from the future" would make every
-- incremental search miss later server changes, a generator artefact.
genWorld :: Gen World
genWorld = do
  fr <- genFake
  -- All in Unsorted: the app then holds enough items for batches of
  -- several reminders; items leave Unsorted through ServerMove and archive.
  let past i = i & biLastUpdate %~ min cutoff & biCreated %~ min cutoff & biCollectionId .~ (-1)
      cutoff = addUTCTime (-(2 * 86400)) t0
  pure (World (settle initialModel) [] (fr & frItems %~ fmap past) 0 False Nothing)

genCmd :: World -> Gen Cmd
genCmd w
  | pending w =
      Gen.frequency $
        [ (10, pure DeliverOne),
          (4, pure Quiesce),
          (2, pure (CIntent Refresh)),
          (2, pure (CIntent Execute)),
          (2, CIntent . Navigate 5 <$> Gen.element [Up, Down, Top, Bottom, NextFlagged]),
          (2, CIntent . SetFilter <$> Gen.element ["", "git"])
        ]
          <> [(1, CIntent . Select <$> Gen.element visible) | not (null visible)]
  | otherwise = genIdle w
  where
    visible = map (^. biId) (V.toList (wModel w ^. mVisible))

genIdle :: World -> Gen Cmd
genIdle w =
  Gen.frequency $
    -- Weights follow the world, so sequences reach fetch, flag, Execute,
    -- deliver instead of mostly dropping commands on a busy slot.
    [ (if null local && idle then 10 else 2, pure (CIntent Refresh)),
      (if nFlagged >= 3 && idle then 10 else 2, pure (CIntent Execute)),
      (if null (wQueue w) || unarmed then 2 else 10, pure Quiesce),
      (if null (wQueue w) || unarmed then 1 else 6, pure DeliverOne),
      (2, pure (CIntent ClearAllFlags)),
      (1, pure (CIntent FlagAllArchive)),
      (2, CIntent . Navigate 5 <$> Gen.element [Up, Down, Top, Bottom, Page 1, HalfPage (-1), NextFlagged, PrevFlagged]),
      (2, CIntent . SetFilter <$> Gen.element ["", "git", "you", "video"]),
      (2, FailNext <$> Gen.element [F.StepFetch, F.StepArchive, F.StepSetReminders, F.StepRemoveReminders] <*> Gen.element [ItemScoped, Global] <*> genOffset)
    ]
      <> [(if nFlagged >= 3 then 3 else 8, CIntent <$> keyIntent) | not (null local)]
      <> [(if nFlagged >= 3 then 3 else 8, CIntent <$> (SetFlag <$> Gen.element local <*> Gen.frequency [(1, pure FlagArchive), (3, pure FlagReminder), (2, pure FlagRemoveReminder), (1, pure FlagNone)] <*> pure now)) | not (null local)]
      <> [(2, CIntent . Select <$> Gen.element visible) | not (null visible)]
      <> [(2, ServerMove <$> Gen.element unsorted) | not (null unsorted)]
      <> [(2, ServerEdit <$> Gen.element unsorted) | not (null unsorted)]
      -- A batch is waiting: fail one of its steps' next calls before Quiesce.
      -- A batch is waiting: fail a call its steps will make, often a later
      -- item's, so an abort can follow earlier successes.
      <> [ (8, Gen.element steps >>= \(st, n) -> FailNext st <$> Gen.frequency [(2, pure Global), (1, pure ItemScoped)] <*> Gen.int (Range.constant 0 (n - 1)))
         | StartBatch plan <- wQueue w,
           let steps = stepSizes plan,
           not (null steps)
         ]
      -- "Item 2 of 5 -> 503": a global error after earlier successes.
      <> [ (12, Gen.element multi >>= \(st, n) -> FailNext st Global <$> Gen.int (Range.constant 1 (n - 1)))
         | StartBatch plan <- wQueue w,
           let multi = [(st, n) | (st, n) <- stepSizes plan, st /= F.StepArchive, n >= 2],
           not (null multi)
         ]
  where
    genOffset = Gen.int (Range.constant 0 3)
    -- A batch is queued and no rule waits for any of its calls yet: give
    -- FailNext a chance before delivery.
    unarmed =
      or
        [ null [() | FailRule st (Just k) _ <- wFake w ^. frRules, (st', _) <- stepSizes plan, st == st', k > Map.findWithDefault 0 st (wFake w ^. frCalls)]
        | StartBatch plan <- wQueue w
        ]
    -- Each planned step and how many calls it makes (archive: one).
    stepSizes plan =
      [(F.StepArchive, 1) | not (null (bpArchive plan))]
        <> [(F.StepSetReminders, length (bpSetReminders plan)) | not (null (bpSetReminders plan))]
        <> [(F.StepRemoveReminders, length (bpRemoveReminders plan)) | not (null (bpRemoveReminders plan))]
    local = Map.keys (wModel w ^. mContents)
    idle = isNothing (wModel w ^. mAsyncOp)
    nFlagged = length (filter ((/= None) . fst) (Map.elems (wModel w ^. mContents)))
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
checkStep cmd (Step _ after delivered) = do
  let m = wModel after
  -- P6: I4 after every command.
  case m ^. mSelected of
    Nothing -> V.null (m ^. mVisible) === True
    Just bid -> assert (V.any ((== bid) . (^. biId)) (m ^. mVisible))
  -- P1: one outstanding async op at most; after Quiesce the slot is free.
  assert (starts (wQueue after) + maybe 0 (starts . pure . ifEffect) (wInFlight after) <= 1)
  when (isQuiesce cmd) $ do
    wQueue after === []
    isNothing (wInFlight after) === True
    isNothing (m ^. mAsyncOp) === True
  -- I2/B2: only the terminal message frees the slot, not one before it.
  case wInFlight after of
    Just f | pending after, isBatch (ifEffect f) -> isJust (m ^. mAsyncOp) === True
    _ -> pure ()
  -- P5: SetFlag twice is SetFlag once.
  case cmd of
    CIntent i@(SetFlag {}) -> (wModel (apply (Intent i) after) ^. mContents) === (m ^. mContents)
    _ -> pure ()
  cover 35 "Navigate" (case cmd of CIntent (Navigate _ _) -> True; _ -> False)
  forM_ delivered $ \(eff, msgs, before) -> case eff of
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
    isBatch (StartBatch _) = True
    isBatch _ = False

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
  -- P3, server side (B5): a planned reminder item changed on the server
  -- (accepted writes bump lastUpdate) exactly when the step reports it ok.
  let changed bid = (wFake before ^? frItems . ix bid . biLastUpdate) /= (wFake after ^? frItems . ix bid . biLastUpdate)
      okSet = Set.fromList (concat [map fst (o ^. soOk) | RemindersSetAt o <- results] <> concat [o ^. soOk | RemindersRemoved o <- results])
  forM_ (map fst (bpSetReminders plan) <> bpRemoveReminders plan) $ \bid ->
    (bid, changed bid) === (bid, Set.member bid okSet)
  -- P7 from the fake (D6): the first global rule a step reached aborts it,
  -- at that call, and no later step calls.
  let callsBefore st = Map.findWithDefault 0 st (wFake before ^. frCalls)
      -- The fake applies the first rule that matches a call.
      ruleFor st k = listToMaybe [status | FailRule st' nth status <- wFake before ^. frRules, st' == st, maybe True (== k) nth]
      globalAt st = [k | k <- [callsBefore st + 1 .. callsBefore st + callsOf st], maybe False isGlobal (ruleFor st k)]
      isGlobal = \case FailConn -> True; FailHttp c -> c >= 500 || c `elem` [401, 403, 408, 429]
  forM_ planned $ \st -> case globalAt st of
    [] -> pure ()
    ks -> do
      let k = minimum ks
      (st, [aborted r | r <- results, stepOf r == st]) === (st, [True])
      (st, callsOf st) === (st, k - callsBefore st)
      forM_ (drop 1 (dropWhile (/= st) planned)) $ \later -> (later, callsOf later) === (later, 0)
  cover 5 "a global error aborted a step" (any aborted results)
  cover 0.5 "an abort after at least one success" (any (\r -> aborted r && let (n, _, _, _) = parts r in n >= 1) [r | r <- results, stepOf r /= F.StepArchive])
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
valid w cmd | pending w && changesFlagsOrServer cmd = False
valid w cmd = validIdle w cmd

-- | Not generated while messages are pending: "before delivery" would
-- stop being one moment.
changesFlagsOrServer :: Cmd -> Bool
changesFlagsOrServer = \case
  CIntent i -> case i of
    SetArchive _ -> True
    ToggleArchive _ -> True
    ToggleReminder _ _ -> True
    Unflag _ -> True
    SetFlag {} -> True
    ClearAllFlags -> True
    FlagAllArchive -> True
    _ -> False
  ServerMove _ -> True
  ServerEdit _ -> True
  FailNext {} -> True
  _ -> False

validIdle :: World -> Cmd -> Bool
validIdle w = \case
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
smCore = withTests 500 . property $ do
  start <- forAllWith (show . wFake) genWorld
  ref <- liftIO (newIORef start)
  actions <- forAll (Gen.sequential (Range.linear 1 60) (State start) [command ref])
  liftIO (writeIORef ref start)
  executeSequential (State start) actions

smCoreTests :: TestTree
smCoreTests = testPropertyNamed "SM-core: update, settle and the real workers over the fake" "smCore" smCore
