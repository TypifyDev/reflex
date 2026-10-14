{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Interpose-cascade architecture for the opaque-m
-- 'Reflex.Host.HeadlessEffectful.Adjustable' effect.
--
-- Goal of THIS module: prove out the effect-system architecture. Concrete
-- per-effect wiring (RequesterState IORef, mergeInt aggregation, etc.) is
-- left as 'undefined'-typed placeholders — what matters here is that the
-- interpose cascade shape typechecks and composes cleanly.
--
-- Design summary (see 2026-08-31 session):
--   * 'HE.Adjustable t' is opaque-m — same monad for parent + child, nesting
--     is free via 'runWithReplace'.
--   * Per-effect cohort semantics come from INTERPOSERS that layer on top of
--     the shared 'HE.Adjustable t' effect. Each interposer:
--       (1) catches 'send @(Adjustable t) RunWithReplace';
--       (2) wraps the child action with per-cohort setup for ONE effect;
--       (3) re-sends. Dispatch goes to the next handler down the stack.
--   * Widget code is unchanged — writes 'runWithReplace' + normal effect ops.
--     Interposers activate automatically per cohort.
--
-- Composition (see 'exampleComposition' at the bottom):
--   > runAdjustable . withAdjustableRequester . withAdjustableQuery $ widget
--
-- Widget sees ONE 'HE.Adjustable t' effect in its row. There is no
-- ambiguity in send-dispatch. Nesting works because opaque m.
module Reflex.Host.AdjustableInterpose
  ( -- * Per-cohort identity
    CohortEnv (..)
  , newCohortEnv

    -- * Per-effect interposers on 'HE.Adjustable t'
  , withAdjustableRequester
  , withAdjustableQuery
  ) where

import           Data.Bits                       ((.|.))
import qualified Data.Foldable                   as Foldable
import qualified Data.IntMap.Strict              as IntMap
import qualified Data.IORef                      as IORef
import           Data.IORef                      (IORef)
import qualified Data.Patch                      as RP
import           Data.Semigroup.Commutative      (Commutative)
import qualified Data.Proxy                      as Proxy
import qualified System.IO.Unsafe                as Unsafe
import qualified Unsafe.Coerce                   as Coerce

import qualified Control.Monad.IO.Class          as MIO

import qualified Effectful                       as E
import           Effectful                       (Eff, (:>))
import qualified Effectful.Dispatch.Dynamic      as ED

import qualified GHC.Exts                        as Exts

import qualified Reflex                          as R
import qualified Reflex.Query.Class              as RQuery
import qualified Reflex.Requester.Base           as RB
import qualified Reflex.Spider.Internal          as SI

import qualified Reflex.Host.HeadlessEffectful   as HE
import           Reflex.Host.HeadlessEffectful   (Adjustable (..), Requester (..))


--------------------------------------------------------------------------------
-- Rank2 — newtype wrapper to hide 'forall a.' polymorphism from monadic
-- 'pure' inside 'localUnliftIO' IO callbacks (used by DMap traverse
-- variants). Necessary because 'pure' would monomorphize the rank-2
-- function on the way out.
--------------------------------------------------------------------------------

newtype Rank2 k v v' m = Rank2 (forall a. k a -> v a -> m (v' a))

--------------------------------------------------------------------------------
-- CohortEnv — per-cohort identity handed to every interposer
--------------------------------------------------------------------------------

-- | Fresh cohort identity allocated per 'RunWithReplace' invocation. All
-- interposers in the stack share the same CohortEnv for a given cohort so
-- they can coordinate per-cohort resources if needed.
newtype CohortEnv = CohortEnv { unCohortEnv :: Int }
  deriving (Eq, Ord, Show)

cohortIdCounter :: IORef Int
cohortIdCounter = Unsafe.unsafePerformIO (IORef.newIORef 0)
{-# NOINLINE cohortIdCounter #-}

newCohortEnv :: IO CohortEnv
newCohortEnv = do
  n <- IORef.atomicModifyIORef' cohortIdCounter
         (\i -> let i' = i + 1 in (i', i'))
  pure (CohortEnv n)


--------------------------------------------------------------------------------
-- withAdjustableRequester
--
-- Interposes on 'HE.Adjustable t'. When 'RunWithReplace' fires:
--   1. Allocate a fresh CohortEnv per cohort (initial + each replacement).
--   2. Wrap the child action via 'requesterCohortWrap' — this is where
--      per-cohort Requester setup happens (fresh RequesterState IORef,
--      interpose on Requester, snapshot for aggregation).
--   3. Re-send 'RunWithReplace' — cascades to next handler.
--   4. Route the per-cohort aggregated requests back to the outer Requester.
--
-- Concrete Requester machinery (marked 'undefined :: X') is deferred; what
-- this module PROVES is that the interpose+re-send shape typechecks and
-- composes with itself.
--------------------------------------------------------------------------------

withAdjustableRequester
  :: forall x req rsp es a.
     ( SI.HasSpiderTimeline x
     , R.Reflex (SI.SpiderTimeline x)
     , E.IOE :> es
     , HE.Adjustable (SI.SpiderTimeline x) :> es
     , HE.Requester (SI.SpiderTimeline x) req rsp :> es
     --                                    ^^^ Requester is REALLY in row —
     --                                        used via requestingBundledE below
     )
  => Eff es a -> Eff es a
withAdjustableRequester =
  ED.interpose @(HE.Adjustable (SI.SpiderTimeline x)) $ \env -> \case

    RunWithReplace a0 a' -> do
      -- Strategy = SeqForkUnlift (compiler-suggested at storage-mismatch site).
      -- Previously ConcUnlift Persistent Unlimited was tried and produced
      -- 'version (N) /= storageVersion (M)' when the replacement fired
      -- inside pushCheap in a later frame; runtime error message says to
      -- try SeqForkUnlift. Each replacement fire gets a fresh unlift-fork
      -- rather than reusing a persistent-cloned env.
      (wrappedInitial, wrappedRepls) <-
        ED.localUnliftIO env E.SeqForkUnlift $ \unliftIO -> pure
          ( do  -- wrappedInitial :: Eff es (a1, agg)
              cid <- MIO.liftIO newCohortEnv
              requesterCohortWrap @x @req @rsp cid
                (MIO.liftIO (unliftIO a0))
          , R.fmapCheap
              (\r -> do  -- per replacement fire
                  cid <- MIO.liftIO newCohortEnv
                  requesterCohortWrap @x @req @rsp cid
                    (MIO.liftIO (unliftIO r)))
              a'
          )

      -- Re-send RunWithReplace to next handler down. GHC unifies the child's
      -- return type to (childVal, aggEv), so the returned Event carries per-
      -- cohort agg too.
      ((childVal, aggInit), perCohortEv)
        <- ED.send @(HE.Adjustable (SI.SpiderTimeline x))
             (RunWithReplace wrappedInitial wrappedRepls)

      -- ROUTE the initial cohort's aggregated requests to the OUTER Requester.
      -- aggInit :: Event t (RequesterData req) — wrap with `IntMap.singleton 0`
      -- (cohort 0 = initial) then `multiEntry` to produce `Entry req Multi`
      -- for `requestingBundledE`. Mirrors 'HE.runAdjustableInRequesterT'
      -- lines 927-942. Real impl would also route perCohortEv aggs via
      -- 'switchPromptlyDyn' — deferred here to keep the proof minimal.
      _initRspEv <- HE.requestingBundledE
        @(SI.SpiderTimeline x) @req @rsp
        (R.fmapCheap (RB.multiEntry . IntMap.singleton 0) aggInit)

      -- Strip the aggregation from the caller-visible event.
      pure (childVal, R.fmapCheap fst perCohortEv)

    -- Traverse variants: pass-through re-send to next handler down. The
    -- fused runner at HeadlessEffectful.hs:1580/1692/1808 already has full
    -- per-cohort Requester handling for these three constructors (via
    -- cohortPtr / mkCohortSel / runInCohortIO). We do NOT add extra
    -- per-cohort Requester wrapping here — the fused runner's is correct
    -- and complete.
    --
    -- The child fn 'f :: ... -> m (v' _)' has 'm ~ Eff localEs'. Need to
    -- rewrap into 'Eff es' via 'localUnliftIO env SeqForkUnlift' (same
    -- strategy that fixed the RunWithReplace storage-mismatch above).
    -- Then re-send to next handler.
    -- IntMap variant: 'f' is monomorphic, straightforward bind.
    TraverseIntMapWithKeyWithAdjustOp f m0 pev -> do
      wrappedF <- ED.localUnliftIO env E.SeqForkUnlift $ \unliftIO -> pure
        (\k v -> MIO.liftIO (unliftIO (f k v)))
      ED.send @(HE.Adjustable (SI.SpiderTimeline x))
        (TraverseIntMapWithKeyWithAdjustOp wrappedF m0 pev)

    -- DMap variants: 'f' is rank-2 ('forall a. k a -> v a -> m (v' a)').
    -- 'pure' would monomorphize the rank-2 fn on the way out of the
    -- 'localUnliftIO' IO callback. Wrap in a Rank2 newtype to hide the
    -- polymorphism from 'pure', then unwrap on the other side.
    TraverseDMapWithKeyWithAdjustOp f m0 pev -> do
      Rank2 wrappedF <- ED.localUnliftIO env E.SeqForkUnlift $ \unliftIO -> pure
        (Rank2 (\k v -> MIO.liftIO (unliftIO (f k v))))
      ED.send @(HE.Adjustable (SI.SpiderTimeline x))
        (TraverseDMapWithKeyWithAdjustOp wrappedF m0 pev)

    TraverseDMapWithKeyWithAdjustWithMoveOp f m0 pev -> do
      Rank2 wrappedF <- ED.localUnliftIO env E.SeqForkUnlift $ \unliftIO -> pure
        (Rank2 (\k v -> MIO.liftIO (unliftIO (f k v))))
      ED.send @(HE.Adjustable (SI.SpiderTimeline x))
        (TraverseDMapWithKeyWithAdjustWithMoveOp wrappedF m0 pev)


-- | Requester-specific per-cohort wrap.
--
-- Mirrors 'HE.runAdjustableInRequesterT' body lines 837-907 as a standalone
-- helper composable in a discharge/interpose cascade.
--
--   (a) allocate a fresh 'RB.RequesterState' IORef scoped to this cohort;
--   (b) 'interpose' on 'HE.Requester' — Requesting_ writes to fresh IORef
--       instead of the ambient outer state;
--   (c) run @child@ under the interpose;
--   (d) snapshot IORef, build aggregation event via 'R.mergeInt' wrapped in
--       'RB.RequesterData . RB.TagMap'.
--
-- Response-carrying paths (Requesting / RequestingBundled / RequestingBundled3)
-- return 'R.never' — matches upstream's deferred-response semantics for the
-- fire-and-forget 'performEvent_' case (the mainstream flow).
--
-- The outer caller ('withAdjustableRequester') routes the returned aggEv to
-- the outer Requester via 'IntMap.singleton n' + 'RB.multiEntry' +
-- 'HE.requestingBundledE'.
requesterCohortWrap
  :: forall x req rsp es a.
     ( SI.HasSpiderTimeline x
     , R.Reflex (SI.SpiderTimeline x)
     , E.IOE :> es
     , HE.Requester (SI.SpiderTimeline x) req rsp :> es
     )
  => CohortEnv
  -> Eff es a
  -> Eff es (a, R.Event (SI.SpiderTimeline x) (RB.RequesterData req))
requesterCohortWrap _cid child = do
  childStateRef <- MIO.liftIO $ IORef.newIORef
    (RB.RequesterState (-4) [] :: RB.RequesterState (SI.SpiderTimeline x) Exts.Any)
  let freshHandler
        :: ED.EffectHandler
             (HE.Requester (SI.SpiderTimeline x) req rsp)
             es
      freshHandler _ (Requesting_ evReqA) = MIO.liftIO $ do
        state <- IORef.readIORef childStateRef
        let n = RB._requesterState_nextMyTag state
                .|. RB.myTagTypeOffset (Proxy.Proxy :: Proxy.Proxy (RB.Single Exts.Any))
            evReqBoxed :: R.Event (SI.SpiderTimeline x) Exts.Any
            evReqBoxed = Coerce.unsafeCoerce (R.fmapCheap RB.singleEntry evReqA)
        IORef.writeIORef childStateRef $ state
          { RB._requesterState_nextMyTag = RB._requesterState_nextMyTag state - 4
          , RB._requesterState_requests  =
              (n, evReqBoxed) : RB._requesterState_requests state
          }
      freshHandler _ (Requesting _) =
        pure (Coerce.unsafeCoerce (R.never :: R.Event (SI.SpiderTimeline x) Exts.Any))
      freshHandler _ (RequestingBundled _) =
        pure (Coerce.unsafeCoerce
          (R.never :: R.Event (SI.SpiderTimeline x) (RB.Entry rsp RB.Multi)))
      freshHandler _ (RequestingBundled3 _) =
        pure (Coerce.unsafeCoerce
          (R.never :: R.Event (SI.SpiderTimeline x) (RB.Entry rsp RB.Multi3)))
  result <- ED.interpose
    @(HE.Requester (SI.SpiderTimeline x) req rsp)
    freshHandler
    child
  s <- MIO.liftIO $ IORef.readIORef childStateRef
  let aggEv = R.fmapCheap (RB.RequesterData . RB.TagMap) $
                R.mergeInt $ IntMap.fromDistinctAscList
                  (RB._requesterState_requests s)
  pure (result, aggEv)


--------------------------------------------------------------------------------
-- withAdjustableQuery
--
-- Same architectural shape as 'withAdjustableRequester', different per-effect
-- logic. Proves the pattern generalizes — cascade layering works for any
-- effect that wants per-cohort scoping.
--------------------------------------------------------------------------------

withAdjustableQuery
  :: forall x q es a.
     ( SI.HasSpiderTimeline x
     , R.Reflex (SI.SpiderTimeline x)
     , RQuery.Query q
     , RP.Group q, Commutative q, Monoid q, Semigroup q
     , R.Patch (RQuery.QueryResult q)
     , E.IOE :> es
     , HE.Adjustable (SI.SpiderTimeline x) :> es
     , HE.Query (SI.SpiderTimeline x) q :> es
     )
  => Eff es a -> Eff es a
withAdjustableQuery =
  ED.interpose @(HE.Adjustable (SI.SpiderTimeline x)) $ \env -> \case

    -- Strategy = SeqForkUnlift (same as withAdjustableRequester). Was
    -- 'localSeqUnlift' which produces an ephemeral unlift that dies at
    -- block exit — replacement callbacks fire in a later Reflex frame
    -- (pushCheap inside the fused runner) and hit 'version/storageVersion'
    -- mismatch. 'localUnliftIO env SeqForkUnlift' survives the async
    -- boundary. See 'withAdjustableRequester' + fused-runner comments
    -- (HeadlessEffectful.hs:877-889).
    RunWithReplace a0 a' -> do
      (wrappedInitial, wrappedRepls) <-
        ED.localUnliftIO env E.SeqForkUnlift $ \unliftIO -> pure
          ( do  -- wrappedInitial :: Eff es (a1, Incremental t (AdditivePatch q))
              cid <- MIO.liftIO newCohortEnv
              queryCohortWrap @x @q cid
                (MIO.liftIO (unliftIO a0))
          , R.fmapCheap
              (\r -> do
                  cid <- MIO.liftIO newCohortEnv
                  queryCohortWrap @x @q cid
                    (MIO.liftIO (unliftIO r)))
              a'
          )

      ((childVal, aggInit), perCohortEv)
        <- ED.send @(HE.Adjustable (SI.SpiderTimeline x))
             (RunWithReplace wrappedInitial wrappedRepls)

      -- ROUTE the initial cohort's aggregated queries to the OUTER Query.
      HE.tellQueryIncrementalE @(SI.SpiderTimeline x) @q aggInit

      pure (childVal, R.fmapCheap fst perCohortEv)

    -- Traverse variants: pass-through re-send to next handler down. Same
    -- pattern as 'withAdjustableRequester' — the fused runner handles the
    -- per-cohort machinery; we just rewrap the child fn's monad row.
    TraverseIntMapWithKeyWithAdjustOp f m0 pev -> do
      wrappedF <- ED.localUnliftIO env E.SeqForkUnlift $ \unliftIO -> pure
        (\k v -> MIO.liftIO (unliftIO (f k v)))
      ED.send @(HE.Adjustable (SI.SpiderTimeline x))
        (TraverseIntMapWithKeyWithAdjustOp wrappedF m0 pev)

    TraverseDMapWithKeyWithAdjustOp f m0 pev -> do
      Rank2 wrappedF <- ED.localUnliftIO env E.SeqForkUnlift $ \unliftIO -> pure
        (Rank2 (\k v -> MIO.liftIO (unliftIO (f k v))))
      ED.send @(HE.Adjustable (SI.SpiderTimeline x))
        (TraverseDMapWithKeyWithAdjustOp wrappedF m0 pev)

    TraverseDMapWithKeyWithAdjustWithMoveOp f m0 pev -> do
      Rank2 wrappedF <- ED.localUnliftIO env E.SeqForkUnlift $ \unliftIO -> pure
        (Rank2 (\k v -> MIO.liftIO (unliftIO (f k v))))
      ED.send @(HE.Adjustable (SI.SpiderTimeline x))
        (TraverseDMapWithKeyWithAdjustWithMoveOp wrappedF m0 pev)


-- | Query-specific per-cohort wrap. Mirrors 'HE.runQueryE'
-- (HeadlessEffectful.hs:4262-4307): accumulate per-cohort
-- 'tellQueryIncremental' behaviors + updated-events into two IORef lists,
-- then build the aggregate 'Incremental t (AdditivePatch q)' at the end.
--
-- Interpose on 'HE.Query':
--   * 'TellQueryIncremental inc' — capture (current inc, updated inc) into
--     per-cohort state (does NOT re-send to outer; outer sees only the
--     final aggregate via 'HE.tellQueryIncrementalE' from
--     'withAdjustableQuery').
--   * 'AskQueryResult' — delegate to outer via re-'send' (per-cohort shares
--     the outer response Dynamic; upstream MTL @withQueryT@ semantics).
--   * 'QueryIncremental inc' — capture + delegate ask (compound behavior).
queryCohortWrap
  :: forall x q es a.
     ( SI.HasSpiderTimeline x
     , R.Reflex (SI.SpiderTimeline x)
     , RQuery.Query q
     , RP.Group q, Commutative q, Monoid q, Semigroup q
     , R.Patch (RQuery.QueryResult q)
     , E.IOE :> es
     , HE.Query (SI.SpiderTimeline x) q :> es
     )
  => CohortEnv
  -> Eff es a
  -> Eff es (a, R.Incremental (SI.SpiderTimeline x) (RP.AdditivePatch q))
queryCohortWrap _cid child = do
  behRef <- MIO.liftIO $ IORef.newIORef
    ([] :: [R.Behavior (SI.SpiderTimeline x) q])
  evRef  <- MIO.liftIO $ IORef.newIORef
    ([] :: [R.Event (SI.SpiderTimeline x) q])
  let freshHandler
        :: ED.EffectHandler (HE.Query (SI.SpiderTimeline x) q) es
      freshHandler _ (HE.TellQueryIncremental inc) = MIO.liftIO $ do
        IORef.modifyIORef' behRef (R.currentIncremental inc :)
        IORef.modifyIORef' evRef
          (R.fmapCheap RP.unAdditivePatch (R.updatedIncremental inc) :)
      freshHandler _ HE.AskQueryResult =
        ED.send @(HE.Query (SI.SpiderTimeline x) q) HE.AskQueryResult
      freshHandler _ (HE.QueryIncremental inc) = do
        MIO.liftIO $ do
          IORef.modifyIORef' behRef (R.currentIncremental inc :)
          IORef.modifyIORef' evRef
            (R.fmapCheap RP.unAdditivePatch (R.updatedIncremental inc) :)
        r <- ED.send @(HE.Query (SI.SpiderTimeline x) q) HE.AskQueryResult
        pure (R.zipDynWith RQuery.crop (R.incrementalToDynamic inc) r)
  result <- ED.interpose @(HE.Query (SI.SpiderTimeline x) q) freshHandler child
  (behs, evs) <- MIO.liftIO $ do
    b <- IORef.readIORef behRef
    e <- IORef.readIORef evRef
    pure (b, e)
  let mergedEv = R.mergeWith (<>) evs
      aggIncr  = R.unsafeBuildIncremental
                   (Foldable.foldlM (\acc c -> (acc <>) <$> R.sample c) mempty behs)
                   (R.fmapCheap RP.AdditivePatch mergedEv)
  pure (result, aggIncr)


--------------------------------------------------------------------------------
-- ARCHITECTURE PROOF — example composition
--
-- Widget code sees ONE 'HE.Adjustable t' + normal 'HE.Requester' /
-- 'HE.Query' in its row. Interposers stack transparently. Widget's single
-- 'runWithReplace' call cascades through every interposer, each adding its
-- per-effect cohort setup, before hitting the base handler.
--
-- The type signature of 'exampleComposition' below is what proves the shape
-- compiles — no 'undefined' in the composition itself.
--------------------------------------------------------------------------------

-- | Example composition — demonstrates the interpose cascade shape.
--
-- >   runAdjustable . withAdjustableRequester . withAdjustableQuery $ widget
--
-- Handler stack at runtime (top = catches send first):
--
-- >   +----------------------+
-- >   | withAdjustableQuery  |  ← rightmost in .-chain, TOPMOST
-- >   +----------------------+
-- >   | withAdjustableReq    |
-- >   +----------------------+
-- >   | runAdjustable (base) |  ← leftmost, INNERMOST, only peels Adjustable
-- >   +----------------------+
--
-- Widget's 'runWithReplace' call:
--   1. Query interposer fires — wraps child with Query cohort setup, re-sends.
--   2. Requester interposer fires — wraps (already Query-wrapped) child with
--      Requester cohort setup, re-sends.
--   3. Base runAdjustable fires — runs the fully-wrapped child.
--
-- Interpose vs interpret rule: only ONE 'runAdjustable' (interpret peels the
-- Adjustable effect). ANY NUMBER of interposers (interpose layers on top of
-- an already-present effect).
{- BEGIN OLD-PERFORMEVENT-COMMENTED-OUT exampleComposition (superseded by version below).
Task #268: OLD 'HE.PerformEvent x :> es' constraint migrated to 'HE.PerformEventEff subEs
(SI.SpiderTimeline x) :> es'. Preserved verbatim.

exampleComposition
  :: forall x req rsp q es a.
     ( SI.HasSpiderTimeline x
     , R.Reflex (SI.SpiderTimeline x)
     , RQuery.Query q
     , RP.Group q, Commutative q, Monoid q, Semigroup q
     , R.Patch (RQuery.QueryResult q)
     , E.IOE :> es
     , HE.PerformEvent x :> es
     , HE.Requester (SI.SpiderTimeline x) req rsp :> es
     , HE.Query     (SI.SpiderTimeline x) q :> es
     )
  => Eff (HE.Adjustable (SI.SpiderTimeline x) : es) a
  -> Eff es a
exampleComposition
  = HE.runAdjustable
  . withAdjustableRequester @x @req @rsp
  . withAdjustableQuery     @x @q

-- NEW timeline-polymorphic 'exampleComposition' deferred: the underlying helpers
-- 'HE.runAdjustable' and 'withAdjustableRequester' / 'withAdjustableQuery' are
-- still Spider-tag-parameterized (@x@). A t-polymorphic 'exampleComposition' can
-- only be written after those helpers are migrated to be timeline-polymorphic
-- (@forall t. ReflexHost t@) — separate wave. No Spider-hardcoded example is
-- shipped here (per user directive 2026-09-01: never force SpiderTimeline).
END OLD-PERFORMEVENT-COMMENTED-OUT exampleComposition -}
