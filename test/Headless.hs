-- Regression tests for both the upstream MTL-based 'Reflex.Host.Headless'
-- and the native Effectful-based 'Reflex.Host.HeadlessEffectful'. Same
-- FRP logic exercised through both hosts so regressions in either surface
-- from the same test file.

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Effectful (Eff, IOE)

import Reflex
import qualified Reflex.Host.Headless           as Upstream
import qualified Reflex.Host.HeadlessEffectful  as Native
import Reflex.Spider (Global, SpiderTimeline)
import Reflex.Spider.Internal (HasSpiderTimeline, SpiderHostFrame)

-- Test 4 imports: OLD 'Adjustable t' send helper. Nested runWithReplace
-- compiles against this shape (opaque 'm' filled by Effectful dispatch —
-- your proposed "just take Eff es and let the interpreter provide the
-- discharge function" pattern IS what this effect encodes). Contrast:
-- 'AdjustableE (sub :: [Effect]) t' at HeadlessEffectful.hs:592 is the
-- variant that parameterizes sub row at the type level, which structurally
-- can't type nested widgets due to recursive-row expansion.
import Reflex.Adjustable.Class (runWithReplace)
import Reflex.Host.HeadlessEffectful (HeadlessRow)

--------------------------------------------------------------------------------
-- Test 1: post-build fires; guest returns immediately.
--
-- Same guest for both hosts. Upstream signature uses MTL constraint
-- bundle; native uses the effect-row signature. We provide two
-- variants (a MTL-guest and a native-guest) that do the same thing.
--------------------------------------------------------------------------------

testGuestUpstream
  :: forall t m. Upstream.MonadHeadlessApp t m
  => IORef Int
  -> m (Event t ())
testGuestUpstream counter = do
  pb <- getPostBuild
  performEvent_ (liftIO (atomicModifyIORef' counter (\c -> (c + 1, ()))) <$ pb)
  return pb

testGuestNative
  :: forall t. Reflex t
  => IORef Int
  -> Eff (Native.HeadlessRow '[IOE] t)
       (Event t ())
testGuestNative counter = do
  pb <- Native.getPostBuildE @t
  Native.performEventEffE_ @'[IOE]
    ( fmap (\_ -> liftIO (atomicModifyIORef' counter (\c -> (c + 1, ()))))
           pb :: Event t (Eff '[IOE] ())
    )
  return pb

-- | Guest for the NEW 'runHeadlessAppE' composer that uses upstream-
-- faithful 'runAdjustableE' (Multi tag + fanInt + holdDynE +
-- switchPromptlyDyn — mirrors upstream 'runWithReplaceRequesterTWith').
-- Sub-widget row is 'HeadlessSubEsE' (differs from 'HeadlessRow' —
-- no AdjustableE at the row's head).
testGuestNativeE
  :: forall x. HasSpiderTimeline x
  => IORef Int
  -> Eff (Native.HeadlessSubEsE x)
       (Event (SpiderTimeline x) ())
testGuestNativeE counter = do
  pb <- Native.getPostBuildE @(SpiderTimeline x)
  Native.performEventEffE_ @'[IOE]
    ( fmap (\_ -> liftIO (atomicModifyIORef' counter (\c -> (c + 1, ()))))
           pb :: Event (SpiderTimeline x) (Eff '[IOE] ())
    )
  return pb

-- | Guest for the standalone AdjustableE composer 'runHeadlessAppEStandalone'.
-- Same postBuild + performEvent_ semantics as the other guests.
-- Widget row is 'HeadlessSubEsEStandalone' which INCLUDES 'Requester' in
-- sub (option (b) — merged Requester + PerformEvent per cohort).
testGuestNativeEStandalone
  :: forall x. HasSpiderTimeline x
  => IORef Int
  -> Eff (Native.HeadlessSubEsEStandalone x)
       (Event (SpiderTimeline x) ())
testGuestNativeEStandalone counter = do
  pb <- Native.getPostBuildE @(SpiderTimeline x)
  Native.performEventEffE_ @'[IOE]
    ( fmap (\_ -> liftIO (atomicModifyIORef' counter (\c -> (c + 1, ()))))
           pb :: Event (SpiderTimeline x) (Eff '[IOE] ())
    )
  return pb

--------------------------------------------------------------------------------
-- Test 4: NESTED runWithReplace demo.
--
-- Positive result: nested 'runWithReplace' COMPILES against the OLD
-- 'Adjustable t' effect ('HeadlessEffectful.hs:526', opaque 'm'
-- parameter). This is the "take Eff es, let interpreter supply the
-- discharge function via localUnliftIO" pattern — exactly what the
-- MTL-style opaque-m encoding gives you, and exactly what routes the
-- widget stack today (via 'HH.runWithReplaceE' at DomHost.hs:3587,
-- landed to fix HydrationBrowser test 'can be nested in postBuild
-- widget').
--
-- We do a nested runWithReplace inside a runWithReplace, in the outer's
-- REPLACEMENT slot (fired by pb). The OUTER handler discharges its own
-- Requester scope; the INNER call inside the replacement's body hits
-- the same handler again — no recursive-row shenanigans because
-- 'Adjustable t' has no sub-row parameter.
--
-- Contrast: 'AdjustableE (sub :: [Effect]) t' at HeadlessEffectful.hs:592
-- CAN'T type this pattern — see docs at HeadlessEffectful.hs:3539-3610
-- describing why the recursive row diverges GHC.
--------------------------------------------------------------------------------

testGuestNativeNested
  :: forall t. Reflex t
  => IORef Int
  -> Eff (HeadlessRow '[IOE] t) (Event t ())
testGuestNativeNested counter = do
  pb <- Native.getPostBuildE @t
  let bumpCounter :: Eff (HeadlessRow '[IOE] t) ()
      bumpCounter = liftIO (atomicModifyIORef' counter (\c -> (c + 1, ())))
  -- OUTER runWithReplace: initial widget does nothing; replacement fires
  -- on postBuild and installs an INNER runWithReplace inside its body.
  -- Type applications elided — inference handles it since 'runWithReplace'
  -- is a class method (Reflex.Adjustable.Class) and 'HeadlessRow' fixes
  -- the Adjustable instance.
  let innerBody :: Eff (HeadlessRow '[IOE] t) ()
      innerBody = do
        _ <- runWithReplace bumpCounter (never :: Event t (Eff (HeadlessRow '[IOE] t) ()))
        pure ()
  _ <- runWithReplace
         (pure () :: Eff (HeadlessRow '[IOE] t) ())
         (ffor pb $ \_ -> innerBody)
  pure pb

--------------------------------------------------------------------------------
-- Runner
--------------------------------------------------------------------------------

runOne :: String -> IO () -> IORef Int -> IO ()
runOne label act counter = do
  putStrLn ("--- " ++ label ++ " ---")
  act
  n <- readIORef counter
  putStrLn (label ++ ": performEvent_ callback ran " ++ show n ++ " time(s)")
  if n >= 1
    then putStrLn (label ++ ": OK")
    else error (label ++ ": FAIL — performEvent_ did not run")

main :: IO ()
main = do
  -- Test 1: upstream host, MTL guest.
  counterU <- newIORef 0
  runOne "upstream MTL"
    (Upstream.runHeadlessApp (testGuestUpstream counterU))
    counterU

  -- Test 2: native effectful host, native guest.
  counterN <- newIORef 0
  runOne "native Effectful"
    (Native.runHeadlessApp (testGuestNative counterN))
    counterN

  counterNE <- newIORef 0
  runOne "native Effectful (new AdjustableE path)"
    (Native.runHeadlessAppE (testGuestNativeE counterNE))
    counterNE

  counterNES <- newIORef 0
  runOne "native Effectful (AdjustableE standalone path)"
    (Native.runHeadlessAppEStandalone (testGuestNativeEStandalone counterNES))
    counterNES

  -- Test 4: nested runWithReplace via OLD 'Adjustable t' effect.
  -- Compiling is the primary signal (proves the shape works for nested).
  -- Running exercises the per-cohort scope wiring in the OLD handler.
  counterNested <- newIORef 0
  runOne "native Effectful nested runWithReplace (old Adjustable, opaque m)"
    (Native.runHeadlessApp (testGuestNativeNested counterNested))
    counterNested

  putStrLn "All tests passed."
