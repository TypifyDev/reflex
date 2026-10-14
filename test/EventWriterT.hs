{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (SomeException, try)
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import qualified Data.Dependent.Map as DMap
import Data.Functor.Misc
import qualified Data.Map as M
import Data.These

#if defined(MIN_VERSION_these_lens) || (MIN_VERSION_these(0,8,0) && !MIN_VERSION_these(0,9,0))
import Data.These.Lens
#endif

import Reflex
import Reflex.EventWriter.Base
import Test.Run

-- Run same body on upstream + native, print both. requireAgree=True → error on mismatch.
runBoth
  :: forall a. (Show a, Eq a)
  => Bool -> String -> IO a -> IO a -> IO a
runBoth requireAgree label upstream native = do
  putStrLn ("--- " <> label <> " ---")
  osU <- upstream
  putStrLn ("  upstream: " <> show osU)
  eOsN <- try native
  case (eOsN :: Either SomeException a) of
    Left e -> putStrLn ("  native:   <exception> " <> show e)
    Right o -> putStrLn ("  native:   " <> show o)
  if requireAgree
    then case (eOsN :: Either SomeException a) of
      Left e -> error (label <> ": native threw " <> show e)
      Right osN ->
        if osU == osN
          then pure osU
          else error (label <> ": upstream/native disagree")
    else pure osU

main :: IO ()
main = do
  os1 <- runBoth True "testOrdering"
    (runApp'          (unwrapApp testOrdering) [Just ()])
    (runAppEffectful' (unwrapApp testOrdering) [Just ()])
  os2 <- runBoth True "testSimultaneous"
    (runApp'          (unwrapApp testSimultaneous) $ map Just [This (), That (), This (), These () ()])
    (runAppEffectful' (unwrapApp testSimultaneous) $ map Just [This (), That (), This (), These () ()])
  os3 <- runBoth True "testMoribundTellEvent"
    (runApp'          (unwrapApp testMoribundTellEvent) [Just ()])
    (runAppEffectful' (unwrapApp testMoribundTellEvent) [Just ()])
  os4 <- runBoth True "testMoribundTellEventDMap"
    (runApp'          (unwrapApp testMoribundTellEventDMap) [Just ()])
    (runAppEffectful' (unwrapApp testMoribundTellEventDMap) [Just ()])
  os5 <- runBoth True "testLiveTellEventDMap"
    (runApp'          (unwrapApp testLiveTellEventDMap) [Just ()])
    (runAppEffectful' (unwrapApp testLiveTellEventDMap) [Just ()])
  os6 <- runBoth True "delayedPulse"
    (runApp'          (unwrapApp delayedPulse) [Just ()])
    (runAppEffectful' (unwrapApp delayedPulse) [Just ()])
  -- Upstream assertions preserved.
  let ![[Just [10,9,8,7,6,5,4,3,2,1]]] = os1
  let ![[Just [1,3,5,7,9]],[Nothing,Nothing],[Just [2,4,6,8,10]],[Just [2,4,6,8,10],Nothing]] = os2
  let ![[Nothing, Just [2]]] = os3
  let ![[Nothing, Just [2]]] = os4
  let ![[Nothing, Just [1, 2]]] = os5
  let ![[Nothing, Nothing]] = os6
  putStrLn "All EventWriterT upstream assertions passed; native runs exercised."
  return ()

unwrapApp :: (Reflex t, Monad m) => (a -> EventWriterT t [Int] m ()) -> a -> m (Event t [Int])
unwrapApp x appIn = do
  ((), e) <- runEventWriterT $ x appIn
  return e

testOrdering :: (Reflex t, Monad m) => Event t () -> EventWriterT t [Int] m ()
testOrdering pulse = forM_ [10,9..1] $ \i -> tellEvent ([i] <$ pulse)

testSimultaneous :: (Reflex t, Adjustable t m, MonadHold t m) => Event t (These () ()) -> EventWriterT t [Int] m ()
testSimultaneous pulse = do
  let e0 = fmapMaybe (^? here) pulse
      e1 = fmapMaybe (^? there) pulse
  forM_ [1,3..9] $ \i -> runWithReplace (tellEvent ([i] <$ e0)) $ ffor e1 $ \_ -> tellEvent ([i+1] <$ e0)

-- | Test that a widget telling and event which fires at the same time it has been replaced
-- doesn't count along with the new widget.
testMoribundTellEvent
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m
     )
  => Event t ()
  -> EventWriterT t [Int] m ()
testMoribundTellEvent pulse = do
  rec let tellIntOnReplace :: Int -> EventWriterT t [Int] m ()
          tellIntOnReplace x = tellEvent $ [x] <$ rwrFinished
      (_, rwrFinished) <- runWithReplace (tellIntOnReplace 1) $ tellIntOnReplace 2 <$ pulse
  return ()

-- | The equivalent of 'testMoribundTellEvent' for 'traverseDMapWithKeyWithAdjust'.
testMoribundTellEventDMap
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m
     )
  => Event t ()
  -> EventWriterT t [Int] m ()
testMoribundTellEventDMap pulse = do
  rec let tellIntOnReplace :: Int -> EventWriterT t [Int] m ()
          tellIntOnReplace x = tellEvent $ [x] <$ rwrFinished
      (_, rwrFinished :: Event t (PatchDMap (Const2 () Int) Identity)) <-
        traverseDMapWithKeyWithAdjust
          (\(Const2 ()) (Identity v) -> Identity . const v <$> tellIntOnReplace v)
          (mapToDMap $ M.singleton () 1)
          ((PatchDMap $ DMap.map (ComposeMaybe . Just) $ mapToDMap $ M.singleton () 2) <$ pulse)
  return ()

-- | Ensures that elements which are _not_ removed can still fire 'tellEvent's
-- during the same frame as other elements are updated.
testLiveTellEventDMap
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m
     )
  => Event t ()
  -> EventWriterT t [Int] m ()
testLiveTellEventDMap pulse = do
  rec let tellIntOnReplace :: Int -> EventWriterT t [Int] m ()
          tellIntOnReplace x = tellEvent $ [x] <$ rwrFinished
      (_, rwrFinished :: Event t (PatchDMap (Const2 Int ()) Identity)) <-
        traverseDMapWithKeyWithAdjust
          (\(Const2 k) (Identity ()) -> Identity <$> tellIntOnReplace k)
          (mapToDMap $ M.singleton 1 ())
          ((PatchDMap $ DMap.map (ComposeMaybe . Just) $ mapToDMap $ M.singleton 2 ()) <$ pulse)
  return ()

delayedPulse
  :: forall t m
  .  ( Reflex t
     , Adjustable t m
     , MonadHold t m
     , MonadFix m
     )
  => Event t ()
  -> EventWriterT t [Int] m ()
delayedPulse pulse = void $ flip runWithReplace (pure () <$ pulse) $ do
    -- This has the effect of delaying pulse' from pulse
    (_, pulse') <- runWithReplace (pure ()) $ pure [1] <$ pulse
    tellEvent pulse'
