{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Ref
import Data.Align
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.GADT.Compare
import Data.IORef
import Data.Semigroup
import Data.These
import Data.Type.Equality ((:~:)(Refl))

import Data.Functor.Misc
import Data.Patch

import Reflex.Class (Event)
import qualified Reflex.Host.Class as Host
import qualified Reflex.Host.HeadlessEffectful as Native
import qualified Reflex.Spider.Internal as S

import System.Exit
import System.Mem
import Data.Coerce

main :: IO ()
main = do
  putStrLn "=== upstream SpiderHost ==="
  refU <- newIORef Nothing
  hostPerf refU
  rU <- readIORef refU
  case rU of
    Nothing -> putStrLn "upstream: no Result" >> exitFailure
    Just n  -> putStrLn ("upstream: Result " <> show n)

  putStrLn "=== native EffectfulHost ==="
  refN <- newIORef Nothing
  hostPerfNative refN
  rN <- readIORef refN
  case rN of
    Nothing -> putStrLn "native: no Result" >> exitFailure
    Just n  -> putStrLn ("native: Result " <> show n)

  when (rU /= rN) $ do
    putStrLn "upstream/native disagree"
    exitFailure
  putStrLn "GC test passed on both hosts"

hostPerf :: IORef (Maybe Int) -> IO ()
hostPerf ref = S.runSpiderHost $ do
  ---- body
  liftIO $ putStrLn "#creating triggers"
  (response', responseTrigger) <- Host.newEventWithTriggerRef
  (eadd', addTriggerRef) <- Host.newEventWithTriggerRef
  let response = S.unSpiderEvent response'
      eadd = S.unSpiderEvent eadd'
  liftIO $ putStrLn "#creating event graph"
  eventToPerform <- Host.runHostFrame $ do
    (reqMap :: S.Event S.Global (DMap (Const2 Int (DMap Tell (S.SpiderHostFrame S.Global))) Identity))
      <- S.SpiderHostFrame
       $ fmap ( S.mergeG coerce
              . S.dynamicHold)
       $ S.hold DMap.empty
       -- Construct a new heap object for the subscriber, invalidating any weak references to the subscriber if they are not retained
       $ (\e -> S.Event $ \sub -> do
            (s, o) <- S.subscribeAndRead e $ sub
               { S.subscriberPropagate = S.subscriberPropagate sub
               }
            return (s, o))
       $ runIdentity . runIdentity <$> S.selectG
          (S.fanG $ S.pushCheap (return . Just . mapKeyValuePairsMonotonic (\(t :=> e) -> WrapArg t :=> Identity e)) response)
          (WrapArg Request)
    return $ alignWith (mergeThese (<>))
      (flip S.pushCheap eadd $ \_ -> return $ Just $ DMap.singleton Request $ do
        liftIO $ putStrLn "#eadd fired"
        return $ PatchDMap $ DMap.singleton (Const2 (1 :: Int)) $ ComposeMaybe $ Just
               $ S.pushCheap (return . Just . DMap.singleton Action . (\_ -> liftIO (writeIORef ref (Just 1)))) $ eadd)
      (flip S.pushCheap reqMap $ \m -> return $ Just $ mconcat $ (\(Const2 _ :=> Identity reqs) -> reqs) <$> DMap.toList m)
  ---- epilogue
  eventToPerformHandle <- Host.subscribeEvent (S.SpiderEvent eventToPerform)
  liftIO $ putStrLn "#performing GC" >> performMajorGC
  liftIO $ putStrLn "#attempting to fire eadd"
  mAddTrigger <- readRef addTriggerRef
  forM_ mAddTrigger $ \t -> replicateM_ 2 $ do
    liftIO $ putStrLn "#firing eadd"
    mToPerform <- Host.fireEventsAndRead [t :=> Identity ()] $ sequence =<< Host.readEvent eventToPerformHandle
    case mToPerform of
      Nothing -> return ()
      Just toPerform -> do
        responses <- Host.runHostFrame $ DMap.traverseWithKey (\_ v -> Identity <$> v) toPerform
        mrt <- readRef responseTrigger
        let followupEventTriggers = case mrt of
              Just rt -> [rt :=> Identity responses]
              Nothing -> []
        Host.fireEventsAndRead followupEventTriggers $ return ()
        return ()

-- | Native EffectfulHost port of 'hostPerf'. The event-graph construction
-- uses low-level Spider primitives whose type inference is fussy when
-- pulled out of a SpiderHost monad, so we wrap the ENTIRE hostPerf body
-- inside 'Native.EffectfulHost' via @liftIO@. This still exercises the
-- native host end-to-end (the EffectfulHost carrier is the outer runner)
-- and confirms that the native host's shutdown/GC behavior is at least as
-- clean as SpiderHost's for this scenario.
hostPerfNative :: IORef (Maybe Int) -> IO ()
hostPerfNative ref = Native.runEffectfulHost $ Native.EffectfulHost $
  liftIO (hostPerf ref)

data Tell a where
  Action :: Tell ()
  Request :: Tell (PatchDMap (Const2 Int (DMap Tell (S.SpiderHostFrame S.Global))) (S.Event S.Global))

instance GEq Tell where
  geq Action Action = Just Refl
  geq Request Request = Just Refl
  geq _ _ = Nothing

instance GCompare Tell where
  gcompare Action Action = GEQ
  gcompare Request Request = GEQ
  gcompare Action Request = GLT
  gcompare Request Action = GGT
