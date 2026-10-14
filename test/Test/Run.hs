{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Test.Run where

import Control.Concurrent.Chan (newChan)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Ref
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.IORef
import Data.These

import Effectful (Eff, IOE)

import Reflex
import Reflex.Host.Class
import qualified Reflex.Host.HeadlessEffectful as Native
import Reflex.Requester.Base (RequesterData, traverseRequesterData)
import qualified Reflex.Spider.Internal as SI
import Reflex.Spider.Internal (HasSpiderTimeline)

data AppIn t b e = AppIn
  { _appIn_behavior :: Behavior t b
  , _appIn_event :: Event t e
  }

data AppOut t b e = AppOut
  { _appOut_behavior :: Behavior t b
  , _appOut_event :: Event t e
  }

runApp :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
       => (AppIn t bIn eIn -> PerformEventT t m (AppOut t bOut eOut))
       -> bIn
       -> [Maybe (These bIn eIn)]
       -> IO [[(bOut, Maybe eOut)]]
runApp app b0 input = runSpiderHost $ do
  (appInHoldE, pulseHoldTriggerRef) <- newEventWithTriggerRef
  (appInE, pulseEventTriggerRef) <- newEventWithTriggerRef
  appInB <- hold b0 appInHoldE
  (out, FireCommand fire) <- hostPerformEventT $ app $ AppIn
    { _appIn_event = appInE
    , _appIn_behavior = appInB
    }
  hnd <- subscribeEvent (_appOut_event out)
  mpulseB <- readRef pulseHoldTriggerRef
  mpulseE <- readRef pulseEventTriggerRef
  let readPhase = do
        b <- sample (_appOut_behavior out)
        frames <- sequence =<< readEvent hnd
        return (b, frames)
  forM input $ \case
    Nothing ->
      fire [] $ readPhase
    Just i -> case i of
      This b' -> case mpulseB of
        Nothing -> error "tried to fire in-behavior but ref was empty"
        Just pulseB -> fire [ pulseB :=> Identity b' ] $ readPhase
      That e' -> case mpulseE of
        Nothing -> error "tried to fire in-event but ref was empty"
        Just pulseE -> fire [ pulseE :=> Identity e' ] $ readPhase
      These b' e' -> case mpulseB of
        Nothing -> error "tried to fire in-behavior but ref was empty"
        Just pulseB -> case mpulseE of
          Nothing -> error "tried to fire in-event but ref was empty"
          Just pulseE -> fire [ pulseB :=> Identity b', pulseE :=> Identity e' ] $ readPhase

runApp' :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
        => (Event t eIn -> PerformEventT t m (Event t eOut))
        -> [Maybe eIn]
        -> IO [[Maybe eOut]]
runApp' app input = do
  let app' = fmap (AppOut (pure ())) . app
  map (map snd) <$> runApp (app' . _appIn_event) () (map (fmap That) input)

runAppB :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
        => (Event t eIn -> PerformEventT t m (Behavior t bOut))
        -> [Maybe eIn]
        -> IO [[bOut]]
runAppB app input = do
  let app' = fmap (flip AppOut never) . app
  map (map fst) <$> runApp (app' . _appIn_event) () (map (fmap That) input)

--------------------------------------------------------------------------------
-- Native-Effectful analogs of the runners above. Same semantics
-- (per-input fire + per-frame observation), but the guest is Eff-typed
-- and runs through 'Reflex.Host.HeadlessEffectful' with zero MTL.
--------------------------------------------------------------------------------

-- | 'runApp' analog for the native Effectful host. The guest is
-- @Eff (Native.HeadlessRow '[IOE] (SpiderTimeline x)) (AppOut (SpiderTimeline x) bOut eOut)@
-- — polymorphic in the fresh timeline @x@ so a fresh SpiderTimelineEnv is
-- created per invocation via 'SI.withSpiderTimeline'.
--
-- Task #268 migration: OLD body used the fused runner
-- 'Native.runPerformEvent' (now commented out). NEW body uses compositional
-- peels 'runAdjustable . runPerformEventEff . runRequester' per the same
-- pattern as 'HH.runHeadlessApp' and 'DH.hostPerformEventT'.
runAppEffectful
  :: (forall x. HasSpiderTimeline x
      => AppIn (SpiderTimeline x) bIn eIn
      -> Eff (Native.HeadlessRow '[IOE] (SpiderTimeline x))
             (AppOut (SpiderTimeline x) bOut eOut))
  -> bIn
  -> [Maybe (These bIn eIn)]
  -> IO [[(bOut, Maybe eOut)]]
runAppEffectful app b0 input =
  SI.withSpiderTimeline $ \(env :: SI.SpiderTimelineEnv x) ->
    Native.runEffectfulHostFor @x
      (do
        (appInHoldE, pulseHoldTrigRef) <- liftIO $ do
          rt <- newIORef Nothing
          e <- SI.newEventWithTriggerIO $ \t -> do
            writeIORef rt (Just t)
            pure (writeIORef rt Nothing)
          pure (SI.SpiderEvent e, rt)
        (appInE, pulseEventTrigRef) <- liftIO $ do
          rt <- newIORef Nothing
          e <- SI.newEventWithTriggerIO $ \t -> do
            writeIORef rt (Just t)
            pure (writeIORef rt Nothing)
          pure (SI.SpiderEvent e, rt)
        (respEv, respTrigRef) <- liftIO $ do
          rt <- newIORef Nothing
          e <- SI.newEventWithTriggerIO $ \t -> do
            writeIORef rt (Just t)
            pure (writeIORef rt Nothing)
          pure (SI.SpiderEvent e, rt)
        chan <- liftIO newChan
        appInB <- Native.EffectfulHost (Native.holdE b0 appInHoldE)
        ((out, aggActions), _extAggReqs) <- Native.EffectfulHost $
            Native.runTriggerEvent chan
          . Native.runPostBuild never
          . Native.runRequester (never :: Event (SpiderTimeline x) (RequesterData Identity))
          . Native.runPerformEventEff @'[IOE] @(SpiderTimeline x) respEv
          . Native.runAdjustable @'[IOE] @x
          $ app AppIn
              { _appIn_behavior = appInB
              , _appIn_event    = appInE
              }
        outHandle <- Native.EffectfulHost
          (Native.subscribeEventE @(SpiderTimeline x) (_appOut_event out))
        actionsHandle <- Native.EffectfulHost
          (Native.subscribeEventE @(SpiderTimeline x) aggActions)
        mpulseB <- liftIO (readIORef pulseHoldTrigRef)
        mpulseE <- liftIO (readIORef pulseEventTrigRef)
        let readPhase = do
              b <- sample (_appOut_behavior out)
              mE <- liftIO (readIORef (SI.spiderEventHandleValue outHandle))
              pure (b, mE)
        let fireBatch ts = fireAndReadE @x ts readPhase actionsHandle respTrigRef
        liftIO $ forM input $ \case
          Nothing -> fireBatch []
          Just (This b') -> case mpulseB of
            Nothing -> error "runAppEffectful: tried to fire in-behavior but ref was empty"
            Just p -> fireBatch [p :=> Identity b']
          Just (That e') -> case mpulseE of
            Nothing -> error "runAppEffectful: tried to fire in-event but ref was empty"
            Just p -> fireBatch [p :=> Identity e']
          Just (These b' e') -> case (mpulseB, mpulseE) of
            (Just pB, Just pE) -> fireBatch [pB :=> Identity b', pE :=> Identity e']
            _ -> error "runAppEffectful: tried to fire in-{behavior,event} but a ref was empty"
      )
      env

-- | Mirror of upstream 'hostPerformEventT's fire-loop @go@: fire triggers,
-- run readPhase, drain any 'performEvent'-scheduled 'SpiderHostFrame'
-- actions in follow-up frames until stable. Returns one readPhase result
-- per frame (the initial fire plus each follow-up frame).
fireAndReadE
  :: forall x r
   . HasSpiderTimeline x
  => [DSum (SI.RootTrigger x) Identity]
  -> SI.ResultM x r
  -> SI.SpiderEventHandle x (RequesterData (SI.SpiderHostFrame x))
  -> IORef (Maybe (SI.RootTrigger x (RequesterData Identity)))
  -> IO [r]
fireAndReadE fires readPhase actionsHandle respTrigRef = go fires
  where
    go ts = do
      (r, mToPerform) <- Native.runE ts $ do
        r <- readPhase
        pending <- liftIO (readIORef (SI.spiderEventHandleValue actionsHandle))
        pure (r, pending)
      case mToPerform of
        Nothing -> pure [r]
        Just toPerform -> do
          responses <- traverseRequesterData
            (\shf -> Identity <$> Native.frame @x (SI.runSpiderHostFrame shf))
            toPerform
          mrt <- readIORef respTrigRef
          let followupFires = case mrt of
                Just rt -> [rt :=> Identity responses]
                Nothing -> []
          (r :) <$> go followupFires

-- | 'runApp'' analog. Same relationship to 'runAppEffectful' as
-- upstream's 'runApp'' has to 'runApp'.
runAppEffectful'
  :: (forall x. HasSpiderTimeline x
      => Event (SpiderTimeline x) eIn
      -> Eff (Native.HeadlessRow '[IOE] (SpiderTimeline x))
             (Event (SpiderTimeline x) eOut))
  -> [Maybe eIn]
  -> IO [[Maybe eOut]]
runAppEffectful' app input = do
  map (map snd) <$>
    runAppEffectful
      (\ain -> AppOut (pure ()) <$> app (_appIn_event ain))
      ()
      (map (fmap That) input)

-- | 'runAppB' analog.
runAppBEffectful
  :: (forall x. HasSpiderTimeline x
      => Event (SpiderTimeline x) eIn
      -> Eff (Native.HeadlessRow '[IOE] (SpiderTimeline x))
             (Behavior (SpiderTimeline x) bOut))
  -> [Maybe eIn]
  -> IO [[bOut]]
runAppBEffectful app input = do
  map (map fst) <$>
    runAppEffectful
      (\ain -> flip AppOut never <$> app (_appIn_event ain))
      ()
      (map (fmap That) input)

--------------------------------------------------------------------------------
-- Task #272 M-variants: same shape as runAppEffectful* but the guest row
-- installs 'PerformEventM (SpiderHostFrame x) (SpiderTimeline x)' instead of
-- 'PerformEventEff '[IOE] t'. This shape satisfies the MTL 'RHPE.PerformEvent'
-- bridge that pins @Performable = SpiderHostFrame x@, so guests polymorphic
-- over MTL 'PerformEvent t m' + downstream 'MonadHold (Performable m)' etc.
-- (e.g. 'matchResponsesWithRequests' in RequesterT test) typecheck.
--
-- Discharge chain mirrors 'runHeadlessAppM': runRequester . runPerformEventM .
-- runAdjustableM.
--------------------------------------------------------------------------------

runAppEffectfulM
  :: (forall x. HasSpiderTimeline x
      => AppIn (SpiderTimeline x) bIn eIn
      -> Eff (Native.HeadlessRowM (SpiderTimeline x))
             (AppOut (SpiderTimeline x) bOut eOut))
  -> bIn
  -> [Maybe (These bIn eIn)]
  -> IO [[(bOut, Maybe eOut)]]
runAppEffectfulM app b0 input =
  SI.withSpiderTimeline $ \(env :: SI.SpiderTimelineEnv x) ->
    Native.runEffectfulHostFor @x
      (do
        (appInHoldE, pulseHoldTrigRef) <- liftIO $ do
          rt <- newIORef Nothing
          e <- SI.newEventWithTriggerIO $ \t -> do
            writeIORef rt (Just t)
            pure (writeIORef rt Nothing)
          pure (SI.SpiderEvent e, rt)
        (appInE, pulseEventTrigRef) <- liftIO $ do
          rt <- newIORef Nothing
          e <- SI.newEventWithTriggerIO $ \t -> do
            writeIORef rt (Just t)
            pure (writeIORef rt Nothing)
          pure (SI.SpiderEvent e, rt)
        (respEv, respTrigRef) <- liftIO $ do
          rt <- newIORef Nothing
          e <- SI.newEventWithTriggerIO $ \t -> do
            writeIORef rt (Just t)
            pure (writeIORef rt Nothing)
          pure (SI.SpiderEvent e, rt)
        chan <- liftIO newChan
        appInB <- Native.EffectfulHost (Native.holdE b0 appInHoldE)
        -- Task #273: use fused runner instead of split peels.
        (out, aggActions) <- Native.EffectfulHost $
            Native.runTriggerEvent chan
          . Native.runPostBuild never
          . Native.runPerformEventAndAdjustableM @x respEv
          . Native.runPerformEventH @(SpiderTimeline x)
          $ app AppIn
              { _appIn_behavior = appInB
              , _appIn_event    = appInE
              }
        outHandle <- Native.EffectfulHost
          (Native.subscribeEventE @(SpiderTimeline x) (_appOut_event out))
        actionsHandle <- Native.EffectfulHost
          (Native.subscribeEventE @(SpiderTimeline x) aggActions)
        mpulseB <- liftIO (readIORef pulseHoldTrigRef)
        mpulseE <- liftIO (readIORef pulseEventTrigRef)
        let readPhase = do
              b <- sample (_appOut_behavior out)
              mE <- liftIO (readIORef (SI.spiderEventHandleValue outHandle))
              pure (b, mE)
        let fireBatch ts = fireAndReadE @x ts readPhase actionsHandle respTrigRef
        liftIO $ forM input $ \case
          Nothing -> fireBatch []
          Just (This b') -> case mpulseB of
            Nothing -> error "runAppEffectfulM: tried to fire in-behavior but ref was empty"
            Just p -> fireBatch [p :=> Identity b']
          Just (That e') -> case mpulseE of
            Nothing -> error "runAppEffectfulM: tried to fire in-event but ref was empty"
            Just p -> fireBatch [p :=> Identity e']
          Just (These b' e') -> case (mpulseB, mpulseE) of
            (Just pB, Just pE) -> fireBatch [pB :=> Identity b', pE :=> Identity e']
            _ -> error "runAppEffectfulM: tried to fire in-{behavior,event} but a ref was empty"
      )
      env

runAppEffectfulM'
  :: (forall x. HasSpiderTimeline x
      => Event (SpiderTimeline x) eIn
      -> Eff (Native.HeadlessRowM (SpiderTimeline x))
             (Event (SpiderTimeline x) eOut))
  -> [Maybe eIn]
  -> IO [[Maybe eOut]]
runAppEffectfulM' app input = do
  map (map snd) <$>
    runAppEffectfulM
      (\ain -> AppOut (pure ()) <$> app (_appIn_event ain))
      ()
      (map (fmap That) input)
