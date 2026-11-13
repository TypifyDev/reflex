{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (unless, replicateM_)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, wait)
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity (Identity (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.GADT.Compare (GEq (..))
import qualified Data.IntMap.Strict as IntMap
import Data.Type.Equality ((:~:) (Refl))
import Effectful (Eff)
import Reflex (Event, SpiderTimeline, constDyn, current, holdDyn, leftmost, runWithReplace, sample, tag, updated)
import qualified Reflex.Host.HeadlessEffectful as Native
import Reflex.Requester.Base
import Reflex.Spider.Internal (Global, HasSpiderTimeline, RootTrigger, SpiderHostFrame)
import System.Mem (performGC)
import Test.Run (AppIn (..), AppOut (..), runAppEffectfulM', runAppEffectfulMWith)
import Data.These (These (That))
import Data.Patch.IntMap (PatchIntMap (..))
import Reflex.Adjustable.Class (traverseIntMapWithKeyWithAdjust)
import System.Timeout (timeout)

-- Each inner Requester scope starts at this scalar tag; outer bundle keys
-- distinguish the same tag in different elements and replacement cohorts.
scalarTag :: MyTag (Single Int)
scalarTag = MyTag (-4)

request :: Applicative f => Int -> RequesterData f
request value = singletonRequesterData (RequesterDataKey_Single scalarTag) (pure value)

bundle :: Applicative f => Int -> Entry f Multi3
bundle value = Entry $ IntMap.fromList
  [ (10, IntMap.fromList [(0, request value), (1, request (value + 1))])
  , (20, IntMap.singleton 0 $ request (value + 2))
  ]

decodeResponse :: DSum RequesterDataKey Identity -> Either String Int
decodeResponse (RequesterDataKey_Single responseTag :=> Identity value) =
  case geq responseTag scalarTag of
    Just Refl -> Right value
    Nothing -> Left "response changed the inner request tag"
decodeResponse _ = Left "response gained an unexpected nested request scope"

decodeBundle
  :: Entry Identity Multi3
  -> Either String (IntMap.IntMap (IntMap.IntMap [Int]))
decodeBundle = traverse (traverse (traverse decodeResponse . requesterDataToList)) . unEntry

bundledRoundTrip
  :: forall x. HasSpiderTimeline x
  => Event (SpiderTimeline x) Int
  -> Eff (Native.HeadlessRowM (SpiderTimeline x))
       (Event (SpiderTimeline x) (Either String (IntMap.IntMap (IntMap.IntMap [Int]))))
bundledRoundTrip pulse = fmap decodeBundle <$>
  Native.requestingBundled3E @(SpiderTimeline x) @(SpiderHostFrame x) @Identity
    (bundle <$> pulse)

-- Sampling an independent behavior must not subscribe to queued holds
-- while their recursive inputs are still being constructed.
recursiveSampling
  :: forall x. HasSpiderTimeline x
  => Event (SpiderTimeline x) Int
  -> Eff (Native.HeadlessRowM (SpiderTimeline x)) (Event (SpiderTimeline x) Int)
recursiveSampling pulse = mdo
  output <- holdDyn 0 $ leftmost [tag (current later) pulse, updated later]
  initial <- sample $ current $ constDyn @(SpiderTimeline x) 7
  later <- holdDyn initial pulse
  pure $ updated output

checkRecursiveSampling :: IO ()
checkRecursiveSampling = do
  actual <- timeout 5000000 $ runAppEffectfulM' recursiveSampling [Just 100, Just 200]
  unless (actual == Just [[Just 7], [Just 100]]) $ ioError $ userError $
    "sampling forced recursive event inputs or changed frame semantics: " <> show actual

checkBundles :: IO ()
checkBundles = do
  actual <- runAppEffectfulM' bundledRoundTrip [Just 100, Just 200]
  let first = IntMap.fromList [(10, IntMap.fromList [(0, [100]), (1, [101])]), (20, IntMap.singleton 0 [102])]
      second = IntMap.fromList [(10, IntMap.fromList [(0, [200]), (1, [201])]), (20, IntMap.singleton 0 [202])]
      expected = [[Nothing, Just (Right first)], [Nothing, Just (Right second)]]
  unless (actual == expected) $ ioError $ userError $
    "bundled responses changed keys, cohorts, ordering or values: " <> show actual

replacement
  :: HasSpiderTimeline x
  => AppIn (SpiderTimeline x) () Int
  -> Eff (Native.HeadlessRowM (SpiderTimeline x)) (AppOut (SpiderTimeline x) () Int)
replacement input = do
  (initial, changes) <- runWithReplace (pure 0) (pure <$> _appIn_event input)
  value <- holdDyn initial changes
  pure $ AppOut (pure ()) $ updated value

checkReplacement :: (forall a. IO a -> IO a) -> IO ()
checkReplacement dispatch = do
  actual <- runAppEffectfulMWith dispatch
    replacement () [Just (That 10), Just (That 20)]
  unless (actual == [[((), Nothing), ((), Just 10)], [((), Nothing), ((), Just 20)]]) $
    ioError $ userError $ "replacement failed on the browser event thread: " <> show actual

adjustIntMap
  :: HasSpiderTimeline x
  => AppIn (SpiderTimeline x) () (PatchIntMap Int)
  -> Eff (Native.HeadlessRowM (SpiderTimeline x))
       (AppOut (SpiderTimeline x) (IntMap.IntMap Int) (PatchIntMap Int))
adjustIntMap input = do
  (initial, changes) <- traverseIntMapWithKeyWithAdjust
    (\key value -> pure (key + value)) (IntMap.singleton 1 5) (_appIn_event input)
  pure $ AppOut (pure initial) changes

checkIntMap :: (forall a. IO a -> IO a) -> IO ()
checkIntMap dispatch = do
  let patch = PatchIntMap $ IntMap.fromList [(1, Nothing), (2, Just 10)]
      expected = PatchIntMap $ IntMap.fromList [(1, Nothing), (2, Just 12)]
  actual <- runAppEffectfulMWith dispatch adjustIntMap () [Just (That patch)]
  let initial = IntMap.singleton 1 6
  unless (actual == [[(initial, Nothing), (initial, Just expected)]]) $
    ioError $ userError $ "IntMap replacement changed keys or values: " <> show actual

-- Only the reader closure retains the handle, as in the native drain loop.
newRetainedEventReader
  :: IO (IORef (Maybe (RootTrigger Global Int)), IO (Maybe Int))
newRetainedEventReader = Native.runEffectfulHost $ Native.EffectfulHost $ do
  triggerRef <- liftIO $ newIORef Nothing
  event <- Native.newEventWithTriggerE @(SpiderTimeline Global) $ \trigger -> do
    writeIORef triggerRef $ Just trigger
    pure $ writeIORef triggerRef Nothing
  handle <- Native.subscribeEventE event
  pure (triggerRef, Native.readSpiderEventHandle handle)

checkEventHandleLifetime :: IO ()
checkEventHandleLifetime = do
  (triggerRef, readCurrent) <- newRetainedEventReader
  initial <- readCurrent
  unless (initial == Nothing) $ fail "event fired before its first trigger"
  replicateM_ 3 $ performGC >> threadDelay 10000
  trigger <- readIORef triggerRef >>= maybe (fail "GC disconnected the live event subscription") pure
  actual <- Native.runE [trigger :=> Identity 42] $ liftIO readCurrent
  unless (actual == Just 42) $ fail "event delivery stopped after GC"

main :: IO ()
main = do
  checkEventHandleLifetime
  checkBundles
  checkRecursiveSampling
  checkReplacement id
  checkReplacement (\action -> withAsync action wait)
  checkIntMap id
  checkIntMap (\action -> withAsync action wait)
