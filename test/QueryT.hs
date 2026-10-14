{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Lens
import Control.Monad.Fix
import Data.Align
import qualified Data.AppendMap as AMap
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import Data.Semigroup
import Data.Semigroup.Commutative
import Data.These

#if defined(MIN_VERSION_these_lens) || (MIN_VERSION_these(0,8,0) && !MIN_VERSION_these(0,9,0))
import Data.These.Lens
#endif

import Control.Exception (SomeException, try)
import Reflex
import Data.Patch.MapWithMove
import Test.Run

newtype MyQuery = MyQuery SelectedCount
  deriving (Show, Read, Eq, Ord, Monoid, Semigroup, Commutative, Group)

instance Query MyQuery where
  type QueryResult MyQuery = ()
  crop _ _ = ()

instance (Ord k, Query a, Eq (QueryResult a), Align (MonoidalMap k)) => Query (Selector k a) where
  type QueryResult (Selector k a) = Selector k (QueryResult a)
  crop q r = undefined

newtype Selector k a = Selector { unSelector :: MonoidalMap k a }
  deriving (Show, Read, Eq, Ord, Functor)

#if !(MIN_VERSION_monoidal_containers(0,4,1))
deriving instance Ord k => Align (MonoidalMap k)
#endif

instance (Ord k, Eq a, Monoid a, Align (MonoidalMap k)) => Semigroup (Selector k a) where
  (Selector a) <> (Selector b) = Selector $ fmapMaybe id $ f a b
    where
      f = alignWith $ \case
        This x -> Just x
        That y -> Just y
        These x y ->
          let z = x `mappend` y
          in if z == mempty then Nothing else Just z

instance (Ord k, Eq a, Monoid a, Align (MonoidalMap k)) => Monoid (Selector k a) where
  mempty = Selector AMap.empty
  mappend = (<>)

instance (Eq a, Ord k, Group a, Align (MonoidalMap k)) => Group (Selector k a) where
  negateG = fmap negateG

instance (Eq a, Ord k, Group a, Align (MonoidalMap k)) => Commutative (Selector k a)

runBothNoAssert :: Show a => String -> IO a -> IO a -> IO a
runBothNoAssert label upstream native = do
  putStrLn ("--- " <> label <> " ---")
  osU <- upstream
  putStrLn ("  upstream: " <> show osU)
  eOsN <- try native :: IO (Either SomeException _)
  case eOsN of
    Left e  -> putStrLn ("  native:   <exception> " <> show e)
    Right o -> putStrLn ("  native:   " <> show o)
  pure osU

main :: IO ()
main = do
  os1 <- runBothNoAssert "testRunWithReplace"
    (fmap (map fst . concat) $
      runApp (testQueryT testRunWithReplace) () $
        map (Just . That) [That (), This (), That ()])
    (fmap (map fst . concat) $
      runAppEffectful (testQueryT testRunWithReplace) () $
        map (Just . That) [That (), This (), That ()])
  os2 <- runBothNoAssert "testSequenceDMapWithAdjust"
    (fmap (map fst . concat) $
      runApp (testQueryT testSequenceDMapWithAdjust) () $
        map (Just . That) [That (), This (), That ()])
    (fmap (map fst . concat) $
      runAppEffectful (testQueryT testSequenceDMapWithAdjust) () $
        map (Just . That) [That (), This (), That ()])
  os3 <- runBothNoAssert "testSequenceDMapWithAdjustWithMove"
    (fmap (map fst . concat) $
      runApp (testQueryT testSequenceDMapWithAdjustWithMove) () $
        map (Just . That) [That (), This (), That ()])
    (fmap (map fst . concat) $
      runAppEffectful (testQueryT testSequenceDMapWithAdjustWithMove) () $
        map (Just . That) [That (), This (), That ()])
  -- Upstream assertions preserved.
  let ![0, 1, 1, 0] = (os1 :: [Int])
  let ![0, 1, 1, 0] = os2
  let ![0, 1, 1, 0] = os3
  putStrLn "All QueryT upstream assertions passed; native runs exercised."
  return ()

testQueryT :: (Reflex t, MonadFix m)
           => (Event t () -> Event t () -> QueryT t (Selector Int MyQuery) m ())
           -> AppIn t () (These () ())
           -> m (AppOut t Int Int)
testQueryT w (AppIn _ pulse) = do
  let replace = fmapMaybe (^? here) pulse
      increment = fmapMaybe (^? there) pulse
  (_, q) <- runQueryT (w replace increment) $ pure mempty
  let qDyn = head . AMap.keys . unSelector <$> incrementalToDynamic q
  return $ AppOut
    { _appOut_behavior = current qDyn
    , _appOut_event = updated qDyn
    }

testRunWithReplace :: ( Reflex t
                      , Adjustable t m
                      , MonadHold t m
                      , MonadFix m
                      , MonadQuery t (Selector Int MyQuery) m)
                   => Event t ()
                   -> Event t ()
                   -> m ()
testRunWithReplace replace increment = do
  let w = do
        n <- count increment
        queryDyn $ zipDynWith (\x y -> Selector (AMap.singleton (x :: Int) y)) n $ pure $ MyQuery $ SelectedCount 1
  _ <- runWithReplace w $ w <$ replace
  return ()

testSequenceDMapWithAdjust :: ( Reflex t
                              , Adjustable t m
                              , MonadHold t m
                              , MonadFix m
                              , MonadQuery t (Selector Int MyQuery) m)
                           => Event t ()
                           -> Event t ()
                           -> m ()
testSequenceDMapWithAdjust replace increment = do
  _ <- listHoldWithKey (Map.singleton () ()) (Map.singleton () (Just ()) <$ replace) $ \_ _ -> do
    n <- count increment
    queryDyn $ zipDynWith (\x y -> Selector (AMap.singleton (x :: Int) y)) n $ pure $ MyQuery $ SelectedCount 1
  return ()

testSequenceDMapWithAdjustWithMove :: ( Reflex t
                                      , Adjustable t m
                                      , MonadHold t m
                                      , MonadFix m
                                      , MonadQuery t (Selector Int MyQuery) m)
                                   => Event t ()
                                   -> Event t ()
                                   -> m ()
testSequenceDMapWithAdjustWithMove replace increment = do
  _ <- listHoldWithKeyWithMove (Map.singleton () ()) (Map.singleton () (Just ()) <$ replace) $ \_ _ -> do
    n <- count increment
    queryDyn $ zipDynWith (\x y -> Selector (AMap.singleton (x :: Int) y)) n $ pure $ MyQuery $ SelectedCount 1
  return ()

-- scam it out to test traverseDMapWithAdjustWithMove
listHoldWithKeyWithMove :: forall t m k v a. (Ord k, MonadHold t m, Adjustable t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> m a) -> m (Dynamic t (Map k a))
listHoldWithKeyWithMove m0 m' f = do
  (n0, n') <- mapMapWithAdjustWithMove f m0 $ ffor m' $ PatchMapWithMove . Map.map (\v -> NodeInfo (maybe From_Delete From_Insert v) Nothing)
  incrementalToDynamic <$> holdIncremental n0 n'
-- -}
