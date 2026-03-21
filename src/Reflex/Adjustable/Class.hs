{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
-- |
-- Module:
--   Reflex.Adjustable.Class
-- Description:
--   The 'Adjustable' typeclass — monadic actions that can be replaced or
--   incrementally patched over time in response to 'Event's.
-- Copyright:
--   (c) 2015-2020 Ryan Trinkle
-- License:
--   BSD3
-- Maintainer:
--   ryan.trinkle@gmail.com
--
-- 'Adjustable' is the foundation for dynamic widget switching in Reflex. It
-- provides 'runWithReplace', which swaps an entire sub-computation when an
-- 'Event' fires, and 'traverseDMapWithKeyWithAdjust', which incrementally
-- patches a collection of sub-computations.
--
-- Most application code uses higher-level wrappers from "Reflex.Network"
-- ('networkView', 'networkHold') or "Reflex.Collection" ('listWithKey',
-- 'simpleList') rather than calling 'Adjustable' methods directly.
--
-- @
-- -- Replace a widget entirely when a tab changes
-- (initialResult, eNewResult) <- runWithReplace
--   (renderTab HomeTab)          -- initial widget
--   (renderTab \<$\> eTabChange)   -- new widget on each tab change
-- @
module Reflex.Adjustable.Class
  (
  -- * The Adjustable typeclass
    Adjustable(..)
  , sequenceDMapWithAdjust
  , sequenceDMapWithAdjustWithMove
  , mapMapWithAdjustWithMove
  -- * Deprecated aliases
  , MonadAdjust
  ) where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Dependent.Map (DMap)
import Data.GADT.Compare (GCompare(..))
import qualified Data.Dependent.Map as DMap
import Data.Functor.Constant
import Data.Functor.Misc
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map (Map)

import Reflex.Class
import Data.Patch.DMapWithMove

-- | A 'Monad' that supports adjustment over time.  After an action has been
-- run, if the given events fire, it will adjust itself so that its net effect
-- is as though it had originally been run with the new value.  Note that there
-- is some issue here with persistent side-effects: obviously, IO (and some
-- other side-effects) cannot be undone, so it is up to the instance implementer
-- to determine what the best meaning for this class is in such cases.
class (Reflex t, Monad m) => Adjustable t m | m -> t where
  -- | Run the first action, then replace it entirely each time the 'Event'
  -- fires with a new action. Returns the result of the initial action and
  -- an 'Event' of results from each replacement.
  --
  -- @
  -- -- Show a loading spinner, then replace with content when data arrives
  -- (_, eContent) <- runWithReplace loadingSpinner (renderContent \<$\> eDataLoaded)
  -- @
  runWithReplace
    :: m a
    -> Event t (m b)
    -> m (a, Event t b)

  traverseIntMapWithKeyWithAdjust
    :: (IntMap.Key -> v -> m v')
    -> IntMap v
    -> Event t (PatchIntMap v)
    -> m (IntMap v', Event t (PatchIntMap v'))

  traverseDMapWithKeyWithAdjust
    :: GCompare k
    => (forall a. k a -> v a -> m (v' a))
    -> DMap k v
    -> Event t (PatchDMap k v)
    -> m (DMap k v', Event t (PatchDMap k v'))
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust f dm0 dm' = fmap (fmap (fmap fromPatchWithMove)) $
    traverseDMapWithKeyWithAdjustWithMove f dm0 $ fmap toPatchWithMove dm'
   where
    toPatchWithMove (PatchDMap m) = PatchDMapWithMove $ DMap.map toNodeInfoWithMove m
    toNodeInfoWithMove = \case
      ComposeMaybe (Just v) -> NodeInfo (From_Insert v) $ ComposeMaybe Nothing
      ComposeMaybe Nothing -> NodeInfo From_Delete $ ComposeMaybe Nothing
    fromPatchWithMove (PatchDMapWithMove m) = PatchDMap $ DMap.map fromNodeInfoWithMove m
    fromNodeInfoWithMove (NodeInfo from _) = ComposeMaybe $ case from of
      From_Insert v -> Just v
      From_Delete -> Nothing
      From_Move _ -> error "traverseDMapWithKeyWithAdjust: implementation of traverseDMapWithKeyWithAdjustWithMove inserted spurious move"

  traverseDMapWithKeyWithAdjustWithMove
    :: GCompare k
    => (forall a. k a -> v a -> m (v' a))
    -> DMap k v
    -> Event t (PatchDMapWithMove k v)
    -> m (DMap k v', Event t (PatchDMapWithMove k v'))

instance Adjustable t m => Adjustable t (ReaderT r m) where
  runWithReplace a0 a' = do
    r <- ask
    lift $ runWithReplace (runReaderT a0 r) $ fmap (`runReaderT` r) a'
  traverseIntMapWithKeyWithAdjust f dm0 dm' = do
    r <- ask
    lift $ traverseIntMapWithKeyWithAdjust (\k v -> runReaderT (f k v) r) dm0 dm'
  traverseDMapWithKeyWithAdjust f dm0 dm' = do
    r <- ask
    lift $ traverseDMapWithKeyWithAdjust (\k v -> runReaderT (f k v) r) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
    r <- ask
    lift $ traverseDMapWithKeyWithAdjustWithMove (\k v -> runReaderT (f k v) r) dm0 dm'

-- | Traverse a 'DMap' of 'Adjustable' actions, running each of them. The provided 'Event' of patches
-- to the 'DMap' can add, remove, or update values.
sequenceDMapWithAdjust
  :: (GCompare k, Adjustable t m)
  => DMap k m
  -> Event t (PatchDMap k m)
  -> m (DMap k Identity, Event t (PatchDMap k Identity))
sequenceDMapWithAdjust = traverseDMapWithKeyWithAdjust $ \_ -> fmap Identity

-- | Traverses a 'DMap' of 'Adjustable' actions, running each of them. The provided 'Event' of patches
-- to the 'DMap' can add, remove, update, move, or swap values.
sequenceDMapWithAdjustWithMove
  :: (GCompare k, Adjustable t m)
  => DMap k m
  -> Event t (PatchDMapWithMove k m)
  -> m (DMap k Identity, Event t (PatchDMapWithMove k Identity))
sequenceDMapWithAdjustWithMove = traverseDMapWithKeyWithAdjustWithMove $ \_ -> fmap Identity

-- | Traverses a 'Map', running the provided 'Adjustable' action. The provided 'Event' of patches to the 'Map'
-- can add, remove, update, move, or swap values.
mapMapWithAdjustWithMove
  :: forall t m k v v'. (Adjustable t m, Ord k)
  => (k -> v -> m v')
  -> Map k v
  -> Event t (PatchMapWithMove k v)
  -> m (Map k v', Event t (PatchMapWithMove k v'))
mapMapWithAdjustWithMove f m0 m' = do
  (out0 :: DMap (Const2 k v) (Constant v'), out') <- traverseDMapWithKeyWithAdjustWithMove (\(Const2 k) (Identity v) -> Constant <$> f k v) (mapToDMap m0) (const2PatchDMapWithMoveWith Identity <$> m')
  return (dmapToMapWith (\(Constant v') -> v') out0, patchDMapWithMoveToPatchMapWithMoveWith (\(Constant v') -> v') <$> out')

--------------------------------------------------------------------------------
-- Deprecated functions
--------------------------------------------------------------------------------

{-# DEPRECATED MonadAdjust "Use Adjustable instead" #-}
-- | Synonym for 'Adjustable'
type MonadAdjust = Adjustable
