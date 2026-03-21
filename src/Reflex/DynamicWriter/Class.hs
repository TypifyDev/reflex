-- |
-- Module:
--   Reflex.DynamicWriter.Class
-- Description:
--   The 'DynamicWriter' typeclass — collect 'Dynamic' outputs from child
--   widgets and combine them monoidally.
-- Copyright:
--   (c) 2015-2020 Ryan Trinkle
-- License:
--   BSD3
-- Maintainer:
--   ryan.trinkle@gmail.com
--
-- 'DynamicWriter' is the FRP analogue of @Writer@ for 'Dynamic' values. Child
-- widgets call 'tellDyn' to contribute 'Dynamic' values, and the parent
-- receives a single 'Dynamic' that is the monoidal combination of all
-- contributions.
--
-- @
-- (result, dAllCss) <- runDynamicWriterT $ do
--   -- Each child contributes CSS classes
--   tellDyn (constDyn \"container\")
--   childWidget  -- may also tellDyn
-- -- dAllCss :: Dynamic t Text (combined via Monoid)
-- @
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.DynamicWriter.Class
  ( MonadDynamicWriter
  , DynamicWriter(..)
  ) where

import Control.Monad.Reader (ReaderT, lift)
import Reflex.Class (Dynamic)

{-# DEPRECATED MonadDynamicWriter "Use 'DynamicWriter' instead" #-}
-- | Type synonym for 'DynamicWriter'
type MonadDynamicWriter = DynamicWriter

-- | 'MonadDynamicWriter' efficiently collects 'Dynamic' values using 'tellDyn'
-- and combines them monoidally to provide a 'Dynamic' result.
class (Monad m, Monoid w) => DynamicWriter t w m | m -> t w where
  tellDyn :: Dynamic t w -> m ()

instance DynamicWriter t w m => DynamicWriter t w (ReaderT r m) where
  tellDyn = lift . tellDyn
