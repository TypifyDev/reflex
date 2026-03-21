-- |
-- Module:
--   Reflex.EventWriter.Class
-- Description:
--   The 'EventWriter' typeclass — collect 'Event' outputs from child widgets
--   and combine them monoidally.
-- Copyright:
--   (c) 2015-2020 Ryan Trinkle
-- License:
--   BSD3
-- Maintainer:
--   ryan.trinkle@gmail.com
--
-- 'EventWriter' is the FRP analogue of @Writer@ for events. Child widgets call
-- 'tellEvent' to contribute 'Event's, and the parent receives a single
-- combined 'Event' (via 'Semigroup') containing all children's contributions.
--
-- @
-- (result, eAllClicks) <- runEventWriterT $ do
--   -- Each button contributes its label to the collected event
--   tellEvent (\"Save\" \<$ eSaveClick)
--   tellEvent (\"Cancel\" \<$ eCancelClick)
-- -- eAllClicks :: Event t Text (combined via Semigroup)
-- @
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.EventWriter.Class
  ( EventWriter (..)
  ) where

import Control.Monad.Reader (ReaderT, lift)
import Data.Semigroup (Semigroup)

import Reflex.Class (Event)


-- | 'EventWriter' efficiently collects 'Event' values using 'tellEvent'
-- and combines them via 'Semigroup' to provide an 'Event' result.
class (Monad m, Semigroup w) => EventWriter t w m | m -> t w where
  tellEvent :: Event t w -> m ()


instance EventWriter t w m => EventWriter t w (ReaderT r m) where
  tellEvent = lift . tellEvent
