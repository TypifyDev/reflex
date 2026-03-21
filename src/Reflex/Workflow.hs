{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module:
--   Reflex.Workflow
-- Description:
--   State-machine–style widget sequencing. Describes a series of widgets that
--   can send data to, invoke, and replace one another.
-- Copyright:
--   (c) 2015-2020 Ryan Trinkle
-- License:
--   BSD3
-- Maintainer:
--   ryan.trinkle@gmail.com
--
-- A 'Workflow' is a widget that produces both a result value and an 'Event'
-- carrying the /next/ workflow step. When that event fires, the current widget
-- is torn down and replaced by the new one.
--
-- This is useful for multi-step forms, wizards, login flows, or any UI where
-- exactly one "page" is active at a time.
--
-- @
-- loginWorkflow :: (DomBuilder t m, ...) => Workflow t m ()
-- loginWorkflow = Workflow $ do
--   eSubmit <- loginForm
--   pure ((), verifyWorkflow \<$ eSubmit)
--
-- verifyWorkflow :: (DomBuilder t m, ...) => Workflow t m ()
-- verifyWorkflow = Workflow $ do
--   eVerified <- verificationCodeForm
--   pure ((), dashboardWorkflow \<$ eVerified)
--
-- dashboardWorkflow :: (DomBuilder t m, ...) => Workflow t m ()
-- dashboardWorkflow = Workflow $ do
--   eLogout <- dashboard
--   pure ((), loginWorkflow \<$ eLogout)
--
-- -- Run the workflow
-- main = mainWidget $ void $ workflow loginWorkflow
-- @
module Reflex.Workflow (
    Workflow (..)
  , workflow
  , workflowView
  , mapWorkflow
  , mapWorkflowCheap
  ) where

import Control.Arrow ((***))
import Control.Monad.Fix (MonadFix)

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.Network
import Reflex.NotReady.Class
import Reflex.PostBuild.Class

-- | A widget in a workflow
--
-- When the 'Event' returned by a 'Workflow' fires, the current 'Workflow' is replaced by the one inside the firing 'Event'. A series of 'Workflow's must share the same return type.
newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) }

-- | Runs a 'Workflow' and returns the 'Dynamic' result of the 'Workflow' (i.e., a 'Dynamic' of the value produced by the current 'Workflow' node, and whose update 'Event' fires whenever one 'Workflow' is replaced by another).
workflow :: forall t m a. (Reflex t, Adjustable t m, MonadFix m, MonadHold t m) => Workflow t m a -> m (Dynamic t a)
workflow w0 = do
  rec eResult <- networkHold (unWorkflow w0) $ fmap unWorkflow $ switch $ snd <$> current eResult
  return $ fmap fst eResult

-- | Similar to 'workflow', but outputs an 'Event' that fires at post-build time and whenever the current 'Workflow' is replaced by the next 'Workflow'.
workflowView :: forall t m a. (Reflex t, NotReady t m, Adjustable t m, MonadFix m, MonadHold t m, PostBuild t m) => Workflow t m a -> m (Event t a)
workflowView w0 = do
  rec eResult <- networkView . fmap unWorkflow =<< holdDyn w0 eReplace
      eReplace <- fmap switch $ hold never $ fmap snd eResult
  return $ fmap fst eResult

-- | Map a function over a 'Workflow', possibly changing the return type.
mapWorkflow :: (Reflex t, Functor m) => (a -> b) -> Workflow t m a -> Workflow t m b
mapWorkflow f (Workflow x) = Workflow (fmap (f *** fmap (mapWorkflow f)) x)

-- | Map a "cheap" function over a 'Workflow'. Refer to the documentation for 'pushCheap' for more information and performance considerations.
mapWorkflowCheap :: (Reflex t, Functor m) => (a -> b) -> Workflow t m a -> Workflow t m b
mapWorkflowCheap f (Workflow x) = Workflow (fmap (f *** fmapCheap (mapWorkflowCheap f)) x)
