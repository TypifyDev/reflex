{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
-- |
-- Module:
--   Reflex.Network
-- Description:
--   High-level combinators for dynamically switching entire sub-networks
--   based on 'Dynamic' or 'Event' changes.
-- Copyright:
--   (c) 2015-2020 Ryan Trinkle
-- License:
--   BSD3
-- Maintainer:
--   ryan.trinkle@gmail.com
--
-- These are the most commonly used functions for rebuilding parts of a Reflex
-- UI when some 'Dynamic' value changes.
--
-- @
-- -- Rebuild the page body whenever the current route changes
-- ePageResult <- networkView $ renderPage \<$\> dCurrentRoute
--
-- -- Or use networkHold to get a Dynamic result
-- dPageResult <- networkHold (renderPage Home) (renderPage \<$\> updated dRoute)
-- @
module Reflex.Network
  ( networkView
  , networkHold
  , untilReady
  ) where

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.NotReady.Class
import Reflex.PostBuild.Class

-- | A 'Dynamic' "network": Takes a 'Dynamic' of network-creating actions and
-- replaces the network whenever the 'Dynamic' updates. The returned Event of
-- network results fires at post-build time and when the 'Dynamic' updates.
--
-- Note: Often, the type @a@ is an 'Event', in which case the return value is
-- an Event-of-Events that would typically be flattened via 'switchHold'.
--
-- @
-- -- Rebuild the entire form when the form type changes
-- eFormResult <- networkView $ buildForm \<$\> dFormType
-- eSubmit <- switchHold never eFormResult
-- @
networkView :: (NotReady t m, Adjustable t m, PostBuild t m) => Dynamic t (m a) -> m (Event t a)
networkView child = do
  postBuild <- getPostBuild
  let newChild = leftmost [updated child, tagCheap (current child) postBuild]
  snd <$> runWithReplace notReady newChild

-- | Given an initial "network" and an 'Event' of network-creating actions,
-- create a network that is recreated whenever the Event fires.
-- The returned Dynamic of network results occurs when the Event does.
--
-- @
-- -- Start with a welcome page, switch to content when user logs in
-- dContent <- networkHold welcomePage (dashboardPage \<$\> eLoggedIn)
-- @
networkHold :: (Adjustable t m, MonadHold t m) => m a -> Event t (m a) -> m (Dynamic t a)
networkHold child0 newChild = do
  (result0, newResult) <- runWithReplace child0 newChild
  holdDyn result0 newResult

-- | Render a placeholder network to be shown while another network is not yet
-- done building
untilReady :: (Adjustable t m, PostBuild t m) => m a -> m b -> m (a, Event t b)
untilReady a b = do
  postBuild <- getPostBuild
  runWithReplace a $ b <$ postBuild
