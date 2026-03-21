{-# LANGUAGE CPP #-}
-- |
-- Module:
--   Reflex
-- Description:
--   The main entry point for Reflex — a higher-order Functional Reactive
--   Programming (FRP) library for Haskell.
-- Copyright:
--   (c) 2015-2020 Ryan Trinkle
-- License:
--   BSD3
-- Maintainer:
--   ryan.trinkle@gmail.com
--
-- This module re-exports everything you need to build Reflex applications.
-- If you are just getting started with Reflex, import this module.
--
-- = Core FRP Primitives (from "Reflex.Class")
--
-- The three fundamental types are:
--
-- * 'Event' @t a@ — a stream of discrete occurrences carrying values of type @a@
-- * 'Behavior' @t a@ — a time-varying value that can be sampled at any time
-- * 'Dynamic' @t a@ — combines an 'Event' and a 'Behavior'; you get change
--   notifications /and/ can sample the current value
--
-- The @t@ parameter is the /timeline/ — it selects which FRP engine to use.
-- In practice you almost always use 'Reflex.Spider.Spider' via 'Reflex.Spider.SpiderHost'.
--
-- = Building Stateful Logic
--
-- * 'holdDyn' — create a 'Dynamic' from an initial value and an update 'Event'
-- * 'foldDyn' — accumulate state over time (like a pure fold over events)
-- * 'hold' — create a 'Behavior' from an initial value and an update 'Event'
-- * 'accumDyn', 'mapAccumDyn' — more advanced state accumulators
--
-- = Combining and Transforming Signals
--
-- * 'leftmost' — combine multiple 'Event's; leftmost wins on simultaneous fire
-- * 'merge', 'mergeMap' — merge a collection of 'Event's into one
-- * 'fan' / 'select' — efficiently split one 'Event' into many by key
-- * 'tag', 'attach', 'attachWith' — sample a 'Behavior' when an 'Event' fires
-- * 'gate' — block an 'Event' based on a 'Bool' 'Behavior'
-- * 'switch' — flatten a @'Behavior' ('Event' a)@ into an @'Event' a@
-- * 'switchDyn', 'switchHold' — flatten dynamic event sources
--
-- = Performing Side Effects
--
-- * 'Reflex.PerformEvent.Class.performEvent' — run IO actions when an 'Event' fires
-- * 'Reflex.PostBuild.Class.getPostBuild' — get an 'Event' that fires once after setup
-- * 'Reflex.TriggerEvent.Class.newTriggerEvent' — create events triggered from IO
--
-- = Dynamic Network Switching (from "Reflex.Adjustable.Class")
--
-- * 'Reflex.Adjustable.Class.runWithReplace' — swap an entire sub-network when an 'Event' fires
-- * 'Reflex.Adjustable.Class.traverseDMapWithKeyWithAdjust' — incrementally adjust a collection
-- * 'Reflex.Network.networkView' — rebuild a widget whenever a 'Dynamic' changes
-- * 'Reflex.Network.networkHold' — like 'networkView' but returns a 'Dynamic' result
--
-- = Writer Monads (for output aggregation)
--
-- * "Reflex.EventWriter" — collect 'Event' outputs from subcomputations
-- * "Reflex.DynamicWriter" — collect 'Dynamic' outputs from subcomputations
-- * "Reflex.BehaviorWriter" — collect 'Behavior' outputs from subcomputations
--
-- = Time Utilities (from "Reflex.Time")
--
-- * 'Reflex.Time.delay' — delay an 'Event' by a given duration
-- * 'Reflex.Time.debounce' — debounce rapid 'Event' firings
-- * 'Reflex.Time.throttle' — throttle 'Event' firings to a maximum rate
-- * 'Reflex.Time.tickLossy' — create a recurring timer 'Event'
--
-- = Module Map
--
-- [@"Reflex.Class"@] Core FRP primitives: 'Event', 'Behavior', 'Dynamic', and all combinators
-- [@"Reflex.Dynamic"@] Additional 'Dynamic' utilities: 'foldDyn', 'switchDyn', 'Demux', etc.
-- [@"Reflex.Dynamic.TH"@] Template Haskell for creating 'Dynamic' expressions (requires @-fuse-template-haskell@)
-- [@"Reflex.Spider"@] The default FRP engine implementation (graph-traversal based)
-- [@"Reflex.Adjustable.Class"@] The 'Adjustable' typeclass for dynamic sub-network switching
-- [@"Reflex.Network"@] High-level network switching: 'networkView', 'networkHold'
-- [@"Reflex.PerformEvent.Class"@] The 'PerformEvent' typeclass for triggering side effects
-- [@"Reflex.PostBuild.Class"@] The 'PostBuild' typeclass for one-time initialization events
-- [@"Reflex.TriggerEvent.Class"@] The 'TriggerEvent' typeclass for IO-driven events
-- [@"Reflex.Time"@] Time-based combinators: delay, debounce, throttle, tick
-- [@"Reflex.Collection"@] Utilities for working with dynamic collections
-- [@"Reflex.Workflow"@] State machine–style widget workflows
-- [@"Reflex.Host.Class"@] Low-level hosting API (for framework implementors)
-- [@"Reflex.Host.Headless"@] Run a Reflex network without a GUI (for testing / servers)
module Reflex
  ( module X
  ) where

import Reflex.Class as X
import Reflex.Adjustable.Class as X
import Reflex.BehaviorWriter.Base as X
import Reflex.BehaviorWriter.Class as X
import Reflex.Collection as X
import Reflex.Dynamic as X
import Reflex.EventWriter.Base as X
import Reflex.EventWriter.Class as X
#ifdef USE_TEMPLATE_HASKELL
import Reflex.Dynamic.TH as X
#endif
import Reflex.Dynamic.Uniq as X
import Reflex.DynamicWriter.Base as X
import Reflex.DynamicWriter.Class as X
import Reflex.NotReady.Class as X
import Reflex.PerformEvent.Base as X
import Reflex.PerformEvent.Class as X
import Reflex.PostBuild.Base as X
import Reflex.PostBuild.Class as X
import Reflex.Profiled as X
import Reflex.Query.Base as X
import Reflex.Query.Class as X
import Reflex.Requester.Base as X
import Reflex.Requester.Class as X
import Reflex.Spider as X
import Reflex.Time as X
import Reflex.TriggerEvent.Base as X
import Reflex.TriggerEvent.Class as X
