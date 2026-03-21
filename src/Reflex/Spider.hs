{-# LANGUAGE CPP #-}
-- |
-- Module:
--   Reflex.Spider
-- Description:
--   The Spider FRP engine — the default (and recommended) implementation of
--   the 'Reflex' typeclass.
-- Copyright:
--   (c) 2015-2020 Ryan Trinkle
-- License:
--   BSD3
-- Maintainer:
--   ryan.trinkle@gmail.com
--
-- Spider is a graph-traversal–based FRP engine. When an external event fires,
-- it propagates through a dependency graph of 'Event's and 'Behavior's,
-- updating only the nodes that are reachable from the trigger. This makes it
-- efficient for large networks with sparse activity.
--
-- = Key types
--
-- [@'Spider'@]           Type alias for @'SpiderTimeline' 'Global'@. This is the
--                        timeline type you use in almost all Reflex programs.
--                        Appears as the @t@ parameter: @Event Spider a@,
--                        @Dynamic Spider a@, etc.
-- [@'SpiderTimeline' x@] A fully-independent FRP timeline. The @x@ phantom type
--                        ensures timelines cannot be mixed. 'Global' is the
--                        default timeline shared by all normal Reflex programs.
-- [@'Global'@]           The singleton timeline type for 'Spider'.
-- [@'SpiderHost'@]       The monad for hosting a Spider network. Provides the
--                        ability to fire external triggers and read event
--                        results.
--
-- = Running a Spider network
--
-- Most applications use a framework like @reflex-dom@ which handles hosting
-- for you. If you need to run a network directly:
--
-- @
-- import Reflex.Host.Headless (runHeadlessApp)
--
-- main :: IO ()
-- main = runHeadlessApp $ do
--   -- Your Reflex network here
--   ePostBuild <- getPostBuild
--   performEvent_ $ liftIO (putStrLn \"Network built!\") \<$ ePostBuild
--   pure never  -- Return an event that signals shutdown
-- @
--
-- For lower-level control, use 'runSpiderHost' directly:
--
-- @
-- import Reflex.Spider
-- import Reflex.Host.Class
--
-- main :: IO ()
-- main = runSpiderHost $ do
--   (eExternal, triggerRef) <- newEventWithTriggerRef
--   -- ... set up your network using eExternal ...
--   -- ... fire events with fireEventRef triggerRef value ...
-- @
--
-- = Multiple timelines
--
-- For advanced use cases (e.g., testing, isolation), you can create
-- independent timelines with 'newSpiderTimeline' or 'withSpiderTimeline'.
-- Events from different timelines cannot interact.
module Reflex.Spider
       ( Spider
       , SpiderTimeline
       , Global
       , SpiderHost
       , runSpiderHost
       , runSpiderHostForTimeline
       , newSpiderTimeline
       , withSpiderTimeline
         -- * Deprecated
       , SpiderEnv
       ) where

import Reflex.Spider.Internal
