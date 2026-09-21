------------------------------------------------------------------------------------------------------------------------
-- |
-- Module      : NMonad.Main
-- Description : NMonad's main loop and entrypoint
-- Copyright   : (c) d3adb5 2023
-- License     : BSD3
-- Maintainer  : d3adb5 <me@d3adb5.net>
-- Stability   : experimental
--
-- This module contains the main entrypoint and main loop for NMonad.
------------------------------------------------------------------------------------------------------------------------

module NMonad.Main (nmonad) where

import Control.Concurrent
import Control.Monad ((>=>))
import Control.Monad.Extra (whenJustM, forever)
import NMonad.Core
import NMonad.DBus
import NMonad.Operations

-- | Start nmonad with a given configuration as an `NConfig`.
nmonad :: NConfig -> IO ()
nmonad cfg = do
  syncVar <- newEmptyMVar
  listenForNotifications (cfg ^. dbusRequestNameFlags) syncVar
  _ <- runN (NEnv syncVar cfg) def mainLoop
  return ()

-- | Process notifications as they arrive in envelopes through the global mailbox variable. After processing and
-- indexing it, place it in the response variable indicated by the envelope.
mainLoop :: N ()
mainLoop = forever $ do
  (dbusNotification, responseVar) <- asks (view globalMailbox) >>= liftIO . takeMVar
  whenJustM (processNotification dbusNotification) $ indexNotification >=> liftIO . putMVar responseVar
