module NMonad.Main (nmonad) where

import Control.Concurrent
import NMonad.Core
import NMonad.DBus

nmonad :: NConfig -> IO ()
nmonad cfg = do
  syncVar <- newEmptyMVar
  listenForNotifications syncVar
  _ <- runN (NEnv syncVar cfg) def mainLoop
  return ()

mainLoop :: N ()
mainLoop = forever $ do
  (dbusNotification, responseVar) <- asks globalMailbox >>= liftIO . takeMVar
  notification <- notificationFromDBus dbusNotification
  addToNotificationsAndReturnId notification >>= liftIO . putMVar responseVar
