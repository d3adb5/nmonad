module NMonad.Main (nmonad) where

import Control.Concurrent
import NMonad.Core
import NMonad.DBus

nmonad :: IO ()
nmonad = do
  syncVar <- newEmptyMVar
  listenForNotifications syncVar
  let conf = NConf 60 "Sleeping for 60 seconds..." syncVar
  _ <- runN conf def mainLoop
  return ()

mainLoop :: N ()
mainLoop = forever $ do
  (dbusNotification, responseVar) <- asks dbusMethodCall >>= liftIO . takeMVar
  notification <- notificationFromDBus dbusNotification
  addToNotificationsAndReturnId notification >>= liftIO . putMVar responseVar
