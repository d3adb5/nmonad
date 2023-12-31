------------------------------------------------------------------------------------------------------------------------
-- |
-- Module      : NMonad.DBus
-- Description : DBus related operations for NMonad
-- Copyright   : (c) d3adb5 2023
-- License     : BSD3
-- Maintainer  : d3adb5 <me@d3adb5.net>
-- Stability   : experimental
--
-- Connecting, opening a session, issuing requests and receiving replies from DBus is all handled by this module.
-- For the most part, everything is done within 'IO', in a parallel thread that synchronizes with the main loop through
-- an 'MVar'.
------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module NMonad.DBus
  ( listenForNotifications
  ) where

import Control.Concurrent
import Data.Int (Int32)
import Data.Map (Map)
import Data.Text (Text, pack)
import Data.Word (Word32)
import DBus.Client
import DBus (Variant)

import Data.Version (showVersion)
import Paths_nmonad (version)

import NMonad.Core

-- | Helper function to create an AutoMethod from a function that accepts a 'DBusNotification'.
withDBusNotification
  :: (DBusNotification -> a) -- ^ Function that accepts a DBusNotification.
  -> Text                    -- ^ Name of the application sending the notification.
  -> Word32                  -- ^ ID of the notification to be replaced, if any.
  -> Text                    -- ^ Icon to be displayed in the notification.
  -> Text                    -- ^ Summary of the notification.
  -> Text                    -- ^ Body of the notification.
  -> [Text]                  -- ^ Actions to be displayed in the notification.
  -> Map Text Variant        -- ^ Hints for the notification.
  -> Int32                   -- ^ Timeout for the notification.
  -> a
withDBusNotification = (........ DBusNotification)
  where (........) = (.) . (.) . (.) . (.) . (.) . (.) . (.) . (.)

-- | Receives a notification from DBus and places it in the global MVar.
receiveNotification
  :: MVar (DBusNotification, MVar Word32) -- ^ MVar to place notification data and new MVar.
  -> DBusNotification                     -- ^ Raw notification data received from DBus.
  -> IO Word32                            -- ^ Yields the ID of the sent notification.
receiveNotification syncVar dn = do
  notificationId <- newEmptyMVar
  putMVar syncVar (dn, notificationId)
  takeMVar notificationId

-- | Server information exposed to DBus as GetServerInformation().
serverInformation :: IO (Text, Text, Text, Text)
serverInformation = return ("NMonad", "NMonad", pack $ showVersion version, "1.2")

-- | Capability information exposed to DBus as GetCapabilities().
capabilities :: IO [Text]
capabilities = return ["body", "body-markup", "icon-static"]

-- | Connect to DBus, request the org.freedesktop.Notifications name and export the appropriate interface at
-- /org/freedesktop/Notifications.
--
-- Requires an 'MVar' to for synchronization with the main thread. The 'MVar' is used to pass the notification data
-- received from DBus to the main thread, as well as a reference to a new 'MVar' that will be populated with the
-- 'Word32' representing the ID of the sent notification. Refer to the Desktop Notifications Specification for more
-- information.
--
-- This action starts a new thread due to calling 'connectSession', and so it doesn't block the thread that
-- invoked it.
--
listenForNotifications :: MVar (DBusNotification, MVar Word32) -> IO ()
listenForNotifications syncVar = do
  putStrLn "Connecting to DBus to create client..."
  client <- connectSession

  putStrLn "Requesting the org.freedesktop.Notifications name..."
  reply <- requestName client "org.freedesktop.Notifications" [nameAllowReplacement, nameReplaceExisting]
  putStrLn $ "Reply: " ++ show reply
  when (reply /= NamePrimaryOwner) $
    fail ("Failed to become primary owner of org.freedesktop.Notifications: " ++ show reply)

  export client "/org/freedesktop/Notifications" defaultInterface
    { interfaceName = "org.freedesktop.Notifications"
    , interfaceMethods =
      [ autoMethod "GetServerInformation" serverInformation
      , autoMethod "GetCapabilities" capabilities
      , autoMethod "Notify" (withDBusNotification $ receiveNotification syncVar)
      ]
    }
