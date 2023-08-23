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
