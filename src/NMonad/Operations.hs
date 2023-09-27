------------------------------------------------------------------------------------------------------------------------
-- |
-- Module      :  NMonad.Operations
-- Description :  NMonad operations on internal state.
-- Copyright   : (c) d3adb5 2023
-- License     : BSD3
-- Maintainer  : d3adb5 <me@d3adb5.net>
-- Stability   : experimental
--
-- This module provides standard operations within the 'N' monad that have an effect on internal state.
------------------------------------------------------------------------------------------------------------------------

module NMonad.Operations
  ( indexNotification
  , removeNotification
  , replaceNotification
  , updateNotification
  , generateNotificationId
  ) where

import Prelude hiding (lookup)

import Control.Lens (over, view)
import Control.Monad.State (modify, gets)

import Data.Map (insert, lookup, adjust, delete)
import Data.Word (Word32)

import NMonad.Core (N(..), Notification)
import NMonad.Lenses

-- | Adds a given 'Notification' to internal state. Its ID is returned for convenience.
indexNotification :: Notification -> N Word32
indexNotification n = do
  modify . over notifications $ insert (view identifier n) n
  modify $ over notificationCount (+ 1)
  return (view identifier n)

-- | Generates a new notification ID given knowledge of internal state. If unused, the ID may be reissued.
generateNotificationId :: N Word32
generateNotificationId = gets $ view notificationCount

-- | Removes a notification from internal state. The removed notification, if found, is returned.
removeNotification :: Word32 -> N (Maybe Notification)
removeNotification notificationId = do
  removedNotification <- gets $ lookup notificationId . view notifications
  modify . over notifications $ delete notificationId
  return removedNotification

-- | Transforms an existing notification given its ID and a pure function to apply to it.
updateNotification :: Word32 -> (Notification -> Notification) -> N ()
updateNotification i f = modify . over notifications $ adjust f i

-- | Replaces an existing notification given its ID and a new 'Notification'.
replaceNotification :: Word32 -> Notification -> N ()
replaceNotification i n = updateNotification i (const n)
