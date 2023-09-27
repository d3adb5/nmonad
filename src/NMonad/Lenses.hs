------------------------------------------------------------------------------------------------------------------------
-- |
-- Module      :  NMonad.Lenses
-- Description :  Convenient lenses for NMonad types.
-- Copyright   : (c) d3adb5 2023
-- License     : BSD3
-- Maintainer  : d3adb5 <me@d3adb5.net>
-- Stability   : experimental
--
-- This module provides lenses for some of the types defined in 'NMonad.Core' based vaguely on convenience. This is done
-- separately from the core module itself to avoid relying on lenses or awkwardly prefixed field names when performing
-- basic operations on core types.
------------------------------------------------------------------------------------------------------------------------

module NMonad.Lenses
  ( -- NState
    notifications
  , notificationCount

    -- Notification
  , applicationName
  , applicationIcon
  , summary
  , body
  , identifier
  , actions
  , hints
  , timeout
  ) where

import Control.Lens (Lens', lens)

import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word32)
import DBus (Variant)

import qualified NMonad.Core as Core

notifications :: Lens' Core.NState (Map Word32 Core.Notification)
notifications = lens Core.notifications (\s a -> s { Core.notifications = a })

notificationCount :: Lens' Core.NState Word32
notificationCount = lens Core.notificationCount (\s a -> s { Core.notificationCount = a })

applicationName :: Lens' Core.Notification Text
applicationName = lens Core.applicationName (\n an -> n { Core.applicationName = an })

applicationIcon :: Lens' Core.Notification Text
applicationIcon = lens Core.applicationIcon (\n ai -> n { Core.applicationIcon = ai })

summary :: Lens' Core.Notification Text
summary = lens Core.summary (\n s -> n { Core.summary = s })

body :: Lens' Core.Notification Text
body = lens Core.body (\n b -> n { Core.body = b })

identifier :: Lens' Core.Notification Word32
identifier = lens Core.identifier (\n i -> n { Core.identifier = i })

actions :: Lens' Core.Notification [Text]
actions = lens Core.actions (\n as -> n { Core.actions = as })

hints :: Lens' Core.Notification (Map Text Variant)
hints = lens Core.hints (\n hs -> n { Core.hints = hs })

timeout :: Lens' Core.Notification Core.Expiration
timeout = lens Core.timeout (\n t -> n { Core.timeout = t })
