{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NMonad.Core
  ( N(..)
  , NEnv(..)
  , NConfig(..)
  , NState(..)
  , Expiration(..)
  , DBusNotification(..)
  , Notification(..)

  , notificationFromDBus
  , addToNotificationsAndReturnId
  , runN

  , module Control.Monad.Reader
  , module Control.Monad.State
  , module Data.Default
  ) where

import Control.Concurrent (MVar)
import Control.Monad.Reader
import Control.Monad.State

import Data.Default
import Data.Int (Int32)
import Data.Map (Map, insert)
import Data.Text (Text)
import Data.Word
import DBus (Variant)

-- | The 'N' monad, 'ReaderT' and 'StateT' transformers over 'IO', encapsulating the daemon's read-only environment and
-- state, respectively.
--
-- State can be retrieved with 'get', while read-only configuration can be retrieved with 'ask'. These are a result of
-- deriving the 'MonadReader' and 'MonadState' typeclasses.
--
newtype N a = N (ReaderT NEnv (StateT NState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader NEnv, MonadState NState)

-- | Daemon environment, containing daemon configuration and other globally shared read-only data.
data NEnv = NEnv
  { globalMailbox :: MVar (DBusNotification, MVar Word32) -- ^ Synchronization MVar for DBus notifications.
  , configuration :: NConfig                              -- ^ Daemon configuration.
  }

-- | Daemon configuration.
data NConfig = NConfig
  { defaultTimeout :: Int      -- ^ Default timeout for notification popups.
  , disableReplacement :: Bool -- ^ Whether to disable replacing notifications.
  }

instance Default NConfig where
  def = NConfig
    { defaultTimeout = 3
    , disableReplacement = False
    }

-- | The mutable state of the daemon.
data NState = NState
  { notificationCount :: Word32              -- ^ Counter for notification IDs.
  , notifications :: Map Word32 Notification -- ^ Notifications indexed by ID.
  } deriving (Show, Eq)

instance Default NState where
  def = NState 0 mempty

-- | Helper data type to represent when a given notification should expire.
data Expiration = ServerDefault | Never | Milliseconds Int32 deriving (Show, Eq)

fromTimeout :: Int32 -> Expiration
fromTimeout n
  | n < 0 = ServerDefault
  | n == 0 = Never
  | otherwise = Milliseconds n

-- | Raw notification data received from DBus.
--
-- The desktop notifications specification can be found in the following URL:
--   https://specifications.freedesktop.org/notification-spec/notification-spec-latest.html
--
data DBusNotification = DBusNotification Text Word32 Text Text Text [Text] (Map Text Variant) Int32

-- | A notification sent by some application and processed by NMonad.
data Notification = Notification
  { applicationName :: Text   -- ^ Name of the application sending the notification.
  , applicationIcon :: Text   -- ^ Icon for the application sending the notification.
  , summary :: Text           -- ^ A brief one-line summary of the notification.
  , body :: Text              -- ^ The optional detailed body text.
  , identifier :: Word32      -- ^ Unsigned integer ID for the notification.
  , actions :: [Text]         -- ^ A list of actions (buttons) associated with the notification.
  , hints :: Map Text Variant -- ^ A dictionary of hints passed along with the notification.
  , timeout :: Expiration     -- ^ When the notification should expire.
  } deriving (Show, Eq)

instance Default Notification where
  def = Notification mempty mempty mempty mempty 0 mempty mempty ServerDefault

-- | Produces a Notification given the data provided by DBus.
notificationFromDBus :: DBusNotification -> N Notification
notificationFromDBus (DBusNotification appName replacesId appIcon summ bod acts hnts tout) = do
  newNotificationId <- gets $ (+1) . notificationCount
  when (replacesId == 0) $ modify $ \s -> s { notificationCount = newNotificationId }
  return def
    { applicationName = appName
    , identifier = if replacesId == 0 then newNotificationId else replacesId
    , applicationIcon = appIcon
    , summary = summ
    , body = bod
    , actions = acts
    , hints = hnts
    , timeout = fromTimeout tout
    }

addToNotificationsAndReturnId :: Notification -> N Word32
addToNotificationsAndReturnId n = do
  modify $ \s -> s { notifications = insert (identifier n) n (notifications s) }
  return $ identifier n

-- | Run the 'N' monad.
runN :: NEnv -> NState -> N a -> IO (a, NState)
runN e s (N n) = runStateT (runReaderT n e) s
