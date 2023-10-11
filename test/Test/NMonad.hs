{-# LANGUAGE RecordWildCards #-}

module Test.NMonad where

import Control.Concurrent (newEmptyMVar)
import Data.Map (empty, fromList)
import Data.Text (pack, Text)
import System.IO.Unsafe (unsafePerformIO)

import Test.QuickCheck

import NMonad.Core

instance Arbitrary Text where
  arbitrary = fmap pack arbitrary

instance Arbitrary a => Arbitrary (N a) where
  arbitrary = fmap pure arbitrary

instance Arbitrary NEnv where
  arbitrary = NEnv (unsafePerformIO newEmptyMVar) <$> arbitrary

-- | This instance of 'Arbitrary' always generates nmonad configuration that enable notification replacement.
instance Arbitrary NConfig where
  arbitrary = do
    defaultTimeout <- arbitrary
    let disableReplacement = False
        dbusNotificationHook = return . Just
        notificationHook = return . Just
    return NConfig {..}

instance Arbitrary NState where
  arbitrary = do
    notifications <- fromList . map (\n -> (identifier n, n)) <$> arbitrary
    let notificationCount = fromIntegral $ length notifications
    return NState {..}

instance Arbitrary Notification where
  arbitrary = do
    applicationName <- arbitrary
    applicationIcon <- arbitrary
    summary <- arbitrary
    body <- arbitrary
    identifier <- arbitrary
    actions <- arbitrary
    timeout <- arbitrary
    return Notification {hints = empty, ..}

instance Arbitrary Expiration where
  arbitrary = do
    randomMilliseconds <- arbitrary
    elements [ServerDefault, Never, Milliseconds randomMilliseconds]

instance Arbitrary DBusNotification where
  arbitrary = DBusNotification <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> pure empty <*> arbitrary

-- | Run an action in the 'N' monad with a generated initial environment and state.
runGenN :: N a -> IO (a, NState, (NEnv, NState))
runGenN action = generate arbitrary >>= flip runGenN' action

-- | Run an action in the 'N' monad with a generated initial environment and given initial state.
runGenN' :: NState -> N a -> IO (a, NState, (NEnv, NState))
runGenN' st action = do
  initialEnvironment <- generate arbitrary
  (wrapped, finalState) <- runN initialEnvironment st action
  return (wrapped, finalState, (initialEnvironment, st))
