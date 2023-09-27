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

instance Arbitrary NConfig where
  arbitrary = do
    defaultTimeout <- arbitrary
    disableReplacement <- arbitrary
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
runGenN action = do
  initialEnvironment <- generate arbitrary
  initialState <- generate arbitrary
  (wrapped, finalState) <- runN initialEnvironment initialState action
  return (wrapped, finalState, (initialEnvironment, initialState))
