{-# LANGUAGE OverloadedStrings #-}

module NMonad.DBusSpec (spec) where

import Paths_nmonad (getDataFileName)

import Control.Concurrent (newEmptyMVar)
import Control.Monad.IO.Class
import Data.Maybe (fromJust)
import Data.Text (pack, isPrefixOf)
import Data.Text.Lazy (isInfixOf)
import Data.Typeable (typeOf)
import DBus (Address, parseAddress)
import DBus.Client (connect)
import System.IO.Error (ioeGetErrorString)
import System.IO.Silently (silence)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.User (getEffectiveUserID, getEffectiveGroupID)
import Test.Hspec
import TestContainers.Hspec

import NMonad.DBus (listenForNotificationsWith)

dbusAddress :: FilePath -> Address
dbusAddress hostDir = fromJust . parseAddress $ "unix:path=" ++ hostDir ++ "/bus.sock"

withDbusAddress :: (Address -> IO ()) -> IO ()
withDbusAddress test = withSystemTempDirectory "nmonad-dbus" $ \hostDir -> do
  withContainers (containers hostDir) (\() -> test $ dbusAddress hostDir)

containers :: (MonadDocker m, MonadIO m) => FilePath -> m ()
containers hostDir = do
  dbusBuildPlan <- liftIO $ fromBuildContext <$> getDataFileName "dbus-daemon" <*> pure Nothing
  -- the daemon's EXTERNAL auth needs to resolve our uid, hand it over and treat in entrypoint.sh
  (uid, gid) <- (,) <$> liftIO getEffectiveUserID <*> liftIO getEffectiveGroupID
  let dbusRequest = setEnv [("TEST_UID", pack (show uid)), ("TEST_GID", pack (show gid))]
                  . setWaitingFor (waitForLogLine Stdout ("unix:" `isInfixOf`))
                  . setVolumeMounts [(pack hostDir, "/tmp/nmonad-dbus")]
                  . containerRequest
  _dbusContainer <- build dbusBuildPlan >>= run . dbusRequest
  return ()

spec :: Spec
spec = around withDbusAddress $ do
  describe ("listenForNotificationsWith :: " ++ show (typeOf listenForNotificationsWith)) $ do
    it "connects to the containerized daemon and claims the notifications name" $ \addr -> do
      mailbox <- newEmptyMVar
      silence $ listenForNotificationsWith (connect addr) [] mailbox
    it "raises a failure when unable to own org.freedesktop.Notifications" $ \addr -> do
      mailbox <- newEmptyMVar
      silence $ listenForNotificationsWith (connect addr) [] mailbox
      let failureStart = "Failed to become primary owner of org.freedesktop.Notifications:"
      silence (listenForNotificationsWith (connect addr) [] mailbox)
        `shouldThrow` ((isPrefixOf failureStart) . pack . ioeGetErrorString)
