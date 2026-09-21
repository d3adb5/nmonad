{-# LANGUAGE OverloadedStrings #-}

module NMonad.DBusSpec (spec) where

import Paths_nmonad (getDataFileName)

import Control.Concurrent (newEmptyMVar)
import Control.Monad.IO.Class
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (isInfixOf)
import System.IO.Silently (silence)
import System.Posix.User (getEffectiveUserID, getEffectiveGroupID)
import Test.Hspec
import Test.Main (withEnv)
import TestContainers.Hspec

import NMonad.DBus (listenForNotifications)

dbusSocketDir :: Text
dbusSocketDir = "/tmp/nmonad-dbus"

dbusAddress :: Text
dbusAddress = "unix:path=" <> dbusSocketDir <> "/bus.sock"

withMockDbusAddr :: IO a -> IO a
withMockDbusAddr = withEnv [("DBUS_SESSION_BUS_ADDRESS", Just $ unpack dbusAddress)]

containers :: (MonadDocker m, MonadIO m) => m ()
containers = do
  dbusBuildPlan <- liftIO $ fromBuildContext <$> getDataFileName "dbus-daemon" <*> pure Nothing
  -- the daemon's EXTERNAL auth needs to resolve our uid, hand it over and treat in entrypoint.sh
  (uid, gid) <- (,) <$> liftIO getEffectiveUserID <*> liftIO getEffectiveGroupID
  let dbusRequest = setEnv [("TEST_UID", pack (show uid)), ("TEST_GID", pack (show gid))]
                  . setWaitingFor (waitForLogLine Stdout ("unix:" `isInfixOf`))
                  . setVolumeMounts [(dbusSocketDir, dbusSocketDir)]
                  . containerRequest
  _dbusContainer <- build dbusBuildPlan >>= run . dbusRequest
  return ()

spec :: Spec
spec = around (withMockDbusAddr . withContainers containers) $ do
  describe "listenForNotifications :: MVar (DBusNotification, MVar Word32) -> IO ()" $ do
    it "connects to the containerized daemon and claims the notifications name" $  do
      mailbox <- newEmptyMVar
      silence $ listenForNotifications mailbox
    it "raises a failure when unable to own org.freedesktop.Notifications" $ do
      pending
