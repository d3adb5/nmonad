module NMonad.DBusSpec (spec) where

import Paths_nmonad (getDataFileName)

import Control.Monad.IO.Class
import Test.Hspec
import TestContainers.Hspec

containers :: (MonadDocker m, MonadIO m) => m ()
containers = do
  dbusBuildPlan <- liftIO $ fromBuildContext <$> getDataFileName "dbus-daemon" <*> pure Nothing
  _dbusContainer_ <- build dbusBuildPlan >>= run . containerRequest
  return ()

spec :: Spec
spec = around (withContainers containers) $ do
  describe "listenForNotifications :: MVar (DBusNotification, MVar Word32) -> IO ()" $ do
    it "should do something" $ do
      pending
