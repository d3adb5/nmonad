{-# LANGUAGE StandaloneDeriving #-}

module NMonad.CoreSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.NMonad

import NMonad.Core

deriving instance Show DBusNotification

spec :: Spec
spec = do
  describe "notificationFromDBus :: DBusNotification -> N Notification" $ do
    prop "respects replacesId if set to something other than 0" $ \dbusnotif -> do
      replacesId <- generate $ arbitrary `suchThat` (/= 0)
      let (DBusNotification an _ ai s b a h t) = dbusnotif
          dbusnotif' = DBusNotification an replacesId ai s b a h t
      (notification, _, _) <- runGenN $ notificationFromDBus dbusnotif'
      identifier notification `shouldBe` replacesId

    prop "generates some notification id when replacesId is 0" $ \dbusnotif -> do
      let (DBusNotification an _ ai s b a h t) = dbusnotif
          dbusnotif' = DBusNotification an 0 ai s b a h t
      (notification, finalState, (_, initialState)) <- runGenN $ notificationFromDBus dbusnotif'
      identifier notification `shouldNotBe` 0
      notificationCount finalState `shouldBe` 1 + notificationCount initialState

  describe "addToNotificationsAndReturnId :: Notification -> N Word32" $ do
    prop "adds the notification to the state and returns its identifier" $ \notif -> do
      (returnedId, finalState, (_, initialState)) <- runGenN $ addToNotificationsAndReturnId notif
      notif `shouldNotSatisfy` (`elem` notifications initialState)
      notif `shouldSatisfy` (`elem` notifications finalState)
      identifier notif `shouldBe` returnedId
