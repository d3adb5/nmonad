{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module NMonad.OperationsSpec (spec) where

import Prelude hiding (lookup)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.NMonad
import Test.QuickCheck

import Control.Lens (over, view)

import Data.Map
import Data.Word (Word32)

import NMonad.Operations
import NMonad.Core (NState)
import NMonad.Lenses

nonEmptyState :: Gen NState
nonEmptyState = arbitrary `suchThat` ((/= empty) . view notifications)

stateAndAbsentId :: Gen (NState, Word32)
stateAndAbsentId = do
  generatedState <- arbitrary
  absentId <- arbitrary `suchThat` flip notMember (view notifications generatedState)
  return (generatedState, absentId)

spec :: Spec
spec = do
  describe "indexNotification :: Notification -> N Word32" $ do
    prop "adds a notification to internal state" $ \notif -> do
      (returnedId, finalState, (_, initialState)) <- runGenN $ indexNotification notif
      returnedId `shouldBe` view identifier notif
      lookup (view identifier notif) (view notifications finalState) `shouldBe` Just notif
      view notifications finalState `shouldBe` insert (view identifier notif) notif (view notifications initialState)

  describe "removeNotification :: Word32 -> N (Maybe Notification)" $ do
    prop "removes an existing notification and returns it" . forAll nonEmptyState $ \nstate -> do
      let targetNotification = head . elems $ view notifications nstate
      (removedNotification, finalState, _) <- runGenN' nstate $ removeNotification (view identifier targetNotification)
      removedNotification `shouldBe` Just targetNotification
      lookup (view identifier targetNotification) (view notifications finalState) `shouldBe` Nothing

    prop "makes no change and yields nothing when removing absent notification" . forAll stateAndAbsentId $ \(s,i) -> do
      (removedNotification, finalState, _) <- runGenN' s $ removeNotification i
      removedNotification `shouldBe` Nothing
      finalState `shouldBe` s

  describe "updateNotification :: Word32 -> (Notification -> Notification) -> N ()" $ do
    prop "updates an existing notification" . forAll nonEmptyState $ \nstate -> do
      let targetNotification  = head . elems $ view notifications nstate
          targetId = view identifier targetNotification
          updateSummary = over summary ("updated " <>)
      (_, finalState, _) <- runGenN' nstate $ updateNotification targetId updateSummary
      lookup targetId (view notifications finalState) `shouldBe` Just (updateSummary targetNotification)

    prop "makes no change when updating absent notification" . forAll stateAndAbsentId $ \(s,i) -> do
      (_, finalState, _) <- runGenN' s $ updateNotification i (const undefined)
      finalState `shouldBe` s
