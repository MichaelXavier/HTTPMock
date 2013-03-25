{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMock.InteractionsSpec (spec) where

import ClassyPrelude
import Data.Default
import Control.Lens

import Network.HTTPMock.Types
import Network.HTTPMock.Interactions

import Network.HTTPMock.SpecHelper
import Test.Hspec

spec :: Spec
spec = do
  describe "getResponse" $ do
    describe "no matches" $ do
      let mocker = def
      it "returns Nothing" $
        getResponse req mocker ^. _1 `shouldBe` Nothing
      it "returns an unmodified Mocker" $
        getResponse req mocker ^. _2 `shouldBe` mocker

    describe "with AlwaysReturn match" $ do
      let mocker = def & responder %~ fakedInteractions <>~ [(reqMatcher, AlwaysReturns yep)]
      it "returns the stubbed response" $
        getResponse req mocker ^. _1 `shouldBe` Just yep
      it "returns an unmodified Mocker" $
        getResponse req mocker ^. _2 `shouldBe` mocker
      it "always returns the stubbed response" $ do
        responseAfterTimes 2 req mocker `shouldBe` Just yep
      it "always returns the unmodified mocker" $ do
        mockerAfterTimes 2 req mocker `shouldBe` mocker

    describe "with MatchSequence match" $ do
      let mocker = def & responder %~ fakedInteractions <>~ [(reqMatcher, returnsSequence)]
      describe "first match" $ do
        it "returns the first response" $
          responseAfterTimes 1 req mocker `shouldBe` Just firstResponse
      describe "second match" $ do
        it "returns the second response" $
          responseAfterTimes 2 req mocker `shouldBe` Just secondResponse
      describe "all subsequent matches" $ do
        it "returns the final response" $
          responseAfterTimes 3 req mocker `shouldBe` Just secondResponse
