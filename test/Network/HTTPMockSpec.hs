{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMockSpec (spec) where

import ClassyPrelude
import Data.Default

import Network.HTTPMock
import Network.HTTPMock.Expectations

import Network.HTTPMock.SpecHelper
import Test.Hspec

spec :: Spec
spec = do
  describe "resetRecorder" $ do
    let mocker = def :: HTTPMocker
    it "does nothing on a mocker with nothing recorded" $
      resetRecorder mocker `shouldHaveSameRecordedRequests` mocker

  describe "withMocker" $ do
    it "mocks requests" $ withMocker_ matchPathMocker $ do
      getBody url `shouldReturn` "fake-response"

    it "mocks sequences" $ withMocker_ sequenceMocker $ do
      replicateM 3 (getBody url) `shouldReturn` ["first-response", "second-response", "second-response"]

    it "handles non-GET requests too" $ withMocker_ matchMethodMocker $ do
      postReturningBody url `shouldReturn` "you sent a POST"

    it "records requests" $ ("GET", "/foo/bar") `shouldBeRequestedOnceBy`
                            (runWithMocker_ def $ get_ url)
    
    it "records request to bogus paths" $ ("GET", "/bogus/path") `shouldBeRequestedOnceBy`
                                          (runWithMocker_ def $ get_ "http://127.0.0.1:4568/bogus/path")
  where url = "http://127.0.0.1:4568/foo/bar"
