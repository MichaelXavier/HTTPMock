{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMockSpec (spec) where

import ClassyPrelude
import Control.Rematch
import Data.Default

import Network.HTTPMock

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

    it "handles non-GET requests too" $ withMocker_ (matchMethodMocker "POST") $ do
      doPost `shouldReturn` "you sent a POST"

    it "records requests" $
      ("GET", "/foo/bar") `shouldBeRequestedOnceBy`
        (fst <$> (runWithMocker_ def $ get_ url))
    
    it "records request to bogus paths" $
      ("GET", "/bogus/path") `shouldBeRequestedOnceBy`
        (fst <$> (runWithMocker_ def $ get_ "http://127.0.0.1:4568/bogus/path"))

  describe "matchResultFromMocker" $ do
    it "matches on the result of the action using the mocker" $
      matchResultFromMocker (matchMethodMocker "POST") doPost $ is "you sent a POST"


  describe "matchResultingMocker" $ do
    it "matches against the modified mocker" $
      matchResultingMocker (matchMethodMocker "POST") doPost $
        allRequestsMatch [("POST", "/foo/bar")]

  describe "hasRequestWithBody" $ do
    it "matches the positive case" $ do
      matchResultingMocker (matchMethodMocker "POST") doPost $
        hasRequestWithBody "POSTBODY"

    it "matches the negative case" $ do
      matchResultingMocker (matchMethodMocker "POST") doPost $
        isNot $ hasRequestWithBody "SOMETHINGELSE"

    it "works correctly on PUT" $ do
      matchResultingMocker (matchMethodMocker "PUT") doPut $
        hasRequestWithBody "POSTBODY"

    it "works correctly on DELETE" $ do
      matchResultingMocker (matchMethodMocker "DELETE") doDelete $
        hasRequestWithBody "POSTBODY"
      

  where url    = "http://127.0.0.1:4568/foo/bar"
        doPost   = postReturningBody url
        doPut    = putReturningBody url
        doDelete = deleteReturningBody url
