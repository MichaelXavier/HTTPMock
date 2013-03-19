{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMockSpec (spec) where

import ClassyPrelude
import Control.Lens
import Data.Default
import Network.HTTP.Types (Method)
import Network.Http.Client (get)

import Network.HTTPMock
import Network.HTTPMock.Utils

import Test.Hspec

spec :: Spec
spec = do
  describe "resetRecorder" $ do
    let mocker = def :: HTTPMocker
    it "does nothing on a mocker with nothing recorded" $
      resetRecorder mocker `shouldHaveSameRecordedRequests` mocker

  describe "withMocker" $ do
    let startingMocker = def :: HTTPMocker
    --it "mocks requests" $ withMocker startingMocker $ \mockerR -> do
    it "records requests" $ withMocker startingMocker $ \mockerR -> do
      get_ "http://127.0.0.1:4568/foo/bar"
      mockerR `shouldHaveRecordedRequest` ("GET", "/foo/bar")


get_ url = get url discard
  where discard _ _ = return ()
      

type RequestSummary = (Method, Text)

shouldHaveSameRecordedRequests :: HTTPMocker -> HTTPMocker -> Expectation
shouldHaveSameRecordedRequests a b = aReqs `shouldBe` bReqs
  where aReqs = summarizeRequests a
        bReqs = summarizeRequests b

summarizeRequests :: HTTPMocker -> Seq RequestSummary
summarizeRequests mocker = map extractRequestSummary $ mocker ^. recordedRequests -- see if we can use mapped instead

shouldHaveRecordedRequest :: IORef HTTPMocker -> RequestSummary -> Expectation
shouldHaveRecordedRequest mockerR summary = do mocker <- readIORef mockerR
                                               let summarized = summarizeRequests mocker
                                               elem summary summarized `shouldBe` True


extractRequestSummary :: Request -> RequestSummary
extractRequestSummary req = (meth, joinedPath)
  where meth       = requestMethod req
        joinedPath = requestPath req
