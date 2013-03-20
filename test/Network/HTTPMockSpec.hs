{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMockSpec (spec) where

import ClassyPrelude
import Control.Lens
import Data.Default
import Network.HTTP.Types (Method)
import Network.Http.Client ( get
                           , post
                           , emptyBody
                           , concatHandler')

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
    it "mocks requests" $ withMocker matchPathMocker $ \mockerR -> do
      getBody url `shouldReturn` "fake-response"

    it "mocks sequences" $ withMocker sequenceMocker $ \mockerR -> do
      replicateM 3 (getBody url) `shouldReturn` ["first-response", "second-response", "second-response"]

    it "handles non-GET requests too" $  withMocker matchMethodMocker $ \mockerR -> do
      postReturningBody url `shouldReturn` "you sent a POST"

    it "records requests" $ withMocker def $ \mockerR -> do
      get_ url
      mockerR `shouldHaveRecordedRequest` ("GET", "/foo/bar")
    
    it "records request to bogus paths" $ withMocker def $ \mockerR -> do
      get_ "http://127.0.0.1:4568/bogus/path"
      mockerR `shouldHaveRecordedRequest` ("GET", "/bogus/path")

  where url = "http://127.0.0.1:4568/foo/bar"

matchPathMocker :: HTTPMocker
matchPathMocker = def & responder . fakedInteractions <>~ singleton (fooMatcher, AlwaysReturns "fake-response")
  where fooMatcher = matchPath "/foo/bar"

sequenceMocker :: HTTPMocker
sequenceMocker = def & responder . fakedInteractions <>~ singleton (fooMatcher, ReturnsSequence $ "first-response" !: ["second-response"])
  where fooMatcher = matchPath "/foo/bar"

matchMethodMocker :: HTTPMocker
matchMethodMocker = def & responder . fakedInteractions <>~ singleton (postMatcher, AlwaysReturns "you sent a POST")
  where postMatcher = matchMethod "POST"

getBody url = get url concatHandler'

postReturningBody url = post url "application/json" emptyBody concatHandler'

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
