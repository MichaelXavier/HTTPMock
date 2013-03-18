{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMockSpec (spec) where

import ClassyPrelude
import Control.Lens
import Data.Default
import Network.HTTP.Types (Method)

import Network.HTTPMock

import Test.Hspec

spec :: Spec
spec = do
  describe "resetRecorder" $ do
    let mocker = def :: HTTPMocker
    it "does nothing on a mocker with nothing recorded" $
      resetRecorder mocker `shouldHaveRecordedRequests` mocker

shouldHaveRecordedRequests :: HTTPMocker -> HTTPMocker -> Expectation
shouldHaveRecordedRequests a b = aReqs `shouldBe` bReqs
  where summarizeReqeusts mocker = map extractRequestVitals $ mocker ^. recordedRequests -- see if we can use mapped instead
        aReqs = summarizeReqeusts a
        bReqs = summarizeReqeusts b

extractRequestVitals :: Request -> (Method, Text)
extractRequestVitals req = (meth, joinedPath)
  where meth       = requestMethod req
        joinedPath = intercalate "/" $ pathInfo req
