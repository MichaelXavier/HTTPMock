{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMockSpec (spec) where

import qualified Data.Aeson as A
import Network.Http.Client ( emptyBody
                           , inputStreamBody
                           )
import qualified System.IO.Streams as S
import SpecHelper

spec :: Spec
spec = do
  describe "makeRequest" $ do
    describe "GET" $ do
      let req = buildReq GET "/hello"

      it "returns the correct result" $
        withDummyApp $ makeRequest req emptyBody concatHandler `shouldReturn` "hey"
    describe "POST" $ do
      let req = buildReq POST "/message"

      it "correctly posts the body" $ do
        bsBody <- S.fromByteString "four"
        withDummyApp $ makeRequest req (inputStreamBody bsBody) concatHandler `shouldReturn` "4"

    describe "JSON POST" $ do
      let msg = TestMessage "sup?"
      let req = buildReq POST "/json"

      it "can roundtrip JSON" $ do
        bsBody <- S.fromLazyByteString . A.encode $ msg
        withDummyApp $ makeRequest req (inputStreamBody bsBody) jsonHandler `shouldReturn` msg

  describe "fakeRequest" $ do
    describe "GET" $ do
      let req = buildReq GET "/hello"
      let faker = AlwaysReturns $ FakeResponse (buildResp 200 "OK") "fake hey"

      it "returns the faked result" $
        runFakeRequest faker req emptyBody concatHandler `shouldReturn` "fake hey"

      it "records the requests" $ do
        history <- execFakeRequest faker req emptyBody concatHandler
        expect history (hasRequest' 1 (withSignature GET "localhost" "/hello"))

    describe "POST" $ do
      let req = (buildReq POST "/message") { _reqHeaders = [("X-Top-Secret", "1")]}
      let faker = AlwaysReturns $ FakeResponse (buildResp 200 "OK") "faked length"

      it "returns the faked result" $ do
        bsBody <- S.fromByteString "four"
        runFakeRequest faker req (inputStreamBody bsBody) concatHandler `shouldReturn` "faked length"

      it "records the request" $ do
        bsBody <- S.fromByteString "four"
        history <- execFakeRequest faker req (inputStreamBody bsBody) concatHandler
        expect history (hasRequest' 1 (withSignature POST "localhost" "/message" `andAlso`
                                       withBody "four" `andAlso`
                                       withHeader ("x-top-secret", "1")))
  where buildReq m p = Request m "localhost" dummyPort p []
        buildResp code codeMsg = Response code codeMsg []
