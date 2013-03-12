{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMock.InteractionsSpec (spec) where

import ClassyPrelude
import Control.Lens
import Data.Default
import Network.HTTP.Types (http11)
import Network.Wai (RequestBodyLength(KnownLength))
import Network.Socket (SockAddr(SockAddrUnix))
import Test.Hspec

import Network.HTTPMock.Types
import Network.HTTPMock.Interactions

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
      let matcher = RequestMatcher (\reqToCheck -> "/foo" == rawPathInfo reqToCheck)
      let mocker = def & responder %~ fakedResponses <>~ [(matcher, AlwaysReturns "yep")]
      it "returns the stubbed response" $
        getResponse req mocker ^. _1 `shouldBe` Just "yep"
      it "returns an unmodified Mocker" $
        getResponse req mocker ^. _2 `shouldBe` mocker

req :: Request
req = Request {
    requestMethod     = "GET"
  , httpVersion       = http11
  , rawPathInfo       = "/foo"
  , rawQueryString    = "?wat=true"
  , serverName        = "example.com"
  , serverPort        = 4568
  , requestHeaders    = []
  , isSecure          = False
  , remoteHost        = SockAddrUnix "doesntmatter"
  , pathInfo          = ["foo"]
  , queryString       = [("wat", Just "true")]
  , requestBody       = undefined
  , vault             = undefined
  , requestBodyLength = KnownLength 0
}
