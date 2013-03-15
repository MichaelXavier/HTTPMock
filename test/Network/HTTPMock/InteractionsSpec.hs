{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMock.InteractionsSpec (spec) where

import Prelude (last)
import ClassyPrelude
import Control.Lens
import Control.Monad (replicateM_)
import Control.Monad.Trans.State ( execState
                                 , evalState
                                 , state)
import Data.Default
import Data.NonEmpty ((!:))
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
      let mocker = def & responder %~ fakedInteractions <>~ [(reqMatcher, AlwaysReturns "yep")]
      it "returns the stubbed response" $
        getResponse req mocker ^. _1 `shouldBe` Just "yep"
      it "returns an unmodified Mocker" $
        getResponse req mocker ^. _2 `shouldBe` mocker

    describe "with MatchSequence match" $ do
      let mocker = def & responder %~ fakedInteractions <>~ [(reqMatcher, returnsSequence)]
      describe "first match" $ do
        it "returns the first response" $
          responseAfterTimes 1 req mocker `shouldBe` Just "first-response"
      describe "second match" $ do
        it "returns the second response" $
          responseAfterTimes 2 req mocker `shouldBe` Just "second-response"
      describe "all subsequent matches" $ do
        it "returns the final response" $
          responseAfterTimes 3 req mocker `shouldBe` Just "second-response"

reqMatcher :: RequestMatcher
reqMatcher = RequestMatcher (\reqToCheck -> "/foo" == rawPathInfo reqToCheck)

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

returnsSequence :: CannedResponse
returnsSequence = ReturnsSequence $ "first-response" !: ["second-response"]

responseAfterTimes :: Int -> Request -> HTTPMocker -> Maybe FakeResponse
responseAfterTimes times req = last . runStatefully (getResponse req)
  where runStatefully = evalState . replicateM times . state

mockerAfterTimes :: Int -> Request -> HTTPMocker -> HTTPMocker
----- execState . replicateM 3 . state :: (s -> (a, s)) -> s -> s
mockerAfterTimes times req = runStatefully (getResponse req)
  where runStatefully = execState . replicateM_ times . state
