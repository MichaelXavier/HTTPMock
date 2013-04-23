{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMock.SpecHelper ( okFakeResponse
                                   , matchPathMocker
                                   , sequenceMocker
                                   , matchMethodMocker
                                   , getBody
                                   , postReturningBody
                                   , get_
                                   , shouldHaveSameRecordedRequests
                                   , postResponse
                                   , firstResponse
                                   , secondResponse
                                   , yep
                                   , returnsSequence
                                   , responseAfterTimes
                                   , mockerAfterTimes
                                   , req
                                   , reqMatcher
                                   , module Network.HTTPMock.Expectations) where

import Prelude (last)
import ClassyPrelude
import Control.Lens
import Control.Monad (replicateM_)
import Control.Monad.Trans.State ( execState
                                 , evalState
                                 , state)
import Data.Default
import qualified Data.Text.Lazy as LT
import Network.HTTP.Types ( Method
                          , http11)
import Network.Http.Client ( get
                           , post
                           , emptyBody
                           , inputStreamBody
                           , concatHandler')
import System.IO.Streams (fromLazyByteString)

import Network.HTTPMock
import Network.HTTPMock.Interactions (getResponse)
import Network.HTTPMock.Expectations
import Network.HTTPMock.Utils

import Test.Hspec

okFakeResponse :: LT.Text -> FakeResponse
okFakeResponse body = def & responseBody .~ body

matchPathMocker :: HTTPMocker
matchPathMocker = def & responder . fakedInteractions <>~ singleton (fooMatcher, AlwaysReturns $ okFakeResponse "fake-response")
  where fooMatcher = matchPath "/foo/bar"

sequenceMocker :: HTTPMocker
sequenceMocker = def & responder . fakedInteractions <>~ singleton (fooMatcher, ReturnsSequence $ firstResponse !: [secondResponse])
  where fooMatcher = matchPath "/foo/bar"

matchMethodMocker :: HTTPMocker
matchMethodMocker = def & responder . fakedInteractions <>~ singleton (postMatcher, AlwaysReturns postResponse)
  where postMatcher = matchMethod "POST"

getBody url = get url concatHandler'

postReturningBody url = do
  stream <- fromLazyByteString "POSTBODY"
  post url "application/json" (inputStreamBody stream) concatHandler'

get_ url = get url discard
  where discard _ _ = return ()

shouldHaveSameRecordedRequests :: HTTPMocker -> HTTPMocker -> Expectation
shouldHaveSameRecordedRequests a b = aReqs `shouldBe` bReqs
  where aReqs = summarizeRequests a
        bReqs = summarizeRequests b

postResponse :: FakeResponse
postResponse = okFakeResponse "you sent a POST"

firstResponse :: FakeResponse
firstResponse  = okFakeResponse "first-response"

secondResponse :: FakeResponse
secondResponse = okFakeResponse "second-response"

yep :: FakeResponse
yep = okFakeResponse "yep"

reqMatcher :: RequestMatcher
reqMatcher = RequestMatcher (\reqToCheck -> "/foo" == reqToCheck ^. rawPathInfo)

req :: Request
req = Request {
  _requestMethod     = "GET"
, _rawPathInfo       = "/foo"
, _rawQueryString    = "?wat=true"
, _serverName        = "example.com"
, _serverPort        = 4568
, _requestHeaders    = []
, _pathInfo          = ["foo"]
, _queryString       = [("wat", Just "true")]
, _requestBody       = "FAKEBODY"
}

returnsSequence :: CannedResponse
returnsSequence = ReturnsSequence $ firstResponse !: [secondResponse]

responseAfterTimes :: Int -> Request -> HTTPMocker -> Maybe FakeResponse
responseAfterTimes times req = last . runStatefully (getResponse req)
  where runStatefully = evalState . replicateM times . state

mockerAfterTimes :: Int -> Request -> HTTPMocker -> HTTPMocker
mockerAfterTimes times req = runStatefully (getResponse req)
  where runStatefully = execState . replicateM_ times . state
