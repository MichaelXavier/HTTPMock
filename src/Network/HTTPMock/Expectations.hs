{-# LANGUAGE NoImplicitPrelude #-}
module Network.HTTPMock.Expectations ( RequestSummary
                                     , Method
                                     , allRequestsMatch
                                     , summarizeRequests
                                     , shouldBeOnlyRequestsBy
                                     , shouldBeRequestedBy
                                     , shouldBeRequestedOnceBy
                                     , shouldMatchRequestsMadeBy
                                     , shouldMakeRequestWithHeader
                                     , shouldMatchOneRequestMadeBy) where

import ClassyPrelude
import Control.Lens
import Control.Rematch ( Matcher(..)
                       , standardMismatch)
import Data.CaseInsensitive (mk)
import Network.HTTP.Types (Method)

import Network.HTTPMock.Types
import Network.HTTPMock.Utils ( requestPath
                              , requestHeaders)

import Test.Hspec ( Expectation
                  , shouldBe)

type RequestSummary = (Method, Text)

allRequestsMatch :: [RequestSummary] -> Matcher HTTPMocker
allRequestsMatch summaries = Matcher pred msg mismatchMsg
  where msg         = "made requests " ++ show summaries
        mismatchMsg = ("but requested " ++) . show . summarizeRequests
        pred        = (summaries ==) . summarizeRequests

shouldBeOnlyRequestsBy :: [RequestSummary] -> IO HTTPMocker -> Expectation
shouldBeOnlyRequestsBy expected action = do
  mocker <- action
  summarizeRequests mocker `shouldBe` expected

shouldBeRequestedBy :: Int -> RequestSummary -> IO HTTPMocker -> Expectation
shouldBeRequestedBy count expected action = do
  mocker <- action
  let matches = filter (== expected) $ summarizeRequests mocker
  matches `shouldBe` replicate count expected

shouldBeRequestedOnceBy :: RequestSummary -> IO HTTPMocker -> Expectation
shouldBeRequestedOnceBy = shouldBeRequestedBy 1

shouldMatchRequestsMadeBy :: Int -> (Request -> Bool) -> IO HTTPMocker -> Expectation
shouldMatchRequestsMadeBy count pred action = do
  mocker <- action
  print $ summarizeRequests mocker
  let matching = filter pred $ getRequests mocker
  length matching `shouldBe` count

shouldMatchOneRequestMadeBy :: (Request -> Bool) -> IO HTTPMocker -> Expectation
shouldMatchOneRequestMadeBy = shouldMatchRequestsMadeBy 1

shouldMakeRequestWithHeader :: (ByteString, ByteString) -> IO HTTPMocker -> Expectation
shouldMakeRequestWithHeader (key, val) = shouldMatchOneRequestMadeBy pred
  where pred = (== Just val) . lookupHeader key

lookupHeader :: ByteString -> Request -> Maybe ByteString
lookupHeader key = lookup key' . requestHeaders
  where key' = mk key

---- Helpers

summarizeRequests :: HTTPMocker -> [RequestSummary]
summarizeRequests = map extractRequestSummary . getRequests

getRequests :: HTTPMocker -> [Request]
getRequests mocker = toList $ mocker ^. recordedRequests

extractRequestSummary :: Request -> RequestSummary
extractRequestSummary req = (meth, joinedPath)
  where meth       = requestMethod req
        joinedPath = requestPath req
