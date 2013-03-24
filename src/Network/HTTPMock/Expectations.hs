{-# LANGUAGE NoImplicitPrelude #-}
module Network.HTTPMock.Expectations ( RequestSummary
                                     , Method
                                     , shouldBeOnlyRequestsBy
                                     , shouldBeRequestedBy
                                     , shouldBeRequestedOnceBy
                                     , shouldMatchRequestsMadeBy
                                     , shouldMatchOneRequestMadeBy) where

import ClassyPrelude
import Control.Lens
import Network.HTTP.Types (Method)

import Network.HTTPMock.Types
import Network.HTTPMock.Utils (requestPath)

import Test.Hspec ( Expectation
                  , shouldBe)

type RequestSummary = (Method, Text)

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
  let matching = filter pred $ getRequests mocker
  length matching `shouldBe` count

shouldMatchOneRequestMadeBy :: (Request -> Bool) -> IO HTTPMocker -> Expectation
shouldMatchOneRequestMadeBy = shouldMatchRequestsMadeBy 1

---- Helpers

summarizeRequests :: HTTPMocker -> [RequestSummary]
summarizeRequests = map extractRequestSummary . getRequests

getRequests :: HTTPMocker -> [Request]
getRequests mocker = toList $ mocker ^. recordedRequests

extractRequestSummary :: Request -> RequestSummary
extractRequestSummary req = (meth, joinedPath)
  where meth       = requestMethod req
        joinedPath = requestPath req
