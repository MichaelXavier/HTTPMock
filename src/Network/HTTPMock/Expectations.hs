{-# LANGUAGE NoImplicitPrelude #-}
module Network.HTTPMock.Expectations ( RequestSummary
                                     , Method

                                     , allRequestsMatch
                                     , hasRequestMatching
                                     , hasNumberOfRequestMatching
                                     , hasRequestWithHeader
                                     , hasRequestWithBody

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
import Data.Conduit ( ($$)
                    , runResourceT)
import Data.Conduit.List (consume)
import Network.HTTP.Types (Method)

import Network.HTTPMock.Types
import Network.HTTPMock.Utils (requestPath)

import Test.Hspec ( Expectation
                  , shouldBe)

type RequestSummary = (Method, Text)

allRequestsMatch :: [RequestSummary] -> Matcher HTTPMocker
allRequestsMatch summaries = Matcher pred msg mismatchMsg
  where msg         = "made requests " ++ show summaries
        mismatchMsg = ("but requested " ++) . show . summarizeRequests
        pred        = (summaries ==) . summarizeRequests

hasRequestMatching :: RequestSummary -> Matcher HTTPMocker
hasRequestMatching summary = Matcher pred msg mismatchMsg
  where msg         = "made request " ++ show summary
        mismatchMsg = ("but requested " ++) . show . summarizeRequests
        pred        = elem summary . summarizeRequests

hasNumberOfRequestMatching :: Int -> RequestSummary -> Matcher HTTPMocker
hasNumberOfRequestMatching count summary = Matcher pred msg mismatchMsg
  where msg         = "made request " ++ show summary ++ " " ++ show count ++ " times"
        mismatchMsg = ("but requested " ++) . show . summarizeRequests
        pred        = (== count) . length . filter (== summary) . summarizeRequests

hasRequestWithHeader :: Header -> Matcher HTTPMocker
hasRequestWithHeader (k,v) = Matcher pred msg mismatchMsg
  where msg         = "made request with header" ++ show (k,v)
        mismatchMsg = ("but headers were " ++) . unlines . map show . summarizeHeaders
        pred        = any hasHeader . summarizeHeaders
        hasHeader   = (== Just v) . lookup k

hasRequestWithBody :: ByteString -> Matcher HTTPMocker
hasRequestWithBody body = Matcher pred msg mismatchMsg
  where msg         = "made request with body" ++ show body
        mismatchMsg = ("but bodies were " ++) . unlines . map show . summarizeBodies
        pred        = any (== body) . summarizeBodies

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
lookupHeader key = lookup key' . _requestHeaders
  where key' = mk key

---- Helpers

summarizeRequests :: HTTPMocker -> [RequestSummary]
summarizeRequests = map extractRequestSummary . getRequests

summarizeHeaders :: HTTPMocker -> [RequestHeaders]
summarizeHeaders = map _requestHeaders . getRequests

summarizeBodies :: HTTPMocker -> [ByteString]
summarizeBodies = map _requestBody . getRequests

getRequests :: HTTPMocker -> [Request]
getRequests mocker = toList $ mocker ^. recordedRequests

extractRequestSummary :: Request -> RequestSummary
extractRequestSummary req = (meth, joinedPath)
  where meth       = req ^. requestMethod 
        joinedPath = requestPath req
