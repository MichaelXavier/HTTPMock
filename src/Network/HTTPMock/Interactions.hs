{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMock.Interactions (getResponse) where

import ClassyPrelude
import Control.Lens

import Network.HTTPMock.Types

--TODO: record the request, update canned response and junk
getResponse :: Request -> HTTPMocker -> (Maybe FakeResponse, HTTPMocker)
getResponse req mocker = (lookupResponse req mocker, mocker)

lookupResponse :: Request -> HTTPMocker -> Maybe FakeResponse
lookupResponse req mocker = lookupWithResponder req $ mocker ^. responder . fakedResponses

lookupWithResponder :: Request -> [(RequestMatcher, CannedResponse)] -> Maybe FakeResponse
lookupWithResponder req [] = Nothing
lookupWithResponder req ((reqMatcher, AlwaysReturns resp):responses)
  | (reqMatcher ^. matcher) req = Just resp
  | otherwise                   = lookupWithResponder req responses
