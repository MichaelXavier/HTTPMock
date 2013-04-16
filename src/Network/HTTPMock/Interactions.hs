{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Network.HTTPMock.Interactions (getResponse) where

import ClassyPrelude
import Control.Lens
import Debug.Trace (traceShow)
import Data.NonEmpty ((!:))
import qualified Data.NonEmpty as NE

import Network.HTTPMock.Types

--TODO: record the request, update canned response and junk
-- < edwardk> a Lens is defined as
-- < edwardk> forall f. Functor f => (a -> f b) -> s -> f t
-- < edwardk> findAndUpdateInteraction :: [FakedInteraction] -> f [FakedInteraction]         
-- < NemesisD> and what's the t in Lens s t a b?
-- < Taneb> target
-- < edwardk> where f = (,) (Maybe Response)
-- < edwardk> interactions ::  Functor f => ([Interaction] -> f [Interaction]) -> Mocker -> f Mocker
-- < edwardk> when f = (,) (Maybe Response) that is
-- < edwardk> interactions ::  Functor f => ([Interaction] -> (Maybe Response, [Interaction])) -> Mocker -> (Maybe Response, Mocker)
-- < edwardk> interactions :: ([Interaction] -> (Maybe Response, [Interaction])) -> Mocker -> (Maybe Response, Mocker)
-- < edwardk> well, we have one of those lying around, so we pass it findAndUpdateInteraction and get the signature you want
-- < edwardk> for interactions: s = t = Mocker, a = b = [Interaction]
getResponse :: Request -> HTTPMocker -> (Maybe FakeResponse, HTTPMocker)
getResponse req mocker = mocker' & responder . fakedInteractions %%~ findAndUpdateInteraction
  where findAndUpdateInteraction = matchResponse req
        mocker' = mocker & recordedRequests <>~ singleton req

-- something like (uncurry (++) . (id *** f) . break predicate) 
-- At 0 might help with implementing f
-- replace f with onHead update, onHead _ [] = [] ; onHead f (x:xs) = f x:xs

-- (\p u -> over (taking 1 (traverse . filtered p)) u) even (+2) [1..5]

matchResponse :: Request -> [FakedInteraction] -> (Maybe FakeResponse, [FakedInteraction])
matchResponse req interactions = findResponse $ break (match . fst) interactions
  where match rMatcher = rMatcher ^. matcher $ req
        findResponse (misses, []) = (Nothing, misses)
        findResponse (misses, ((m, cResp):rest)) = let (resp, cResp') = modifyOnMatch cResp in (Just resp, misses ++ ((m, cResp'):rest))


modifyOnMatch :: CannedResponse -> (FakeResponse, CannedResponse)
modifyOnMatch unmodified@(ReturnsSequence (NE.Cons resp []))      = (resp, unmodified)
modifyOnMatch (ReturnsSequence (NE.Cons curResp (nextResp:rest))) = (curResp, ReturnsSequence $ nextResp !: rest)
modifyOnMatch cResp@(AlwaysReturns resp)                          = (resp, cResp)
