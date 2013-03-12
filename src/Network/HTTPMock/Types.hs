{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module Network.HTTPMock.Types ( HasRequestMatcher(..)
                              , RequestMatcher(..)
                              , HasFakedResponder(..)
                              , FakeResponse
                              , CannedResponse(..)
                              , FakedResponder
                              , HasMockerOptions(..)
                              , MockerOptions(..)
                              , HasHTTPMocker(..)
                              , HTTPMocker(..)
                              , Port
                              , Request(..)) where

import ClassyPrelude
import Data.Default
import qualified Data.Text.Lazy as LT
import Text.Show (Show(..)) -- why?
import Control.Lens
-- TODO: rexport Request
import Network.Wai (Request(..))
import Network.Wai.Handler.Warp (Port)

newtype RequestMatcher = RequestMatcher { _matcher :: Request -> Bool }

makeClassy ''RequestMatcher

instance Show RequestMatcher where
  show = const "RequestMatcher"

-- TODO: include status and junk
type FakeResponse = LT.Text

-- TODO: more responses
data CannedResponse = AlwaysReturns FakeResponse deriving (Show, Eq)

newtype FakedResponder = FakedResponder { _fakedResponses :: [(RequestMatcher, CannedResponse)] } deriving (Show)

makeClassy ''FakedResponder

--TODO: inelegant
instance Eq FakedResponder where
  a == b = (map snd $ a ^. fakedResponses) == (map snd $ b ^. fakedResponses)

instance Default FakedResponder where
  def = FakedResponder mempty

data MockerOptions = MockerOptions {
    _clearBetweenRequests :: Bool
  , _mockerPort :: Port
} deriving (Show, Eq)

makeClassy ''MockerOptions

instance Default MockerOptions where
  def = MockerOptions True 4568

data HTTPMocker = HTTPMocker {
    _responder :: FakedResponder
  , _options   :: MockerOptions
} deriving (Show, Eq)

makeClassy ''HTTPMocker

instance Default HTTPMocker where
  def = HTTPMocker def def
