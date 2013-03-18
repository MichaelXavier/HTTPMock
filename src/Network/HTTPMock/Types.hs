{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module Network.HTTPMock.Types ( HasRequestMatcher(..)
                              , RequestMatcher(..)
                              , HasFakedResponder(..)
                              , FakeResponse
                              , FakedInteraction
                              , CannedResponse(..)
                              , FakedResponder
                              , HasMockerOptions(..)
                              , MockerOptions(..)
                              , HasHTTPMocker(..)
                              , HTTPMocker(..)
                              , Port
                              , Request(..)) where

import ClassyPrelude hiding (show)
import Data.Default
import qualified Data.NonEmpty as NE
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
data CannedResponse = ReturnsSequence (NE.T [] FakeResponse) |
                      AlwaysReturns FakeResponse deriving (Show, Eq)

type FakedInteraction = (RequestMatcher, CannedResponse)

newtype FakedResponder = FakedResponder { _fakedInteractions :: [FakedInteraction] } deriving (Show)

makeClassy ''FakedResponder

--TODO: inelegant
instance Eq FakedResponder where
  a == b = (map snd $ a ^. fakedInteractions) == (map snd $ b ^. fakedInteractions)

instance Default FakedResponder where
  def = FakedResponder mempty

data MockerOptions = MockerOptions {
    _mockerPort :: Port
} deriving (Show, Eq)

makeClassy ''MockerOptions

instance Default MockerOptions where
  def = MockerOptions 4568

data HTTPMocker = HTTPMocker {
    _responder :: FakedResponder
  , _options   :: MockerOptions
  , _recordedRequests :: Seq Request
}

makeClassy ''HTTPMocker

-- gross
instance Show HTTPMocker where
  show m = "HTTPMocker { _responder = " ++ show (m ^. responder) ++ ", _options = " ++ show (m ^. options) ++ "}"

-- gross
instance Eq HTTPMocker where
  a == b = a ^. responder == b ^. responder && a ^. options == b ^. options

instance Default HTTPMocker where
  def = HTTPMocker def def def
