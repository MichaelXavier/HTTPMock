{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module Network.HTTPMock.Types ( HasRequestMatcher(..)
                              , RequestMatcher(..)
                              , HasFakedResponder(..)
                              , FakeResponse(..)
                              , HasFakeResponse(..)
                              , FakedInteraction
                              , CannedResponse(..)
                              , FakedResponder
                              , HasMockerOptions(..)
                              , MockerOptions(..)
                              , HasHTTPMocker(..)
                              , HTTPMocker(..)
                              , Port
                              , (!:)
                              , module Network.HTTP.Types.Status
                              , Request(..)) where

import ClassyPrelude hiding (show)
import Data.Default
import qualified Data.NonEmpty as NE
import Data.NonEmpty ((!:))
import qualified Data.Text.Lazy as LT
import Text.Show (Show(..)) -- why?
import Control.Lens
-- TODO: just reexport Network.HTTP.Types.Status
import Network.HTTP.Types.Status ( Status(..)
                                 , status200)
import Network.Wai (Request(..))
import Network.Wai.Handler.Warp (Port)

newtype RequestMatcher = RequestMatcher { _matcher :: Request -> Bool }

makeClassy ''RequestMatcher

instance Show RequestMatcher where
  show = const "RequestMatcher"

-- TODO: include status and junk
data FakeResponse = FakeResponse { _responseStatus  :: Status
                                 , _responseBody    :: LT.Text
                                 , _responseHeaders :: [(LT.Text, LT.Text)] } deriving (Show, Eq)

makeClassy ''FakeResponse

instance Default FakeResponse where
  def = FakeResponse status200 empty empty

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
