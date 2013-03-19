{-# LANGUAGE NoImplicitPrelude    #-}
module Network.HTTPMock ( module Network.HTTPMock.Types
                        , withMocker
                        , matchPath
                        , matchMethod
                        , matchPathAndMethod
                        , resetRecorder) where

import Prelude (and)
import ClassyPrelude
import Control.Lens
import Network.HTTP.Types (Method)

import Network.HTTPMock.Types
import Network.HTTPMock.WebServers.Common
import qualified Network.HTTPMock.WebServers.Scotty as S
import Network.HTTPMock.Utils

withMocker :: HTTPMocker -> (IORef HTTPMocker -> IO ()) -> IO ()
withMocker mocker action = do mockerR <- newIORef mocker
                              tid <- startServer mockerR S.startServer
                              action mockerR `finally` killServer tid

resetRecorder :: HTTPMocker -> HTTPMocker
resetRecorder mocker = mocker & recordedRequests .~ empty

-- Build request matchers
matchPath :: Text -> RequestMatcher
matchPath path = RequestMatcher $ matchPath' path

matchMethod :: Method -> RequestMatcher
matchMethod meth = RequestMatcher $ matchMethod' meth

matchPathAndMethod :: Text -> Method -> RequestMatcher
matchPathAndMethod path meth = RequestMatcher checkAll
  where checkAll req = and $ map ($req) [matchPath' path, matchMethod' meth]

matchPath' :: Text -> Request -> Bool
matchPath' path = (== path) . requestPath

matchMethod' :: Method -> Request -> Bool
matchMethod' meth = (== meth) . requestMethod
