{-# LANGUAGE NoImplicitPrelude    #-}
module Network.HTTPMock ( module Network.HTTPMock.Types
                        , withMocker
                        , resetRecorder) where

import ClassyPrelude
import Control.Lens

import Network.HTTPMock.Types
import Network.HTTPMock.WebServers.Common
import qualified Network.HTTPMock.WebServers.Scotty as S

withMocker :: HTTPMocker -> (IORef HTTPMocker -> IO ()) -> IO ()
withMocker mocker action = do mockerR <- newIORef mocker
                              tid <- startServer mockerR S.startServer
                              action mockerR `finally` killServer tid

resetRecorder :: HTTPMocker -> HTTPMocker
resetRecorder mocker = mocker & recordedRequests .~ empty
