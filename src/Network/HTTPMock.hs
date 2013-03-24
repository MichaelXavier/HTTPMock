{-# LANGUAGE NoImplicitPrelude    #-}
module Network.HTTPMock ( module Network.HTTPMock.Types
                        , runWithMocker
                        , runWithMocker_
                        , withMocker
                        , withMocker_
                        , resetRecorder
                        , module Network.HTTPMock.Matchers) where

import ClassyPrelude
import Control.Lens

import Network.HTTPMock.Matchers
import Network.HTTPMock.Types
import Network.HTTPMock.WebServers.Common
import qualified Network.HTTPMock.WebServers.Scotty as S

runWithMocker :: HTTPMocker -> (IORef HTTPMocker -> IO ()) -> IO HTTPMocker
runWithMocker mocker action = do mockerR <- newIORef mocker
                                 tid <- startServer mockerR S.startServer
                                 action mockerR `finally` killServer tid
                                 readIORef mockerR

runWithMocker_ :: HTTPMocker -> IO () -> IO HTTPMocker
runWithMocker_ mocker action = runWithMocker mocker $ const action

withMocker :: HTTPMocker -> (IORef HTTPMocker -> IO ()) -> IO ()
withMocker mocker = void . runWithMocker mocker

withMocker_ :: HTTPMocker -> IO () -> IO ()
withMocker_ mocker action = withMocker mocker $ const action

resetRecorder :: HTTPMocker -> HTTPMocker
resetRecorder mocker = mocker & recordedRequests .~ empty
