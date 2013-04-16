{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE BangPatterns         #-}
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

--TODO: check order to be consistent with State
runWithMocker :: Show a => HTTPMocker -> (IORef HTTPMocker -> IO a) -> IO (HTTPMocker, a)
runWithMocker mocker action = do mockerR <- newIORef mocker
                                 tid <- startServer mockerR S.startServer
                                 res <- action mockerR `finally` killServer tid
                                 mocker' <- readIORef mockerR
                                 return (mocker', res)

runWithMocker_ :: Show a => HTTPMocker -> IO a -> IO (HTTPMocker, a)
runWithMocker_ mocker action = runWithMocker mocker $ const action

withMocker :: Show a => HTTPMocker -> (IORef HTTPMocker -> IO a) -> IO a
withMocker mocker action = snd <$> runWithMocker mocker action

withMocker_ :: Show a => HTTPMocker -> IO a -> IO a
withMocker_ mocker action = withMocker mocker $ const action
--withMocker_ mocker action = snd <$> runWithMocker_ mocker action

resetRecorder :: HTTPMocker -> HTTPMocker
resetRecorder mocker = mocker & recordedRequests .~ empty
