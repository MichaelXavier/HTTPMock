{-# LANGUAGE NoImplicitPrelude #-}
module Network.HTTPMock.WebServers.Common (startServer,
                                           killServer) where

import Control.Concurrent ( forkIO
                          , ThreadId
                          , threadDelay
                          , killThread)
import Control.Lens
import ClassyPrelude
import Network.Socket ( withSocketsDo
                      , socket
                      , Socket
                      , SockAddr
                      , getAddrInfo
                      , addrFamily
                      , addrAddress
                      , SocketType(Stream)
                      , defaultProtocol
                      , connect)
import System.IO.Error (catchIOError)

import Network.HTTPMock.Types

startServer :: IORef HTTPMocker -> (IORef HTTPMocker -> IO ()) -> IO (ThreadId)
startServer mockerR start = do mocker <- readIORef mockerR
                               let port = mocker ^. options . mockerPort
                               tid <- forkIO $ start mockerR
                               waitForServer port
                               return tid

killServer :: ThreadId -> IO ()
killServer = killThread

waitForServer :: Port -> IO ()
waitForServer port = withSocketsDo $ do
  (ai:_) <- getAddrInfo Nothing (Just "0.0.0.0") (Just port')
  sock <- socket (addrFamily ai) Stream defaultProtocol
  signal <- newEmptyMVar
  forkIO $ wait signal sock $ addrAddress ai
  takeMVar signal
  where port' = show port

wait :: MVar () -> Socket -> SockAddr -> IO ()
wait signal sock addr = connectAndSignal `catchRefusal` sleepAndRetry
  where sleep            = threadDelay delay
        delay            = 500
        connectAndSignal = connect sock addr >> putMVar signal ()
        sleepAndRetry    = sleep >> wait signal sock addr


catchRefusal :: IO a -> IO a -> IO a
catchRefusal action fallback = catchIOError action handler
  where handler e = fallback
