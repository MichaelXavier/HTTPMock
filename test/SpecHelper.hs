{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports #-}
module SpecHelper ( module X
                  , TestMessage(..)
                  , withDummyApp
                  , dummyPort
                  ) where


import ClassyPrelude             as X
import Control.Rematch           as X (is)
import Network.HTTPMock          as X
import Network.HTTPMock.Matchers as X
import Test.Rematch.HUnit        as X (expect)
import Test.Hspec                as X
import Test.QuickCheck           as X

import Control.Concurrent ( forkIO
                          , threadDelay
                          , killThread
                          )
import Data.Aeson.TH
import Network.Socket ( withSocketsDo
                      , socket
                      , Socket
                      , SockAddr
                      , getAddrInfo
                      , addrFamily
                      , addrAddress
                      , SocketType(Stream)
                      , defaultProtocol
                      , connect
                      )
import System.Timeout (timeout)
import Web.Scotty

newtype TestMessage  = TestMessage { testMessage :: Text } deriving (Show, Eq)

$(deriveJSON defaultOptions ''TestMessage) 

dummyPort :: Port
dummyPort = 7654

withDummyApp :: IO a -> IO a
withDummyApp = bracket startDummy killThread . const
  where startDummy = do tid <- forkIO $ scotty dummyPort' dummyApp
                        waitForServer dummyPort
                        return tid
        dummyPort' = fromInteger . toInteger $ dummyPort 

waitForServer :: Port -> IO ()
waitForServer p = fromMaybe (error "could not connect") <$> timeout 2000000 waitOnPort
  where waitOnPort = do (ai:_) <- getAddrInfo Nothing (Just "0.0.0.0") (Just . show $ p)
                        sock   <- socket (addrFamily ai) Stream defaultProtocol
                        signal <- newEmptyMVar
                        _ <- forkIO $ wait signal sock $ addrAddress ai
                        takeMVar signal

wait :: MVar () -> Socket -> SockAddr -> IO ()
wait signal sock addr = connectAndSignal `catchRefusal` sleepAndRetry
  where sleep            = threadDelay delay
        delay            = 500
        connectAndSignal = connect sock addr >> putMVar signal ()
        sleepAndRetry    = sleep >> wait signal sock addr

catchRefusal :: IO a -> IO a -> IO a
catchRefusal action = catchIOError action . const

dummyApp :: ScottyM ()
dummyApp = do
  get "/hello"    $ text "hey"
  post "/message" $ text . pack . show . length =<< body 
  post "/json"    $ json =<< (jsonData :: ActionM TestMessage)
