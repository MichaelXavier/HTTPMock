{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMock.WebServers.Scotty (startServer) where

import ClassyPrelude
import Control.Lens
import Data.Default
import Data.Maybe (maybe)
import Network.HTTP.Types ( Method(..)
                          , notFound404
                          , ok200)
import Web.Scotty ( Options(..)
                  , ActionM
                  , ScottyM
                  , request
                  , text
                  , header
                  , status
                  , notFound
                  , scottyOpts)
import Network.Wai.Handler.Warp ( Settings(settingsPort)
                                , defaultSettings)

import Network.HTTPMock.Interactions (getResponse)
import Network.HTTPMock.Types

startServer :: IORef HTTPMocker -> IO ()
startServer mockerR = do
  mocker <- readIORef mockerR
  let port = mocker ^. options . mockerPort
  scottyOpts (makeOpts port) $ scottyRoutes mockerR

makeOpts :: Port -> Options
makeOpts port = def { verbose = 0
                    , settings = defaultSettings { settingsPort = port } }

scottyRoutes :: IORef HTTPMocker -> ScottyM ()
scottyRoutes mockerR = do
  notFound $ do
    mocker <- liftIO $ readIORef mockerR 
    liftIO $ putStrLn "catchall match"
    req    <- request
    handleMatch mockerR req

-- use of mockerR doesn't make sense now, but state will mutate in getResponse eventually
handleMatch mockerR req = do
  mocker <- liftIO $ readIORef mockerR
  let (mResponse, mocker') = getResponse req mocker
  liftIO $ writeIORef mockerR mocker'
  maybe respondNotFound respondSuccess mResponse

respondNotFound :: ActionM ()
respondNotFound = status notFound404

respondSuccess :: FakeResponse -> ActionM ()
respondSuccess resp = do status          $ resp ^. responseStatus
                         mapM_ setStatus $ resp ^. responseHeaders
                         text            $ resp ^. responseBody
  where setStatus = uncurry header
