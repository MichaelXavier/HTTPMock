{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMock.WebServers.Scotty (startServer) where

import ClassyPrelude
import Control.Lens
import Data.Default
import Data.Conduit ( runResourceT
                    , ($$))
import Data.Conduit.List (consume)
import Data.Maybe (maybe)
import Network.HTTP.Types ( Method(..)
                          , notFound404
                          , ok200)
import qualified Web.Scotty as S
import Web.Scotty ( Options(..)
                  , ActionM
                  , ScottyM
                  , text
                  , header
                  , status
                  , notFound
                  , scottyOpts)
import qualified Network.Wai as W
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
    req <- liftIO . convertToRequest =<< S.request
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
                         mapM_ setHeader $ resp ^. responseHeaders
                         text            $ resp ^. responseBody
  where setHeader = uncurry header

convertToRequest :: W.Request -> IO Request
convertToRequest r = do body <- consumeBody r
                        return $ Request (W.requestMethod r)
                                         (decodeUtf8 . W.serverName $ r)
                                         (W.serverPort r)
                                         (W.requestHeaders r)
                                         (W.isSecure r)
                                         (W.pathInfo r)
                                         (W.queryString r)
                                         body
                                    
--TODO: cleanup
consumeBody :: W.Request -> IO ByteString
consumeBody r = do chunks <- runResourceT $ W.requestBody r $$ consume
                   return $ concat chunks
