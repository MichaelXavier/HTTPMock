{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Network.HTTPMock.WebServers.Snap (startServer) where

import ClassyPrelude
import Control.Lens
import Data.CaseInsensitive (mk)
import Data.Text (splitOn)
import Snap hiding ( Request
                   , getResponse
                   , mapM_)
import qualified Snap as S
import Network.HTTP.Types.URI (Query)
--import Snap.Http.Server

import Network.HTTPMock.Interactions (getResponse)
import Network.HTTPMock.Types

data App = App {
  _mockerR :: IORef HTTPMocker
}


makeLenses ''App

handler :: Handler App App ()
handler = do mr  <- gets _mockerR
             req <- getRequest
             handleMatch mr =<< convertToRequest req

appInit :: IORef HTTPMocker -> SnapletInit App App
appInit mr = makeSnaplet "mocker" "HTTPMocker context" Nothing $ do
  addRoutes [("", handler)]
  return $ App mr

handleMatch :: IORef HTTPMocker -> Request -> Handler App App ()
handleMatch mr req = do
  mocker <- liftIO $ readIORef mr
  liftIO $ print req
  let (mResponse, mocker') = getResponse req mocker
  liftIO $ print mResponse
  liftIO $ writeIORef mr mocker'
  maybe respondNotFound respondSuccess mResponse

respondNotFound :: Handler App App ()
respondNotFound = modifyResponse $ setResponseCode 404

respondSuccess :: FakeResponse -> Handler App App ()
respondSuccess stubResp = modifyResponse setStatus >> setHeaders >> setBody
  where setStatus     = setResponseCode . statusCode $ stubResp ^. responseStatus
        setBody       = writeText $ toStrict $ stubResp ^. responseBody
        setHeaders    = mapM_ setHeaderPair $ stubResp ^. responseHeaders
        setHeaderPair (k,v) = modifyResponse $ setHeader (mk . toBS $ k) (toBS v) -- think i can use "both" here
        toBS    = encodeUtf8 . toStrict

convertToRequest :: MonadSnap m => S.Request -> m Request
convertToRequest r = do body <- readRequestBody maxSize
                        return $ Request (encodeUtf8 . show . rqMethod $ r)
                                         (decodeUtf8 . rqServerName $ r)
                                         (rqServerPort r)
                                         (listHeaders r)
                                         (rqIsSecure r)
                                         (parsePath . rqPathInfo $ r)
                                         (convertParams . rqParams $ r)
                                         (toStrict body)
  where maxSize = 100000000
        parsePath = splitOn "/" . decodeUtf8
        convertParams = concatMap convertParam . toList
        convertParam :: (ByteString, [ByteString]) -> Query
        convertParam (_, []) = []
        convertParam (k, vs) = map ((k,) . Just) vs

startServer :: IORef HTTPMocker -> IO ()
startServer mr = do
  mocker <- readIORef mr
  let port = mocker ^. options . mockerPort
  serveSnaplet (configure port defaultConfig) $ appInit mr
  where configure port = setAccessLog ConfigNoLog . setErrorLog ConfigNoLog . setPort port . setVerbose False
      
