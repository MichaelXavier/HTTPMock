{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Network.HTTPMock ( makeRequest
                        , fakeRequest
                        , evalFakeRequest
                        , execFakeRequest
                        , runFakeRequest
                        , module Network.HTTPMock.Types
                        -- , debugHandler
                        , concatHandler
                        , jsonHandler
                        ) where

import Blaze.ByteString.Builder ( Builder
                                , toByteString
                                )
import qualified Blaze.ByteString.Builder as B
import ClassyPrelude
import Control.Lens
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.Writer.Class (MonadWriter(tell))
import Control.Monad.RWS (evalRWST)
import Data.Aeson ( FromJSON
                  , fromJSON
                  , json'
                  , Result(..)
                  )
import Network.HTTPMock.Types
import Network.Http.Client ( buildRequest
                           , sendRequest
                           , receiveResponse
                           , getStatusCode
                           , getStatusMessage
                           , withConnection
                           , openConnection
                           , http
                           , setHeader
                           , getHeaders
                           )
import Network.Http.Types ( retrieveHeaders )
import qualified Network.Http.Client as H
import System.IO.Streams ( OutputStream
                         , InputStream
                         , fromLazyByteString
                         )
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S

makeRequest :: MonadIO m
            => Request
            -> (OutputStream Builder -> IO ())
            -> (Response -> InputStream ByteString -> IO a)
            -> m a
makeRequest r reqBodyStream handler = do
  req <- liftIO $ buildRealRequest r

  liftIO $ withConnection' $ \conn -> liftIO $ do
    sendRequest conn req (liftIO . reqBodyStream)
    receiveResponse conn (handler . buildResponse)
  where buildResponse resp = Response (getStatusCode resp) -- can this be slimmed down with applicative?
                                      (getStatusMessage resp)
                                      (retrieveHeaders . getHeaders $ resp)
        withConnection' = withConnection (openConnection (r ^. host) (r ^. port))

fakeRequest :: (MonadIO m,
                Functor m,
                MonadReader ResponseFaker m,
                MonadWriter RequestHistory m)
            => Request
            -> (OutputStream Builder -> IO ())
            -> (Response -> InputStream ByteString -> IO a)
            -> m a
fakeRequest req bodySource handler = do
  recordRequest =<< slurpBody

  resp <- lookupResponse <$> ask
  respBodyStream <- liftIO . fromLazyByteString $ resp ^. frBody
  liftIO $ handler (resp ^. frResponse) respBodyStream
  where slurpBody = do (outStream, flush) <- liftIO S.listOutputStream
                       liftIO $ bodySource outStream
                       liftIO $ S.write Nothing outStream >>
                                toByteString . mconcat <$> flush
        recordRequest = tell . singleton . RecordedRequest req

evalFakeRequest :: (Functor m, MonadIO m)
               => ResponseFaker
               -> Request
               -> (OutputStream Builder -> IO ())
               -> (Response -> InputStream ByteString -> IO a)
               -> m (a, RequestHistory)
evalFakeRequest faker req bodySource handler = evalRWST runReq faker ()
  where runReq = fakeRequest req bodySource handler

execFakeRequest :: (Functor m, MonadIO m)
                => ResponseFaker
                -> Request
                -> (OutputStream Builder -> IO ())
                -> (Response -> InputStream ByteString -> IO a)
                -> m RequestHistory
execFakeRequest faker req bodySource handler = snd `fmap` evalFakeRequest faker req bodySource handler

runFakeRequest :: (Functor m, MonadIO m)
               => ResponseFaker
               -> Request
               -> (OutputStream Builder -> IO ())
               -> (Response -> InputStream ByteString -> IO a)
               -> m a
runFakeRequest faker req bodySource handler = fst `fmap` evalFakeRequest faker req bodySource handler

concatHandler :: MonadIO m => Response -> InputStream ByteString -> m ByteString
concatHandler _ i1 = liftIO $ do
    i2 <- S.map B.fromByteString i1
    x <- S.fold mappend mempty i2
    return $ B.toByteString x

jsonHandler :: (MonadIO m, FromJSON a) => Response -> InputStream ByteString -> m a
jsonHandler _ i = liftIO $ do
    v <- S.parseFromStream json' i
    let r = fromJSON v
    case r of
        (Success a) ->  return a
        (Error str) ->  error str

--TODO: contextual fakers
lookupResponse :: ResponseFaker -> FakeResponse
lookupResponse (AlwaysReturns resp) = resp

buildRealRequest :: Request -> IO H.Request
buildRealRequest r = buildRequest $ do
  http (r ^. method) (r ^. path)
  mapM_ (uncurry setHeader) (r ^. reqHeaders)

