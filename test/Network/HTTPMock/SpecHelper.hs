{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMock.SpecHelper ( okFakeResponse
                                   , matchPathMocker
                                   , sequenceMocker
                                   , matchMethodMocker
                                   , getBody
                                   , deleteReturningBody
                                   , postReturningBody
                                   , putReturningBody
                                   , get_
                                   , shouldHaveSameRecordedRequests
                                   , postResponse
                                   , firstResponse
                                   , secondResponse
                                   , yep
                                   , returnsSequence
                                   , responseAfterTimes
                                   , mockerAfterTimes
                                   , req
                                   , reqMatcher
                                   , module Network.HTTPMock.Expectations) where

import Prelude ( last
               , tail
               , read)
import ClassyPrelude
import Control.Lens
import Control.Monad (replicateM_)
import Control.Monad.Trans.State ( execState
                                 , evalState
                                 , state)
import Data.Default
import qualified Data.Text.Lazy as LT
import Network.URI ( parseURI
                   , URIAuth(..)
                   , URI(..))
import Network.HTTP.Types ( http11)
import Network.Http.Client ( get
                           , Method(..)
                           , emptyBody
                           , inputStreamBody
                           , openConnection
                           , withConnection
                           , http
                           , setAccept
                           , sendRequest
                           , buildRequest
                           , receiveResponse
                           , debugHandler
                           , concatHandler')
import System.IO.Streams (fromLazyByteString)

import Network.HTTPMock
import Network.HTTPMock.Interactions (getResponse)
import Network.HTTPMock.Expectations
import Network.HTTPMock.Utils

import Test.Hspec

okFakeResponse :: LT.Text -> FakeResponse
okFakeResponse body = def & responseBody .~ body

matchPathMocker :: HTTPMocker
matchPathMocker = def & responder . fakedInteractions <>~ singleton (fooMatcher, AlwaysReturns $ okFakeResponse "fake-response")
  where fooMatcher = matchPath "/foo/bar"

sequenceMocker :: HTTPMocker
sequenceMocker = def & responder . fakedInteractions <>~ singleton (fooMatcher, ReturnsSequence $ firstResponse !: [secondResponse])
  where fooMatcher = matchPath "/foo/bar"

matchMethodMocker :: ByteString -> HTTPMocker
matchMethodMocker meth = def & responder . fakedInteractions <>~ singleton (methMatcher, AlwaysReturns postResponse)
  where methMatcher = matchMethod meth

getBody url' = get url concatHandler'
  where url = encodeUtf8 . pack $ url'

postReturningBody = requestReturningBody POST
putReturningBody = requestReturningBody PUT
deleteReturningBody = requestReturningBody DELETE

--establish :: URI -> IO (Connection)
establish u = openConnection host port
  where
    auth = case uriAuthority u of
        Just x  -> x
        Nothing -> URIAuth "" "localhost" ""

    host = encodeUtf8 . pack . uriRegName $ auth
    port = case uriPort auth of
        ""  -> 80
        _   -> read $ tail $ uriPort auth-- :: Word16

path :: URI -> ByteString
path u = case url of
            ""  -> "/"
            _   -> url
  where
    url = encodeUtf8 $! pack
                     $! concat [uriPath u, uriQuery u, uriFragment u]

requestReturningBody meth url = do
  let uri = fromMaybe (error "uri parse error") $ parseURI url
  stream <- fromLazyByteString "POSTBODY"

  withConnection (establish uri) $ \c -> do
    q <- buildRequest $ do
        http meth (path uri)
        setAccept "application/json"

    sendRequest c q $ inputStreamBody stream

    receiveResponse c concatHandler'

get_ url' = get url discard
  where discard _ _ = return ()
        url = encodeUtf8 . pack $ url'

shouldHaveSameRecordedRequests :: HTTPMocker -> HTTPMocker -> Expectation
shouldHaveSameRecordedRequests a b = aReqs `shouldBe` bReqs
  where aReqs = summarizeRequests a
        bReqs = summarizeRequests b

postResponse :: FakeResponse
postResponse = okFakeResponse "you sent a POST"

firstResponse :: FakeResponse
firstResponse  = okFakeResponse "first-response"

secondResponse :: FakeResponse
secondResponse = okFakeResponse "second-response"

yep :: FakeResponse
yep = okFakeResponse "yep"

reqMatcher :: RequestMatcher
reqMatcher = RequestMatcher (\reqToCheck -> ["foo"] == reqToCheck ^. pathInfo)

req :: Request
req = Request {
  _requestMethod     = "GET"
, _serverName        = "example.com"
, _serverPort        = 4568
, _requestHeaders    = []
, _pathInfo          = ["foo"]
, _queryString       = [("wat", Just "true")]
, _requestBody       = "FAKEBODY"
}

returnsSequence :: CannedResponse
returnsSequence = ReturnsSequence $ firstResponse !: [secondResponse]

responseAfterTimes :: Int -> Request -> HTTPMocker -> Maybe FakeResponse
responseAfterTimes times req = last . runStatefully (getResponse req)
  where runStatefully = evalState . replicateM times . state

mockerAfterTimes :: Int -> Request -> HTTPMocker -> HTTPMocker
mockerAfterTimes times req = runStatefully (getResponse req)
  where runStatefully = execState . replicateM_ times . state
