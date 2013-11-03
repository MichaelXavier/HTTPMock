{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.HTTPMock.Types ( Request(..)
                              , HasRequest(..)
                              , requestHeaders
                              , Response(..)
                              , HasResponse(..)
                              , responseHeaders
                              , FakeResponse(..)
                              , HasFakeResponse(..)
                              , ResponseFaker(..)
                              , RecordedRequest(..)
                              , HasRecordedRequest(..)
                              , RequestHistory
                              -- reexports
                              , Port
                              , Headers
                              , Method(..)
                              , Hostname
                              , StatusCode
                              , HTTPMockM
                              ) where

import ClassyPrelude
import Control.Lens
import Control.Monad.RWS (RWST)
import Network.Http.Types ( Port
                          , Method(..)
                          , Hostname
                          , StatusCode
                          , Headers
                          , buildHeaders
                          , retrieveHeaders
                          )

--TODO: convenience lens for accept, httpBasic, contentType, contentLength, expectContinue
data Request = Request { _method     :: Method
                       , _host       :: Hostname
                       , _port       :: Port
                       , _path       :: ByteString
                       , _reqHeaders :: [(ByteString, ByteString)]
                       } deriving (Show)

makeClassy ''Request

requestHeaders :: Lens' Request Headers
requestHeaders = lens (buildHeaders . _reqHeaders)
                      (\r hl -> r { _reqHeaders = retrieveHeaders hl})

data Response = Response { _status          :: StatusCode
                         , _statusMessage   :: ByteString
                         , _respHeaders :: [(ByteString, ByteString)] -- needs api change in http-streams to get the whole set
                         }

makeClassy ''Response

responseHeaders :: Lens' Response Headers
responseHeaders = lens (buildHeaders . _respHeaders)
                       (\r hl -> r { _respHeaders = retrieveHeaders hl})

data FakeResponse = FakeResponse { _frResponse :: Response
                                 , _frBody     :: LByteString -- is this the right choice?
                                 }

makeClassy ''FakeResponse

--TODO: more contructors
data ResponseFaker = AlwaysReturns FakeResponse

data RecordedRequest = RecordedRequest { _rRequest :: Request
                                       , _rBody    :: ByteString } deriving (Show)

makeClassy ''RecordedRequest

type RequestHistory = Seq RecordedRequest

type HTTPMockM m a = RWST ResponseFaker RequestHistory () m a
