{-# LANGUAGE NoImplicitPrelude #-}
module Network.HTTPMock.Matchers ( withBody
                                 , withMethod
                                 , withHost
                                 , withPath
                                 , withHeader
                                 , withHeaders
                                 , withSignature
                                 , hasRequest
                                 , hasRequest'
                                 , andAlso
                                 ) where

import ClassyPrelude hiding ( on )
import Control.Applicative ( liftA2 )
import Control.Lens hiding ( allOf )
import Control.Rematch ( Matcher(..)
                       , equalTo
                       , allOf
                       , on
                       )
import Network.Http.Types (lookupHeader)
import Network.HTTPMock.Types

withBody :: ByteString -> Matcher RecordedRequest
withBody b = on (equalTo b) (view rBody, "request body")

withMethod :: Method -> Matcher RecordedRequest 
withMethod m = on (equalTo m) (view requestMethod, "request method")
  where requestMethod = rRequest . method

withHost :: Hostname -> Matcher RecordedRequest 
withHost h = on (equalTo h) (view requestHost, "request host")
  where requestHost = rRequest . host

withPath :: ByteString -> Matcher RecordedRequest
withPath p = on (equalTo p) (view requestPath, "request path")
  where requestPath = rRequest . path

withSignature :: Method -> Hostname -> ByteString -> Matcher RecordedRequest
withSignature m h p = withMethod m `andAlso` withHost h `andAlso` withPath p

withHeaders :: [(ByteString, ByteString)] -> Matcher RecordedRequest
withHeaders = allOf . map withHeader

withHeader :: (ByteString, ByteString) -> Matcher RecordedRequest
withHeader (k, v) = Matcher {
    match = (== Just v) . lookupHeader' k . view headers
  , description = "Have the header " ++ show k ++ " with value " ++ show v 
  , describeMismatch = \r -> case lookupHeader' k (r ^. headers) of
                               Just v' -> "but got " ++ show v' ++ " instead"
                               Nothing -> "but the header didn't exist"
  }
  where headers       = rRequest . requestHeaders
        lookupHeader' = flip lookupHeader

hasRequest :: Matcher RecordedRequest -> Matcher RequestHistory
hasRequest m = Matcher {
    match = any (match m)
  , description = "include at least 1 request to " ++ description m
  , describeMismatch = \h -> "but only recorded " ++ show h
  }

hasRequest' :: Int -> Matcher RecordedRequest -> Matcher RequestHistory
hasRequest' count m = Matcher {
    match = (==count) . countMatches
  , description = "include exactly " ++ show count ++ " requests to " ++ description m
  , describeMismatch = \h -> "but found " ++ show (countMatches h) ++ " matches in " ++ show h
  }
  where countMatches = length . filter (match m)

andAlso :: Matcher a -> Matcher a -> Matcher a
andAlso m m' = Matcher {
    match = match1 <&&> match2
  , description = description m ++ " and " ++ description m'
  , describeMismatch = combineMismatch
  }
  where (<&&>) = liftA2 (&&)
        describeMismatch1 = describeMismatch m
        describeMismatch2 = describeMismatch m'
        match1            = match m
        match2            = match m'
        combineMismatch x
          | match1 x && match2 x = describeMismatch1 x ++ " and " ++ describeMismatch2 x
          | match1 x             = describeMismatch1 x
          | match2 x             = describeMismatch2 x
          | otherwise            = "You've found a bug in rematch!"
