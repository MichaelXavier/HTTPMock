module Network.HTTPMock.RequestMatchers ( matchPath
                                        , matchMethod
                                        , matchPathAndMethod) where

import Prelude (and)
import ClassyPrelude
import Network.HTTPMock.Types
import Network.HTTP.Types (Method)
import Network.HTTPMock.Utils

-- Build request matchers
matchPath :: Text -> RequestMatcher
matchPath path = RequestMatcher $ matchPath' path

matchMethod :: Method -> RequestMatcher
matchMethod meth = RequestMatcher $ matchMethod' meth

matchPathAndMethod :: Text -> Method -> RequestMatcher
matchPathAndMethod path meth = RequestMatcher checkAll
  where checkAll req = and $ map ($req) [matchPath' path, matchMethod' meth]

matchPath' :: Text -> Request -> Bool
matchPath' path = (== path) . requestPath

matchMethod' :: Method -> Request -> Bool
matchMethod' meth = (== meth) . _requestMethod
