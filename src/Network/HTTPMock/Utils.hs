{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMock.Utils ( pathInfoToText
                              , requestPath) where

import ClassyPrelude

import Network.HTTPMock.Types (Request(..)
                              , RequestHeaders(..)
                              , Header(..))

pathInfoToText :: [Text] -> Text
pathInfoToText [] = "/"
pathInfoToText pieces = intercalate "/" (empty:pieces)

requestPath :: Request -> Text
requestPath = pathInfoToText . _pathInfo
