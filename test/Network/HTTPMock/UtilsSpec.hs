{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMock.UtilsSpec (spec) where

import ClassyPrelude

import Network.HTTPMock.Utils

import Test.Hspec

spec :: Spec
spec = do
  describe "pathInfoToText" $ do
    it "returns / on empty path info" $
      pathInfoToText [] `shouldBe` "/"
    it "joins the paths and prepends a /" $
      pathInfoToText ["foo", "bar"] `shouldBe` "/foo/bar"
