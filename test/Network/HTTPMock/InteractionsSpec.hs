{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTPMock.InteractionsSpec (spec) where

import ClassyPrelude
import Control.Lens
import Data.Default
import Test.Hspec

import Network.HTTPMock.Types
import Network.HTTPMock.Interactions

spec :: Spec
spec = do
  describe "getResponse" $ do
    describe "no matches" $ do
      it "returns Nothing" $
        getResponse req def ^. _1 `shouldBe` Nothing
      it "returns an unmodified Mocker" $
        getResponse req def ^. _2 `shouldBe` def

  where req = undefined
