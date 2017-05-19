{-# LANGUAGE OverloadedStrings #-}

module TextUtilSpec where

import           BasicPrelude
import           Test.Hspec

import           TextUtils


spec :: SpecWith ()
spec = parallel $ do

  describe "titleize" $

    it "handles all uppercase" $ do
      TextUtils.titleize "PROTECTION OF CHILDREN" `shouldBe` "Protection of Children"
