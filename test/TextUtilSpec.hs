{-# LANGUAGE OverloadedStrings #-}

module TextUtilSpec where

import           BasicPrelude
import           Test.Hspec

import           TextUtil


spec :: SpecWith ()
spec = parallel $ do

  describe "titleize" $

    it "handles all uppercase" $ do
      titleize "PROTECTION OF CHILDREN" `shouldBe` "Protection of Children"
