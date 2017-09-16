{-# LANGUAGE OverloadedStrings #-}

module TextUtilSpec where

import           BasicPrelude
import           Test.Hspec

import           TextUtil


spec :: SpecWith ()
spec = parallel $ do

  describe "titleize" $ do

    it "handles all uppercase" $ do
      titleize "PROTECTION OF CHILDREN" `shouldBe` "Protection of Children"

    it "handles all lowercase" $ do
        titleize "protection of children" `shouldBe` "Protection of Children"
  
    it "handles Unicode double quotes" $ do
      titleize "“ABUSE OR NEGLECT” DEFINED." `shouldBe` "“Abuse or Neglect” Defined."