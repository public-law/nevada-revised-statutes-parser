{-# LANGUAGE ExtendedDefaultRules #-}


module Models.SectionSpec where

import           BasicPrelude
import           Test.Hspec

import           Models.Section                as Section


spec :: SpecWith ()
spec = parallel $ do
  describe "parseName" $ do

    it "runs tests in a sub-module" $ do
      let a = 1 :: Integer
      let b = 2
      a + b `shouldBe` 3

    it "returns the name when it's simple" $ do
      let simpleName = "Definitions."
      Section.parseName simpleName `shouldBe` simpleName

    it "returns the name when there's a bracket notation" $ pending

    it "returns the bracket annotation when there is one" $ pending

    it "returns Nothing when there's no annotation" $ pending


--
-- Helper Functions
--
main :: IO ()
main = hspec spec
