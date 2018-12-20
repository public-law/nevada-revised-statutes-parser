{-# LANGUAGE ExtendedDefaultRules #-}


module Models.SectionSpec where

import           BasicPrelude
import           Test.Hspec

import           Models.Section                 ( parseName )


spec :: SpecWith ()
spec = parallel $ describe "parseName" $ do

  it "runs tests in a sub-module" $ do
    let a = 1 :: Integer
    let b = 2
    a + b `shouldBe` 3

  it "returns the name when it's simple" $ do
    let simpleName = "Definitions."
    parseName simpleName `shouldBe` simpleName

  it "returns the name when there's a bracket notation" $ do
    let annotatedName = "Definitions. [Effective until 1/1/2019.]"
    parseName annotatedName `shouldBe` "Definitions."


--
-- Helper Functions
--
main :: IO ()
main = hspec spec
