{-# LANGUAGE OverloadedStrings #-}

module MiscSpec where

import           Data.String.Conversions
import           Data.Text               (Text, strip)
import           NvStatutes
import           Test.Hspec


main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "titleCount" $
    it "finds the correct number of titles" $ do
      html <- readFile "nrs.html"
      titleCount (cs html) `shouldBe` 59

  describe "titles" $ do
    it "gets the first title's name" $ do
      html <- readFile "nrs.html"
      titleName (head (titles (cs html))) `shouldBe` ("STATE JUDICIAL DEPARTMENT"::Text)

    it "gets the first title's number" $ do
      pending
      html <- readFile "nrs.html"
      titleNumber (head (titles (cs html))) `shouldBe` 1
