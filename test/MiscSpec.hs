{-# LANGUAGE OverloadedStrings #-}

module MiscSpec where

import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import           Models
import           NvStatutes
import           Test.Hspec


nrsIndexHtml :: IO String
nrsIndexHtml = readFile "nrs.html"


main :: IO()
main =
  -- html <- readFile "nrs.html"
  hspec spec


spec = do
  describe "titleCount" $
    it "finds the correct number of titles" $ do
      html <- nrsIndexHtml
      titleCount (convertString html) `shouldBe` 59

  describe "titles" $ do
    it "gets the first title's name" $ do
      html <- nrsIndexHtml
      let firstTitle = head (titles (convertString html))
      titleName firstTitle `shouldBe` ("STATE JUDICIAL DEPARTMENT"::Text)

    it "gets the first title's number" $ do
      html <- nrsIndexHtml
      let firstTitle = head (titles (convertString html))
      titleNumber firstTitle `shouldBe` 1
