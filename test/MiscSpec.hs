{-# LANGUAGE OverloadedStrings #-}

module MiscSpec where

import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import           Models
import           NvStatutes
import           Test.Hspec


--
-- Helper Functions
--
nrsIndexHtml :: IO String
nrsIndexHtml = readFile "nrs.html"

firstTitle :: IO Title
firstTitle = do
  html <- nrsIndexHtml
  return (head (titles (convertString html)))


main :: IO()
main =
  hspec spec


spec :: Spec
spec = parallel $ do

  describe "titleCount" $
    it "finds the correct number of titles" $ do
      html <- nrsIndexHtml
      titleCount (convertString html) `shouldBe` 59

  describe "titles" $ do
    it "gets the first title's name" $ do
      judicialDept <- firstTitle
      titleName judicialDept `shouldBe` ("STATE JUDICIAL DEPARTMENT"::Text)

    it "gets the first title's number" $ do
      judicialDept <- firstTitle
      titleNumber judicialDept `shouldBe` 1
