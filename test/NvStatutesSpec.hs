{-# LANGUAGE OverloadedStrings #-}

module NvStatutesSpec where

import           BasicPrelude
import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import           Models
import           NvStatutes              (titles)
import           Test.Hspec


--
-- Helper Functions
--
nrsIndexHtml :: IO Text
nrsIndexHtml = readFile "nrs.html"

firstTitle :: IO Title
firstTitle = do
  html <- nrsIndexHtml
  return (head (titles (convertString html)))


main :: IO()
main =
  hspec spec


spec :: Spec
spec = parallel $

  describe "titles" $ do
    it "finds the correct number of titles" $ do
      html <- nrsIndexHtml
      length (titles (convertString html)) `shouldBe` 59

    it "gets the first title's name" $ do
      judicialDept <- firstTitle
      titleName judicialDept `shouldBe` ("STATE JUDICIAL DEPARTMENT"::Text)

    it "gets the first title's number" $ do
      judicialDept <- firstTitle
      titleNumber judicialDept `shouldBe` 1
