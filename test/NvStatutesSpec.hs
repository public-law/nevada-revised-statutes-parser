{-# LANGUAGE OverloadedStrings #-}

module NvStatutesSpec where

import           BasicPrelude
import           Models
import           NvStatutes   (titles)
import           Test.Hspec


--
-- Helper Functions
--
nrsIndexHtml :: IO Text
nrsIndexHtml = readFile "nrs.html"

firstTitle :: IO Title
firstTitle = do
  html <- nrsIndexHtml
  return (head (titles html))


main :: IO()
main =
  hspec spec


spec :: Spec
spec = parallel $ do

  describe "titles" $ do
    it "finds the correct number of titles" $ do
      html <- nrsIndexHtml
      length (titles html) `shouldBe` 59

    it "gets the first title's name" $ do
      judicialDept <- firstTitle
      titleName judicialDept `shouldBe` "STATE JUDICIAL DEPARTMENT"

    it "gets the first title's number" $ do
      judicialDept <- firstTitle
      titleNumber judicialDept `shouldBe` 1

    it "reads a chapter correctly" $ do
      judicialDept <- firstTitle
      length (chapters judicialDept) `shouldNotBe` 0

      let firstChapter = head (chapters judicialDept)
      chapterName firstChapter `shouldBe` "Judicial Department Generally"
