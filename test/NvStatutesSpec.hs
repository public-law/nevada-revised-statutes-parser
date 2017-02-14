{-# LANGUAGE OverloadedStrings #-}

module NvStatutesSpec where

import           BasicPrelude
import           Models
import           NvStatutes   (titles, nrsIndexHtml)
import           Test.Hspec


spec :: Spec
spec = parallel $

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


--
-- Helper Functions
--
firstTitle :: IO Title
firstTitle = do
  html <- nrsIndexHtml
  return (head (titles html))

main :: IO()
main =
  hspec spec
