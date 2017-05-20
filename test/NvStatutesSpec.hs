{-# LANGUAGE OverloadedStrings #-}

module NvStatutesSpec where

import           BasicPrelude
import           Test.Hspec

import           Models
import           NvStatutes   (titles)
import           ChapterFile  (parseChapter)
import           FileUtil     (fixture, readFileAsUtf8)


spec :: SpecWith ()
spec = parallel $ do

  describe "titles" $ do

    it "finds the correct number of titles" $ do
      html <- nrsIndexHtml
      length (titles html) `shouldBe` 59


    it "gets a title's name" $ do
      judicialDept <- firstTitle
      titleName judicialDept `shouldBe` "State Judicial Department"


    it "gets a title's number" $ do
      judicialDept <- firstTitle
      titleNumber judicialDept `shouldBe` 1


    it "reads a chapter correctly" $ do
      judicialDept <- firstTitle
      length (chapters judicialDept) `shouldNotBe` 0
      let chapter1 = head $ chapters judicialDept

      chapterName   chapter1 `shouldBe` "Judicial Department Generally"
      chapterNumber chapter1 `shouldBe` "1"
      chapterUrl    chapter1 `shouldBe` "https://www.leg.state.nv.us/nrs/NRS-001.html"


    it "gets one that is further in" $ do
      publicWelfare <- title38
      titleName publicWelfare `shouldBe` "Public Welfare"

      let chapter432b = last $ chapters publicWelfare
      chapterName chapter432b `shouldBe` "Protection of Children From Abuse and Neglect"


--
-- Helper Functions
--
main :: IO()
main =
  hspec spec


firstTitle :: IO Title
firstTitle = do
  html ← nrsIndexHtml
  return (head (titles html))


title38 :: IO Title
title38 = do
  html ← nrsIndexHtml
  return $ titles html !! 37


nrsIndexHtml :: IO Text
nrsIndexHtml =
  readFile (fixture "nrs.html")

