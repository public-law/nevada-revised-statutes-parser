{-# LANGUAGE OverloadedStrings #-}

module NvStatutesSpec where

import           BasicPrelude
import           Test.Hspec

import           Models
import           NvStatutes   (titles, parseChapter)
import           FileUtil     (readFileAsUtf8)


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


  describe "parseChapter" $ do
  
    it "gets the chapter name" $ do
      html ← chapter_432b_html
      chapterName (parseChapter html) `shouldBe` "Protection of Children from Abuse and Neglect"


    it "gets the chapter number" $ do
      html ← chapter_432b_html
      chapterNumber (parseChapter html) `shouldBe` "432B"


    it "gets the chapter URL" $ do
      html ← chapter_432b_html
      chapterUrl (parseChapter html) `shouldBe` "https://www.leg.state.nv.us/nrs/NRS-432B.html"


    it "gets the sub-chapters" $ do
      pendingWith "Functional test"
      html ← chapter_432b_html
      let chapter_432b = parseChapter html
      let administration = (subChapters chapter_432b) !! 1

      subChapterName administration `shouldBe` "Administration"



  --
  --
  -- describe "sections" $
  --
  --   it "reads a section correctly" $ do
  --     pendingWith "TODO"
  --     judicialDept <- firstTitle
  --     let judicialDeptGenerally = head $ chapters judicialDept
  --     let courtsOfJustice       = head $ sections judicialDeptGenerally
  --
  --     sectionName courtsOfJustice `shouldBe` "Courts of justice"


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


chapter_432b_html :: IO Text
chapter_432b_html = 
  readFileAsUtf8 (fixture "nrs-432b.html") "LATIN1"


fixture :: String -> String
fixture filename =
  "test/fixtures/" ++ filename
