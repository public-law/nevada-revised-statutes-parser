{-# LANGUAGE OverloadedStrings #-}

module ChapterFileSpec where

import           BasicPrelude
import           Test.Hspec

import           Models
import           ChapterFile  (parseChapter)
import           FileUtil     (fixture, readFileAsUtf8)


spec :: SpecWith ()
spec = parallel $ do

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

    
    it "gets the sub-chapter names" $ do
      html ← chapter_432b_html
      let subchapters = subChapters ( parseChapter html )

      subChapterName (subchapters !! 0) `shouldBe` "General Provisions"
      subChapterName (subchapters !! 1) `shouldBe` "Administration"
      subChapterName (subchapters !! 3) `shouldBe` "Protective Services and Custody"


    it "gets a simple sub-chapter's sections" $ do
      html <- chapter_432b_html
      let generalProvisions = head $ subChapters ( parseChapter html )
      case subChapterChildren generalProvisions of
        Sections xs      -> length xs `shouldBe` 31
        SubSubChapters _ -> error "Got sub-sub chapters but expected Sections"


    it "gets the sub-chapter's section names right - 1" $ do
      html <- chapter_432b_html
      let generalProvisions = head $ subChapters ( parseChapter html )
      case subChapterChildren generalProvisions of
        Sections xs      -> sectionName (xs !! 0) `shouldBe` "Definitions."
        SubSubChapters _ -> error "Got sub-sub chapters but expected Sections"


    it "gets the sub-chapter's section names right - 2" $ do
      html <- chapter_432b_html
      let generalProvisions = head $ subChapters ( parseChapter html )
      case subChapterChildren generalProvisions of
        Sections xs      -> sectionName (xs !! 1) `shouldBe` "“Abuse or neglect of a child” defined."
        SubSubChapters _ -> error "Got sub-sub chapters but expected Sections"


    it "gets a complex sub-chapter's sub-sub-chapters" $ do
      html <- chapter_432b_html
      let administration = (!!1) $ subChapters $ parseChapter html
      case subChapterChildren administration of
        SubSubChapters xs -> length xs `shouldBe` 31
        Sections _        -> error "Got sections but expected sub-sub-chapters"



--
-- Helper Functions
--
main :: IO()
main =
  hspec spec


chapter_432b_html :: IO Text
chapter_432b_html = 
  readFileAsUtf8 (fixture "nrs-432b.html") "LATIN1"
