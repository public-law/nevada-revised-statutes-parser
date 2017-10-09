{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module ChapterFileSpec where

import           BasicPrelude
import qualified Data.Text as T
import           Test.Hspec
import           Text.HTML.TagSoup

-- What exactly is this?
-- import          Text.InterpolatedString.Perl6 (q)

import          Models.Chapter
import          Models.Section
import          Models.SubChapter
import          Models.SubSubChapter

import          ChapterFile
import          FileUtil


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
        SubChapterSections xs -> length xs `shouldBe` 31
        SubSubChapters _      -> error "Got sub-sub chapters but expected Sections"


    it "gets the sub-chapter's section names right - 1" $ do
      html <- chapter_432b_html
      let generalProvisions = head $ subChapters ( parseChapter html )
      case subChapterChildren generalProvisions of
        SubChapterSections xs -> sectionName (xs !! 0) `shouldBe` "Definitions."
        SubSubChapters _      -> error "Got sub-sub chapters but expected Sections"


    it "gets the sub-chapter's section names right - 2" $ do
      html <- chapter_432b_html
      let generalProvisions = head $ subChapters ( parseChapter html )
      case subChapterChildren generalProvisions of
        SubChapterSections xs -> sectionName (xs !! 1) `shouldBe` "“Abuse or neglect of a child” defined."
        SubSubChapters _      -> error "Got sub-sub chapters but expected Sections"

    it "gets the sub-chapter's section numbers right" $ do
      html <- chapter_432b_html
      let generalProvisions = head $ subChapters ( parseChapter html )
      case subChapterChildren generalProvisions of
        SubChapterSections xs -> sectionNumber (xs !! 1) `shouldBe` "432B.020"
        SubSubChapters _      -> error "Got sub-sub chapters but expected Sections"


    it "gets a complex sub-chapter's sub-sub-chapters" $ do
      html <- chapter_432b_html
      let administration = (!!1) $ subChapters $ parseChapter html
      case subChapterChildren administration of
        SubSubChapters xs    -> length xs `shouldBe` 3
        SubChapterSections _ -> error "Got sections but expected sub-sub-chapters"


    it "gets a complex sub-chapter's sub-sub-chapter names - 1" $ do
      html <- chapter_432b_html
      let administration = (!!1) $ subChapters $ parseChapter html
      case subChapterChildren administration of
        SubSubChapters xs    -> (subSubChapterName (xs !! 0)) `shouldBe` "General Provisions"
        SubChapterSections _ -> error "Got sections but expected sub-sub-chapters"


    it "gets a complex sub-chapter's sub-sub-chapter names - 2" $ do
      html <- chapter_432b_html
      let administration = (!!1) $ subChapters $ parseChapter html
      case subChapterChildren administration of
        SubSubChapters xs    -> (subSubChapterName (xs !! 2)) `shouldBe` "Grants to Agency Which Provides Child Welfare Services"
        SubChapterSections _ -> error "Got sections but expected sub-sub-chapters"


    it "gets a complex sub-chapter's sub-sub-chapter sections" $ do
      html <- chapter_432b_html
      let administration    = (!!1) $ subChapters $ parseChapter html
      case subChapterChildren administration of
        SubSubChapters xs    -> (sectionName $ (!! 0) $ subSubChapterSections $ (xs !! 0)) `shouldBe` "Duties of Division of Child and Family Services."
        SubChapterSections _ -> error "Got sections but expected sub-sub-chapters"



  describe "isSimpleSubChapter" $ do

    it "correctly identifies a simple sub-chapter" $ do
      html <- chapter_432b_html
      let generalProvisions = (!! 0) $ headingGroups $ parseTags html
      isSimpleSubChapter generalProvisions `shouldBe` True

    it "correctly identifies a complex sub-chapter" $ do
      html <- chapter_432b_html
      let administration = (!! 1) $ headingGroups $ parseTags html
      isSimpleSubChapter administration `shouldBe` False

  describe "sectionBody" $ do

    it "returns the complete HTML - 1" $ do
      html <- chapter_432b_html
      let dom = parseTags html
      let expectedHtml = T.pack "<p class=SectBody><span class=\"Section\">432B.200</span><span class=\"Empty\"> </span><span class=\"Leadline\">Toll-free telephone number for reports of abuse or neglect.</span><span class=\"Empty\"> </span>The Division of Child and Family Services shall establish and maintain a center with a toll-free telephone number to receive reports of abuse or neglect of a child in this State 24 hours a day, 7 days a week. Any reports made to this center must be promptly transmitted to the agency which provides child welfare services in the community where the child is located.</p> <p class=\"SourceNote\"> (Added to NRS by <a href=\"../Statutes/63rd/Stats198506.html#Stats198506page1371\">1985, 1371</a>; A <a href=\"../Statutes/67th/Stats199313.html#Stats199313page2706\">1993, 2706</a>; <a href=\"../Statutes/17thSS/Stats2001SS1701.html#Stats2001SS1701page36\">2001 Special Session, 36</a>)</p>"
      parseSectionBody "432B.200" dom `shouldBe` expectedHtml


    it "returns the complete HTML - 2" $ do
      html <- chapter_432b_html
      let dom = parseTags html
      let expectedHtml = T.pack "<p class=SectBody><span class=\"Section\">432B.215</span><span class=\"Empty\"> </span><span class=\"Leadline\">Acquisition and use of information concerning probationers and parolees.</span></p> <p class=\"SectBody\"> 1. An agency which provides child welfare services may request the Division of Parole and Probation of the Department of Public Safety to provide information concerning a probationer or parolee that may assist the agency in carrying out the provisions of this chapter. The Division of Parole and Probation shall provide such information upon request.</p> <p class=\"SectBody\"> 2. The agency which provides child welfare services may use the information obtained pursuant to subsection 1 only for the limited purpose of carrying out the provisions of this chapter.</p> <p class=\"SourceNote\"> (Added to NRS by <a href=\"../Statutes/69th/Stats199706.html#Stats199706page835\">1997, 835</a>; A <a href=\"../Statutes/71st/Stats200117.html#Stats200117page2612\">2001, 2612</a>; <a href=\"../Statutes/17thSS/Stats2001SS1701.html#Stats2001SS1701page36\">2001 Special Session, 36</a>; <a href=\"../Statutes/72nd/Stats200301.html#Stats200301page236\">2003, 236</a>)</p> <p class=\"DocHeading2\">Corrective Action, Improvement Plans and Incentive Payments</p>"
      parseSectionBody "432B.215" dom `shouldBe` expectedHtml



--
-- Helper Functions
--
main :: IO()
main =
  hspec spec


chapter_432b_html :: IO Text
chapter_432b_html =
  readFileLatin1 (fixture "nrs-432b.html")
