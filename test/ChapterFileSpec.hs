{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use head" #-}


module ChapterFileSpec where

import           BasicPrelude
import           Test.Hspec

import           Models.Chapter                as Chapter
import           Models.Section                as Section
import           Models.SubChapter             as SubChapter
import           Models.SubSubChapter          as SubSubChapter
import           Parsing

import           ChapterFile
import           SimpleChapterFile
import           HtmlUtil


spec :: SpecWith ()
spec = do

  let chapterData  = chapter_432b_data
  let chapter_432b = unwrap . parseChapter <$> chapter_432b_html

  describe "parseChapter" $ do

    it "gets the chapter name" $ do
      chapter <- chapter_432b
      Chapter.name chapter
        `shouldBe` "Protection of Children from Abuse and Neglect"

    -- it "gets the chapter name when it has embedded newline" $ do
    --     html ← chapter_575_html
    --     Chapter.name (unwrap $ parseChapter html) `shouldBe` "Miscellaneous Provisions; Collection of Taxes"

    it "gets the chapter number" $ do
      chapter <- chapter_432b

      Chapter.number chapter `shouldBe` "432B"


    it "gets the chapter URL" $ do
      chapter <- chapter_432b
      url chapter `shouldBe` "https://www.leg.state.nv.us/nrs/NRS-432B.html"


    it "gets the sub-chapter names - 1" $ do
      chapter <- chapter_432b
      let innards = Chapter.content chapter
      case innards of
        ComplexChapterContent subchapters ->
          SubChapter.name (subchapters !! 0) `shouldBe` "General Provisions"
        _ -> error "Got Sections but expected SubChapters"


    it "gets the sub-chapter names - 2" $ do
      chapter <- chapter_432b
      let innards = Chapter.content chapter
      case innards of
        ComplexChapterContent subchapters ->
          SubChapter.name (subchapters !! 1) `shouldBe` "Administration"
        _ -> error "Got Sections but expected SubChapters"


    it "gets the sub-chapter names - 3" $ do
      chapter <- chapter_432b
      let innards = Chapter.content chapter
      case innards of
        ComplexChapterContent subchapters ->
          SubChapter.name (subchapters !! 3)
            `shouldBe` "Protective Services and Custody"
        _ -> error "Got Sections but expected SubChapters"


    it "finds the right number of sub-chapters" $ do
      chapter <- chapter_432b

      let innards = Chapter.content chapter
      case innards of
        ComplexChapterContent subchapters -> length subchapters `shouldBe` 12
        _ -> error "Got Sections but expected SubChapters"


    it "finds the right sub-chapters" $ do
      chapter <- chapter_432b

      let innards = Chapter.content chapter
      case innards of
        ComplexChapterContent subchapters ->
          map SubChapter.name subchapters
            `shouldBe` [ "General Provisions"
                       , "Administration"
                       , "Reports of Abuse or Neglect; Reports of Prenatal Substance Abuse"
                       , "Protective Services and Custody"
                       , "Child Death Review Teams"
                       , "Civil Proceedings"
                       , "Continuation of Jurisdiction of Court Over Child Who Reaches 18 Years of Age While in Custody of Agency Which Provides Child Welfare Services"
                       , "Local Advisory Boards to Expedite Proceedings for Placement of Children"
                       , "Court-ordered Admission of Certain Children With Emotional Disturbance to Certain Facilities"
                       , "Sexual Abuse or Sexual Exploitation of Children Under Age of 18 Years"
                       , "Kinship Guardianship Assistance Program"
                       , "Miscellaneous Provisions"
                       ]
        _ -> error "Got Sections but expected SubChapters"


    it "gets the sub-chapter's section names - 1" $ do
      generalProvisions <- head . subChapters <$> chapter_432b

      case children generalProvisions of
        SubChapterSections xs ->
          show (Section.name (head xs)) `shouldBe` "Definitions."
        SubSubChapters _ -> error "Got sub-sub chapters but expected Sections"


    it "gets the sub-chapter's section names - 2" $ do
      generalProvisions <- head . subChapters <$> chapter_432b

      case children generalProvisions of
        SubChapterSections xs ->
          show (Section.name (xs !! 1))
            `shouldBe` "“Abuse or neglect of a child” defined."
        SubSubChapters _ -> error "Got sub-sub chapters but expected Sections"


    it "gets all the sub-chapter's section numbers" $ do
      chapter <- chapter_432b
      let generalProvisions = head $ subChapters chapter
      case children generalProvisions of
        SubChapterSections xs ->
          map (show . Section.number) xs
            `shouldBe` [ "432B.010"
                       , "432B.020"
                       , "432B.030"
                       , "432B.035"
                       , "432B.040"
                       , "432B.042"
                       , "432B.044"
                       , "432B.050"
                       , "432B.060"
                       , "432B.065"
                       , "432B.0655"
                       , "432B.066"
                       , "432B.067"
                       , "432B.068"
                       , "432B.069"
                       , "432B.070"
                       , "432B.080"
                       , "432B.090"
                       , "432B.100"
                       , "432B.110"
                       , "432B.121"
                       , "432B.130"
                       , "432B.135"
                       , "432B.140"
                       , "432B.150"
                       , "432B.153"
                       , "432B.157"
                       , "432B.159"
                       , "432B.160"
                       , "432B.163"
                       , "432B.165"
                       , "432B.170"
                       , "432B.172"
                       , "432B.174"
                       , "432B.175"
                       , "432B.178"
                       ]
        SubSubChapters _ -> error "Got sub-sub chapters but expected Sections"


    it "gets a complex sub-chapter's sub-sub-chapters" $ do
      html <- chapter_432b_html
      let administration = (!! 1) $ subChapters $ unwrap $ parseChapter html
      case children administration of
        SubSubChapters xs -> length xs `shouldBe` 3
        SubChapterSections _ ->
          error "Got sections but expected sub-sub-chapters"


    it "gets a complex sub-chapter's sub-sub-chapter names - 1" $ do
      html <- chapter_432b_html
      let administration = (!! 1) $ subChapters $ unwrap $ parseChapter html
      case children administration of
        SubSubChapters xs ->
          SubSubChapter.name (head xs) `shouldBe` "General Provisions"
        SubChapterSections _ ->
          error "Got sections but expected sub-sub-chapters"


    it "gets a complex sub-chapter's sub-sub-chapter names - 2" $ do
      html <- chapter_432b_html
      let administration = (!! 1) $ subChapters $ unwrap $ parseChapter html
      case children administration of
        SubSubChapters xs ->
          SubSubChapter.name (xs !! 2)
            `shouldBe` "Grants to Agency Which Provides Child Welfare Services"
        SubChapterSections _ ->
          error "Got sections but expected sub-sub-chapters"


    it "gets a complex sub-chapter's sub-sub-chapter sections" $ do
      html <- chapter_432b_html
      let administration = (!! 1) $ subChapters $ unwrap $ parseChapter html
      case children administration of
        SubSubChapters xs ->
          show (Section.name ((!! 0) $ SubSubChapter.sections (head xs)))
            `shouldBe` "Duties of Division of Child and Family Services."
        SubChapterSections _ ->
          error "Got sections but expected sub-sub-chapters"


    it "gets a complex sub-chapter's sub-sub-chapter section numbers" $ do
      html <- chapter_432b_html
      let administration = (!! 1) $ subChapters $ unwrap $ parseChapter html
      case children administration of
        SubSubChapters xs ->
          map (show . Section.number) (SubSubChapter.sections (head xs))
            `shouldBe` [ "432B.180"
                       , "432B.190"
                       , "432B.195"
                       , "432B.197"
                       , "432B.198"
                       , "432B.199"
                       , "432B.200"
                       , "432B.210"
                       , "432B.215"
                       ]
        SubChapterSections _ ->
          error "Got sections but expected sub-sub-chapters"


    describe "isSimpleSubChapter" $ do

      it "correctly identifies a simple sub-chapter" $ do
        generalProvisions <- ((!! 0) . subChapterHeadingGroups) <$> chapterData
        isSimpleSubChapter generalProvisions `shouldBe` True

      it "correctly identifies a complex sub-chapter" $ do
        administration <- ((!! 1) . subChapterHeadingGroups) <$> chapterData
        isSimpleSubChapter administration `shouldBe` False



    describe "section" $ it "returns the complete HTML - 2" $ do
      let expectedHtml = Right "<p class=SectBody>1. An agency which provides child welfare services may request the Division of Parole and Probation of the Department of Public Safety to provide information concerning a probationer or parolee that may assist the agency in carrying out the provisions of this chapter. The Division of Parole and Probation shall provide such information upon request.</p> <p class=\"SectBody\"> 2. The agency which provides child welfare services may use the information obtained pursuant to subsection 1 only for the limited purpose of carrying out the provisions of this chapter.</p> <p class=\"SourceNote\"> (Added to NRS by <a href=\"../Statutes/69th/Stats199706.html#Stats199706page835\">1997, 835</a>; A <a href=\"../Statutes/71st/Stats200117.html#Stats200117page2612\">2001, 2612</a>; <a href=\"../Statutes/17thSS/Stats2001SS1701.html#Stats2001SS1701page36\">2001 Special Session, 36</a>; <a href=\"../Statutes/72nd/Stats200301.html#Stats200301page236\">2003, 236</a>)</p> <p class=\"DocHeading2\">Corrective Action, Improvement Plans and Incentive Payments</p>"
      actualHtml <- parseSectionBody "432B.215" <$> chapterData

      actualHtml `shouldBe` expectedHtml




--
-- Helper Functions
--
main :: IO ()
main = hspec spec


chapter_432b_data :: IO ChapterData
chapter_432b_data = makeChapterData <$> chapter_432b_html


chapter_432b_html :: IO Html
chapter_432b_html = htmlFixture "NRS-432B.html"


chapter_575_html :: IO Html
chapter_575_html = htmlFixture "NRS-575.html"


-- Return a Chapter's sub-chapters, or raise an error
-- if it's a simple Chapter with just sections.
subChapters :: Chapter -> [SubChapter]
subChapters chapter = case Chapter.content chapter of
  ComplexChapterContent subchapters -> subchapters
  SimpleChapterContent _ -> error "got Sections but expected Subchapters"


unwrap :: (Either String a) -> a
unwrap thing = case thing of
  Right contents -> contents
  Left  message  -> error message
