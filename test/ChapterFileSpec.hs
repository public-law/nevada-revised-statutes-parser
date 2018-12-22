{-# LANGUAGE ExtendedDefaultRules #-}


module ChapterFileSpec where

import           BasicPrelude
import           Test.Hspec
-- import           Text.HTML.TagSoup

import           Models.Chapter                as Chapter
import           Models.Section                as Section
import           Models.SubChapter             as SubChapter
import           Models.SubSubChapter          as SubSubChapter

import           ChapterFile
-- import           SimpleChapterFile
import           HtmlUtil


spec :: SpecWith ()
spec = parallel $ describe "parseChapter" $ do

  it "gets the chapter name" $ do
    html ← chapter_432b_html
    Chapter.name (unwrap $ parseChapter html)
      `shouldBe` "Protection of Children from Abuse and Neglect"

  -- it "gets the chapter name when it has embedded newline" $ do
  --     html ← chapter_575_html
  --     Chapter.name (unwrap $ parseChapter html) `shouldBe` "Miscellaneous Provisions; Collection of Taxes"

  it "gets the chapter number" $ do
    html ← chapter_432b_html
    Chapter.number (unwrap $ parseChapter html) `shouldBe` "432B"


  it "gets the chapter URL" $ do
    html ← chapter_432b_html
    url (unwrap $ parseChapter html)
      `shouldBe` "https://www.leg.state.nv.us/nrs/NRS-432B.html"


  it "gets the sub-chapter names - 1" $ do
    html ← chapter_432b_html
    let innards = content (unwrap $ parseChapter html)
    case innards of
      ComplexChapterContent subchapters ->
        SubChapter.name (subchapters !! 0) `shouldBe` "General Provisions"
      _ -> error "Got Sections but expected SubChapters"


  it "gets the sub-chapter names - 2" $ do
    html ← chapter_432b_html
    let innards = content (unwrap $ parseChapter html)
    case innards of
      ComplexChapterContent subchapters ->
        SubChapter.name (subchapters !! 1) `shouldBe` "Administration"
      _ -> error "Got Sections but expected SubChapters"


  it "gets the sub-chapter names - 3" $ do
    html ← chapter_432b_html
    let innards = content (unwrap $ parseChapter html)
    case innards of
      ComplexChapterContent subchapters ->
        SubChapter.name (subchapters !! 3)
          `shouldBe` "Protective Services and Custody"
      _ -> error "Got Sections but expected SubChapters"


  it "gets a simple sub-chapter's sections" $ do
    html <- chapter_432b_html
    let generalProvisions = head $ subChapters (unwrap $ parseChapter html)
    case children generalProvisions of
      SubChapterSections xs -> length xs `shouldBe` 31
      SubSubChapters _ -> error "Got sub-sub chapters but expected Sections"


  it "gets the sub-chapter's section names right - 1" $ do
    html <- chapter_432b_html
    let generalProvisions = head $ subChapters (unwrap $ parseChapter html)
    case children generalProvisions of
      SubChapterSections xs ->
        show (Section.name (head xs)) `shouldBe` "Definitions."
      SubSubChapters _ -> error "Got sub-sub chapters but expected Sections"


  it "gets the sub-chapter's section names right - 2" $ do
    html <- chapter_432b_html
    let generalProvisions = head $ subChapters (unwrap $ parseChapter html)
    case children generalProvisions of
      SubChapterSections xs ->
        show (Section.name (xs !! 1))
          `shouldBe` "“Abuse or neglect of a child” defined."
      SubSubChapters _ -> error "Got sub-sub chapters but expected Sections"

  it "gets the sub-chapter's section numbers right" $ do
    html <- chapter_432b_html
    let generalProvisions = head $ subChapters (unwrap $ parseChapter html)
    case children generalProvisions of
      SubChapterSections xs ->
        show (Section.number (xs !! 1)) `shouldBe` "432B.020"
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



  -- TODO: Figure out how to get these tests working again, after the
  --       introduction of ChapterData.


  -- describe "isSimpleSubChapter" $ do

  --   it "correctly identifies a simple sub-chapter" $ do
  --     html <- chapter_432b_html
  --     let generalProvisions = (!! 0) $ headingGroups $ parseTags $ toText html
  --     isSimpleSubChapter generalProvisions `shouldBe` True

  --   it "correctly identifies a complex sub-chapter" $ do
  --     html <- chapter_432b_html
  --     let administration = (!! 1) $ headingGroups $ parseTags $ toText html
  --     isSimpleSubChapter administration `shouldBe` False



  -- describe "section" $ do

  --   it "returns the complete HTML - 1" $ do
  --     html <- chapter_432b_html
  --     let dom = parseTags $ toText html
  --     let
  --       expectedHtml
  --         = "<p class=SectBody>The Division of Child and Family Services shall establish and maintain a center with a toll-free telephone number to receive reports of abuse or neglect of a child in this State 24 hours a day, 7 days a week. Any reports made to this center must be promptly transmitted to the agency which provides child welfare services in the community where the child is located.</p> <p class=\"SourceNote\"> (Added to NRS by <a href=\"../Statutes/63rd/Stats198506.html#Stats198506page1371\">1985, 1371</a>; A <a href=\"../Statutes/67th/Stats199313.html#Stats199313page2706\">1993, 2706</a>; <a href=\"../Statutes/17thSS/Stats2001SS1701.html#Stats2001SS1701page36\">2001 Special Session, 36</a>)</p>"
  --     parseSectionBody "432B.200" dom `shouldBe` expectedHtml


  --   it "returns the complete HTML - 2" $ do
  --     html <- chapter_432b_html
  --     let dom = parseTags $ toText html
  --     let
  --       expectedHtml
  --         = "<p class=SectBody>1. An agency which provides child welfare services may request the Division of Parole and Probation of the Department of Public Safety to provide information concerning a probationer or parolee that may assist the agency in carrying out the provisions of this chapter. The Division of Parole and Probation shall provide such information upon request.</p> <p class=\"SectBody\"> 2. The agency which provides child welfare services may use the information obtained pursuant to subsection 1 only for the limited purpose of carrying out the provisions of this chapter.</p> <p class=\"SourceNote\"> (Added to NRS by <a href=\"../Statutes/69th/Stats199706.html#Stats199706page835\">1997, 835</a>; A <a href=\"../Statutes/71st/Stats200117.html#Stats200117page2612\">2001, 2612</a>; <a href=\"../Statutes/17thSS/Stats2001SS1701.html#Stats2001SS1701page36\">2001 Special Session, 36</a>; <a href=\"../Statutes/72nd/Stats200301.html#Stats200301page236\">2003, 236</a>)</p> <p class=\"DocHeading2\">Corrective Action, Improvement Plans and Incentive Payments</p>"
  --     parseSectionBody "432B.215" dom `shouldBe` expectedHtml




--
-- Helper Functions
--
main :: IO ()
main = hspec spec


chapter_432b_html :: IO Html
chapter_432b_html = htmlFixture "NRS-432B.html"


chapter_575_html :: IO Html
chapter_575_html = htmlFixture "NRS-575.html"


-- Return a Chapter's sub-chapters, or raise an error
-- if it's a simple Chapter with just sections.
subChapters :: Chapter -> [SubChapter]
subChapters chapter = case content chapter of
  ComplexChapterContent subchapters -> subchapters
  SimpleChapterContent _ -> error "got Sections but expected Subchapters"


unwrap :: Either String a -> a
unwrap thing = case thing of
  Right contents -> contents
  Left  message  -> error message
