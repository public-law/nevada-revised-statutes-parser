{-# LANGUAGE ExtendedDefaultRules #-}


module SimpleChapterSpec where

import           BasicPrelude
import           Test.Hspec

import           Models.Chapter                as Chapter
import           Models.Section                as Section

import           ChapterFile
import           HtmlUtil


spec :: SpecWith ()
spec = parallel $ describe "parseChapter" $ do

  it "recognizes a chapter with simple content" $ do
    html ← chapter_0_html
    case content (parseChapter html) of
      SimpleChapterContent _ -> pure ()
      ComplexChapterContent xs ->
        expectationFailure $ "Expected Sections but got SubChapters" ++ show xs

  it "finds the correct number of sections" $ do
    html ← chapter_0_html
    length (simpleChapterContent html) `shouldBe` 21

  it "finds the correct section names" $ do
    html <- chapter_0_html
    let firstSection = head $ simpleChapterContent html
    Section.name firstSection `shouldBe` "Scope."

  it "finds the correct section number" $ do
    html <- chapter_0_html
    let firstSection = head $ simpleChapterContent html
    Section.number firstSection `shouldBe` "0.010"

  it "finds the correct section content" $ do
    html <- chapter_0_html
    let firstSection = head $ simpleChapterContent html
    Section.body firstSection
      `shouldBe` "<p class=SectBody><span class=\"Section\">0.010</span><span class=\"Empty\"> </span><span class=\"Leadline\">Scope.</span><span class=\"Empty\"> </span>This chapter provides definitions and declarations of legislative intent which apply to Nevada Revised Statutes as a whole.</p> <p class=\"SourceNote\"> (Added to NRS by <a href=\"../Statutes/59th/Stats197701.html#Stats197701page181\">1977, 181</a>)</p>"


  it "finds the correct section names" $ do
    pendingWith "TODO: bug fix"
    html <- chapter_0_html
    let lastSection = last $ simpleChapterContent html
    Section.name lastSection `shouldBe` "“Substantial bodily harm” defined."

  it "finds the correct section number"  pending

  it "finds the correct section content" pending

--
-- Helper Functions
--
main :: IO ()
main = hspec spec


-- A partial function to grab the simple chapter content,
-- or fail.
simpleChapterContent :: Html -> [Section]
simpleChapterContent html = case content (parseChapter html) of
  SimpleChapterContent sections -> sections
  ComplexChapterContent xs ->
    error $ "Expected Sections but got SubChapters" ++ show xs


chapter_0_html :: IO Html
chapter_0_html = htmlFixture "NRS-000.html"
