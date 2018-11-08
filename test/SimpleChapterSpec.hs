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

  it "finds the correct section names"   pending

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
