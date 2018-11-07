{-# LANGUAGE ExtendedDefaultRules #-}


module SimpleChapterSpec where

import           BasicPrelude
import           Test.Hspec

-- What exactly is this?
-- import          Text.InterpolatedString.Perl6 (q)

import           Models.Chapter                as Chapter

import           ChapterFile
import           HtmlUtil


spec :: SpecWith ()
spec = parallel $

  describe "parseChapter" $

    it "recognizes a chapter with simple content" $ do
      html ← chapter_0_html
      case content (parseChapter html) of
        SimpleChapterContent sections -> length sections `shouldBe` 21
        ComplexChapterContent _ -> error "Got Subchapters but expected Sections"


--
-- Helper Functions
--
main :: IO ()
main = hspec spec


chapter_0_html :: IO Html
chapter_0_html = htmlFixture "NRS-000.html"
