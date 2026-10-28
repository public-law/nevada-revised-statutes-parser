{-# OPTIONS_GHC -Wno-x-partial #-}
module NRSParserSpec where

import           BasicPrelude
import qualified Data.HashMap.Lazy             as HM
import           Data.Time                      ( fromGregorian )
import           Test.Hspec

import           ChapterFile                    ( chapterNumberToFilename
                                                , fillInEmptyChapter
                                                )
import           Config                         ( chapterZeroPathname )
import           HtmlUtil
import           Models.Chapter                as Chapter
import           Models.NRS
import           Models.Title                  as Title
import           Models.ParseFailure            ( chapterNumber
                                                , message
                                                , sourceFile
                                                )
import           Models.Tree
import           NRSParser                      ( parseNRS )


spec :: SpecWith ()
spec = parallel $ do
  describe "fillInEmptyChapter" $ do
    it "keeps the empty shell and records a failure when the file is missing" $ do
      let stub = emptyChapterShell "998" "Missing Chapter"
          (ch, fails) = fillInEmptyChapter HM.empty stub
      Chapter.number ch `shouldBe` "998"
      Chapter.name ch `shouldBe` "Missing Chapter"
      case Chapter.content ch of
        ComplexChapterContent [] -> pure ()
        other ->
          expectationFailure $ "expected empty complex content, got " ++ show other
      length fails `shouldBe` 1
      sourceFile (head fails) `shouldBe` "NRS-998.html"
      chapterNumber (head fails) `shouldBe` Just "998"

    it "keeps the empty shell for chaptersToSkip and records a skip failure" $ do
      let stub = emptyChapterShell "111" "Skipped"
          (ch, fails) = fillInEmptyChapter HM.empty stub
      Chapter.number ch `shouldBe` "111"
      length fails `shouldBe` 1
      message (head fails) `shouldBe` "skipped by Config.chaptersToSkip"

    it "returns parsed content plus section failures for broken HTML" $ do
      html <- htmlFixture "NRS-999-missing-body.html"
      let stub = emptyChapterShell "999" "Broken"
          cmap  = HM.singleton (chapterNumberToFilename "999") html
          (ch, fails) = fillInEmptyChapter cmap stub
      case Chapter.content ch of
        SimpleChapterContent sections -> length sections `shouldBe` 1
        ComplexChapterContent xs ->
          expectationFailure $ "expected simple content, got " ++ show xs
      length fails `shouldBe` 1
      chapterNumber (head fails) `shouldBe` Just "999"

  describe "parseNRS" $ do
    it "builds NRS when one chapter is broken and keeps good chapters" $ do
      index <- htmlFixture "index.html"
      ch0   <- htmlFixture "NRS-000.html"
      ch1   <- htmlFixture "NRS-001.html"
      broken <- htmlFixture "NRS-999-missing-body.html"
      let cmap =
            HM.fromList
              [ (chapterZeroPathname, ch0)
              , (chapterNumberToFilename "1", ch1)
              , (chapterNumberToFilename "999", broken)
              ]
          day = fromGregorian 2020 1 1
          (nrs, fails) = parseNRS index cmap day
      Chapter.number (chapter0 (statuteTree nrs)) `shouldBe` "0"
      length fails `shouldNotBe` 0
      let title1Chapters = Title.chapters (head (titles (statuteTree nrs)))
          chapter1 = head title1Chapters
      Chapter.number chapter1 `shouldBe` "1"
      case Chapter.content chapter1 of
        SimpleChapterContent sections -> length sections `shouldNotBe` 0
        ComplexChapterContent subchapters ->
          length subchapters `shouldNotBe` 0


emptyChapterShell :: Text -> Text -> Chapter
emptyChapterShell number' name' = Chapter
  { Chapter.name    = name'
  , Chapter.number  = number'
  , Chapter.url     = "https://example.test/" ++ number' ++ ".html"
  , Chapter.content = ComplexChapterContent []
  }


main :: IO ()
main = hspec spec
