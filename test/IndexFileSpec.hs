module IndexFileSpec where

import           BasicPrelude
import           Data.Either    (fromRight)
import           Test.Hspec

import           FileUtil       (fixture)
import           HtmlUtil
import           IndexFile      (parseTitles)

import           Models.Chapter as Chapter
import           Models.Title   as Title

spec :: SpecWith ()
spec =
  parallel $
  describe "parseTitles" $ do
    it "finds the correct number of titles" $ do
      html <- nrsIndexHtml
      case parseTitles html of
        Right titles -> length titles `shouldBe` 59
        Left message -> error message

    it "gets a title's name" $ do
      judicialDept <- firstTitle
      Title.name judicialDept `shouldBe` "State Judicial Department"

    it "gets a title's name" $ do
      t <- title17
      Title.name t `shouldBe` "State Legislative Department"

    it "gets a title's number" $ do
      judicialDept <- firstTitle
      Title.number judicialDept `shouldBe` 1

    it "reads a chapter correctly" $ do
      judicialDept <- firstTitle
      length (chapters judicialDept) `shouldNotBe` 0
      let chapter1 = head $ chapters judicialDept
      Chapter.name chapter1 `shouldBe` "Judicial Department Generally"
      Chapter.number chapter1 `shouldBe` "1"
      Chapter.url chapter1 `shouldBe`
        "https://www.leg.state.nv.us/NRS/NRS-001.html"

    it "gets one that is further in" $ do
      publicWelfare <- title38
      Title.name publicWelfare `shouldBe` "Public Welfare"
      let chapter432b = last $ chapters publicWelfare
      Chapter.name chapter432b `shouldBe`
        "Protection of Children From Abuse and Neglect"

--
-- Helper Functions
--
main :: IO ()
main = hspec spec

firstTitle :: IO Title
firstTitle = head <$> parsedTitles

title17 :: IO Title
title17 = (!! 16) <$> parsedTitles

title38 :: IO Title
title38 = (!! 37) <$> parsedTitles

parsedTitles :: IO [Title]
parsedTitles = (fromRight []) . parseTitles <$> nrsIndexHtml

nrsIndexHtml :: IO Html
nrsIndexHtml = NewHtml <$> readFile (fixture "index.html")
