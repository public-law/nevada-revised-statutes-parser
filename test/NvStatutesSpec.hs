{-# LANGUAGE OverloadedStrings #-}
module NvStatutesSpec where
import           BasicPrelude
import           Models
import           NvStatutes   (nrsIndexHtml, titles)
import           Test.Hspec


spec = parallel $
  describe "titles" $ do

    it "finds the correct number of titles" $ do
      html <- nrsIndexHtml
      length (titles html) `shouldBe` 59


    it "gets a title's name" $ do
      judicialDept <- firstTitle
      titleName judicialDept `shouldBe` "STATE JUDICIAL DEPARTMENT"


    it "gets a title's number" $ do
      judicialDept <- firstTitle
      titleNumber judicialDept `shouldBe` 1


    it "reads a chapter correctly" $ do
      judicialDept <- firstTitle
      length (chapters judicialDept) `shouldNotBe` 0

      let firstChapter = head (chapters judicialDept)
      chapterName   firstChapter `shouldBe` "Judicial Department Generally"
      chapterNumber firstChapter `shouldBe` "1"
      chapterUrl    firstChapter `shouldBe` "https://www.leg.state.nv.us/nrs/NRS-001.html"


--
-- Helper Functions
--
firstTitle :: IO Title
firstTitle = do
  html <- nrsIndexHtml
  return (head (titles html))

main :: IO()
main =
  hspec spec
