{-# LANGUAGE OverloadedStrings #-}
module NvStatutesSpec where
import           BasicPrelude
import           Models
import           NvStatutes   (titles)
import           Test.Hspec


spec :: SpecWith ()
spec = parallel $ do

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


  describe "chapters" $

    it "reads a chapter correctly" $ do
      judicialDept <- firstTitle
      length (chapters judicialDept) `shouldNotBe` 0
      let chapter1 = head $ chapters judicialDept

      chapterName   chapter1 `shouldBe` "Judicial Department Generally"
      chapterNumber chapter1 `shouldBe` "1"
      chapterUrl    chapter1 `shouldBe` "https://www.leg.state.nv.us/nrs/NRS-001.html"
  --
  --
  -- describe "sections" $
  --
  --   it "reads a section correctly" $ do
  --     pendingWith "TODO"
  --     judicialDept <- firstTitle
  --     let judicialDeptGenerally = head $ chapters judicialDept
  --     let courtsOfJustice       = head $ sections judicialDeptGenerally
  --
  --     sectionName courtsOfJustice `shouldBe` "Courts of justice"


--
-- Helper Functions
--
main ∷ IO()
main =
  hspec spec


firstTitle ∷ IO Title
firstTitle = do
  html <- nrsIndexHtml
  return (head (titles html))


nrsIndexHtml ∷ IO Text
nrsIndexHtml = readFile "test/fixtures/nrs.html"
