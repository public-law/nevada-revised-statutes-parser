module MiscSpec where

import           NvStatutes
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "titleCount" $
    it "finds the correct number of titles" $ do
      html <- readFile "nrs.html"
      titleCount html `shouldBe` 59

  describe "titles" $ do
    it "gets the first title's number " $ do
      html <- readFile "nrs.html"
      title_number (head (titles html)) `shouldBe` 1

    it "gets the first title's name" $ do
      html <- readFile "nrs.html"
      title_name (head (titles html)) `shouldBe` "STATE JUDICIAL DEPARTMENT"
