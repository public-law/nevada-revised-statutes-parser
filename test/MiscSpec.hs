module MiscSpec where

import           NvStatutes
import           Test.Hspec

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "titleCount" $
    it "finds the correct number of titles" $ do
      html <- readFile "nrs.html"
      titleCount html `shouldBe` 59

  describe "titles" $ do
    it "gets the first title's number " $ do
      html <- readFile "nrs.html"
      titleNumber (head (titles html)) `shouldBe` 1

    it "gets the first title's name" $ do
      pending
      html <- readFile "nrs.html"
      titleName (head (titles html)) `shouldBe` "STATE JUDICIAL DEPARTMENT"
