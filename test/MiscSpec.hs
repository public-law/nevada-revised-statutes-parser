module MiscSpec where

  import           Test.Hspec
  import   NvStatutes


  main :: IO()
  main = hspec spec

  spec :: Spec
  spec =
    describe "titleCount" $
      it "finds the correct number of titles" $ do
        html <- readFile "nrs.html"
        titleCount html `shouldBe` 59
