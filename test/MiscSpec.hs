module MiscSpec where

  import           Test.Hspec


  main :: IO()
  main = hspec spec

  spec :: Spec
  spec =
    describe "titleCount" $
      it "finds the correct number of titles" $
        titleCount `shouldBe` 59
