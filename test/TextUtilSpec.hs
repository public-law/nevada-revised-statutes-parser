module TextUtilSpec where

import           BasicPrelude
import           Test.Hspec

import           TextUtil     (isUsuallyUncapitalized, titleize, titleizeWord)


spec :: SpecWith ()
spec = parallel $ do

    describe "isUsuallyUncapitalized" $ do
        it "handles a typical article" $
            isUsuallyUncapitalized "a" `shouldBe` True

        it "handles a normal word" $
            isUsuallyUncapitalized "dog" `shouldBe` False

        it "handles uppercase input" $
            isUsuallyUncapitalized "THE" `shouldBe` True


    describe "titleizeWord" $ do
        it "handles a single punctuation character prefix" $
            titleizeWord "!snurk" `shouldBe` "!Snurk"

        it "handles a word with no punctuation" $
            titleizeWord "snurk" `shouldBe` "Snurk"

        it "handles a word in quotes" $
            titleizeWord "“ABUSE”" `shouldBe` "“Abuse”"


    describe "titleize" $ do
        it "handles all uppercase" $ do
            titleize "PROTECTION OF CHILDREN" `shouldBe` "Protection of Children"

        it "handles all lowercase" $ do
            titleize "protection of children" `shouldBe` "Protection of Children"

        it "handles Unicode double quotes" $ do
            titleize "“ABUSE OR NEGLECT” DEFINED." `shouldBe` "“Abuse or Neglect” Defined."
