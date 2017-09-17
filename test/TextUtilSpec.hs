{-# LANGUAGE OverloadedStrings #-}

module TextUtilSpec where

import           BasicPrelude
import           Test.Hspec

import           TextUtil       (shouldCapitalize, titleize, titleizeWord)


spec :: SpecWith ()
spec = parallel $ do

    describe "shouldCapitalize" $ do
        it "handles a typical article" $
            shouldCapitalize "a" `shouldBe` False

        it "handles a normal word" $
            shouldCapitalize "dog" `shouldBe` True

        it "handles uppercase input" $
            shouldCapitalize "THE" `shouldBe` False


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
