{-# LANGUAGE ExtendedDefaultRules #-}

module SectionParserSpec where

import           BasicPrelude
import           Test.Hspec
import           Text.HTML.TagSoup

import           SectionParser


spec :: SpecWith ()
spec =
  parallel $ describe "isTOCEntry" $ it "recognizes a valid TOC entry" $ do
    let
      html =
        ("<p class=\"COLeadline\"><a href=\"#NRS002ASec130\">NRS&#8194;2A.130</a> Benefits for surviving child." :: Text
        )
    let tags = parseTags html

    isTOCEntry tags `shouldBe` True
