{-# LANGUAGE ExtendedDefaultRules #-}

module SectionParserSpec where

import           BasicPrelude
import           Test.Hspec
import           Text.HTML.TagSoup

import           SectionParser


spec :: SpecWith ()
spec = parallel $ describe "isTOCEntry" $ do

  it "recognizes a valid TOC entry" $ do
    let
      html =
        "<p class=\"COLeadline\"><a href=\"#NRS002ASec130\">NRS&#8194;2A.130</a> Benefits for surviving child." :: Text
    let tagsAsIfParsed = takeWhile (~/= "</p>") (parseTags html)

    isTOCEntry tagsAsIfParsed `shouldBe` True

  it "rejects a same-length string of incorrect tags" $ do
    let
      html =
        "<div class=\"COLeadline\"><a href=\"#NRS002ASec130\">NRS&#8194;2A.130</a> Benefits for surviving child." :: Text
    let tagsAsIfParsed = takeWhile (~/= "</p>") (parseTags html)

    isTOCEntry tagsAsIfParsed `shouldBe` False


