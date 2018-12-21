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
    isTOCEntry (tagsAsIfParsed html) `shouldBe` True

  it "rejects a same-length list of incorrect tags" $ do
    let html = "<i class=\"COLeadline\"><a href=\"#N\">NRS</a> Hello." :: Text
    isTOCEntry (tagsAsIfParsed html) `shouldBe` False

  it "rejects a same-length list of incorrect tags" $ do
    let html = "<p class=\"COLeadline\"><i href=\"#N\">NRS</i> Hello." :: Text
    isTOCEntry (tagsAsIfParsed html) `shouldBe` False

  it "rejects a blank paragraph" $ do
    let html = "<p class=\"COLeadline\"><a href=\"#N\"> </a> &nbsp;" :: Text
    isTOCEntry (tagsAsIfParsed html) `shouldBe` False

  it "rejects a list missing a text tag" $ do
    let
      html
        = "<p class=\"COLeadline\"><a href=\"#NRS002ASec130\">NRS&#8194;2A.130</a><hr>" :: Text
    isTOCEntry (tagsAsIfParsed html) `shouldBe` False



-- Helper functions

-- In the app, the parser returns the paragraph tags minus the
-- closing </p>.
tagsAsIfParsed :: Text -> [Tag Text]
tagsAsIfParsed html = takeWhile (~/= "</p>") (parseTags html)
