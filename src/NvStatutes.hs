{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module NvStatutes where

import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (confCompare, defConfig,
                                           encodePretty', keyOrder)
import qualified Data.ByteString.Lazy     as B
import           Data.Function            ((&))
import           Data.List.Split
import           Data.String.Conversions
import           Data.Text                (Text, strip)
import           GHC.Generics
import           Text.HTML.TagSoup
import           Text.StringLike


data Title =
  Title {
    titleName   :: Text,
    titleNumber :: Integer,
    chapters    :: [Chapter]
} deriving (Generic, Show)


data Chapter =
  Chapter {
    chapterName   :: Text,
    chapterNumber :: Text,
    url           :: Text
} deriving (Generic, Show)

instance ToJSON Title
instance ToJSON Chapter


titles indexHtml =
  let tags       = parseTags indexHtml
      table      = head $ partitions (~== ("<table class=MsoNormalTable"::String)) tags
      rows       = partitions (~== ("<tr>" :: String)) table
      tuples     = rowTuples rows
  in makeTitles tuples


makeTitles tuples =
  fmap newTitle tuples


newTitle tuple =
  let title_row = head(head tuple)
      rawTitle  = innerText $ head $ partitions (~== ("<b>"::String)) title_row
      name      = nameFromRawTitle rawTitle
  in Title { titleName = name, titleNumber = 0, chapters = [] }


-- Input: "TITLE\n  1 \8212 STATE JUDICIAL DEPARTMENT\n  \n \n "
-- Output: "STATE JUDICIAL DEPARTMENT"
nameFromRawTitle :: Text -> Text
nameFromRawTitle text =
  strip $ convertString $ head $ tail $ splitOn "\8212" (convertString text)


titleCount indexHtml =
  let tags       = parseTags indexHtml
      table      = head $ partitions (~== ("<table class=MsoNormalTable"::String)) tags
      rows       = partitions (~== ("<tr>" :: String)) table
      title_rows = filter isTitleRow rows
  in length title_rows


rowTuples :: [[Tag Text]] -> [[[[Tag Text]]]]
rowTuples rows =
  split (whenElt isTitleRow) rows
    & tail
    & chunksOf 2

isTitleRow :: [Tag Text] -> Bool
isTitleRow r =
  length (partitions (~== ("<td>"::String)) r) == 1


newChapter row =
  Chapter {chapterName="", chapterNumber="", url=""}
