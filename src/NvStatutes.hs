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
    titleNumber :: Int,
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


titleCount :: Text -> Int
titleCount indexHtml =
  length $ titles indexHtml


titles :: Text -> [Title]
titles indexHtml =
  let rows   = contentRows indexHtml
      tuples = rowTuples rows
  in fmap newTitle tuples


contentRows :: Text -> [[Tag Text]]
contentRows indexHtml =
  let tags       = parseTags indexHtml
      table      = head $ partitions (~== ("<table class=MsoNormalTable"::String)) tags
  in partitions (~== ("<tr>"::String)) table


newTitle :: [[[Tag Text]]] -> Title
newTitle tuple =
  let titleRow  = head(head tuple)
      rawTitle  = innerText $ head $ partitions (~== ("<b>"::String)) titleRow
      name      = nameFromRawTitle rawTitle
  in Title { titleName = name, titleNumber = 0, chapters = [] }


-- Input: "TITLE\n  1 \8212 STATE JUDICIAL DEPARTMENT\n  \n \n "
-- Output: "STATE JUDICIAL DEPARTMENT"
nameFromRawTitle :: Text -> Text
nameFromRawTitle text =
  strip $ convertString $ head $ tail $ splitOn "\8212" (convertString text)


rowTuples :: [[Tag Text]] -> [[[[Tag Text]]]]
rowTuples rows =
  split (whenElt isTitleRow) rows
    & tail
    & chunksOf 2


isTitleRow :: [Tag Text] -> Bool
isTitleRow r =
  length (partitions (~== ("<td>"::String)) r) == 1


newChapter :: [Tag Text] -> Chapter
newChapter row =
  Chapter {chapterName="", chapterNumber="", url=""}
