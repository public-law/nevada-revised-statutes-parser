{-# LANGUAGE DeriveGeneric #-}

module NvStatutes where

import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (confCompare, defConfig,
                                           encodePretty', keyOrder)
import qualified Data.ByteString.Lazy     as B
import           Data.Function            ((&))
import           Data.List.Split          (chunksOf, split, whenElt)
import           GHC.Generics
import           Text.HTML.TagSoup        (parseTags, partitions, (~/=), (~==))


data Title =
  Title {
    titleName   :: String,
    titleNumber :: Integer,
    chapters    :: [Chapter]
} deriving (Generic, Show)


data Chapter =
  Chapter {
    chapterName   :: String,
    chapterNumber :: String,
    url           :: String
} deriving (Generic, Show)

instance ToJSON Title
instance ToJSON Chapter


titles indexHtml =
  let tags       = parseTags indexHtml
      table      = head $ partitions (~== "<table class=MsoNormalTable") tags
      rows       = partitions (~== "<tr>") table
      tuples     = rowTuples rows
  in []


titleCount indexHtml =
  let tags       = parseTags indexHtml
      table      = head $ partitions (~== "<table class=MsoNormalTable") tags
      rows       = partitions (~== "<tr>") table
      title_rows = filter isTitleRow rows
  in length title_rows


rowTuples rows =
  split (whenElt isTitleRow) rows
    & tail
    & chunksOf 2


isTitleRow r =
  length (partitions (~== "<td>") r) == 1


newChapter row =
  Chapter {chapterName="", chapterNumber="", url=""}
