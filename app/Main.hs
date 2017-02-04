{-# LANGUAGE DeriveGeneric #-}

import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (confCompare, defConfig,
                                           encodePretty', keyOrder)
import qualified Data.ByteString.Lazy     as B
import           Data.Function            ((&))
import           Data.List.Split          (chunksOf, split, whenElt)
import           GHC.Generics
import           Text.HTML.TagSoup        (parseTags, partitions, (~/=), (~==))


data Container =
  Container {
    header :: Integer,
    body   :: [Integer]
  } deriving (Generic, Show)

data Title =
  Title {
    titleName :: String,
    chapters  :: [Chapter]
} deriving (Generic, Show)

data Chapter =
  Chapter {
    chapterName :: String,
    number      :: String,
    url         :: String
} deriving (Generic, Show)


instance ToJSON Container
instance ToJSON Title
instance ToJSON Chapter


main = do
  html <- readFile "nrs.html"
  let tags   = parseTags html
  let table  = head $ partitions (~== "<table class=MsoNormalTable") tags
  let rows   = partitions (~== "<tr>") table
  let tuples = rowTuples rows


  putStr ("Hello world: " ++ show (fmap isTitleRow rows))


rowTuples rows =
  split (whenElt isTitleRow) rows
    & tail
    & chunksOf 2


isTitleRow r =
  length (partitions (~== "<td>") r) == 1


newChapter row =
  Chapter {chapterName="", number="", url=""}
