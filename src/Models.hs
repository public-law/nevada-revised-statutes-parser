{-# LANGUAGE DeriveGeneric #-}

module Models where

import           BasicPrelude
import           Data.Aeson   (ToJSON)
import           Data.Text    (Text)
import           Data.Time    (Day, defaultTimeLocale, parseTimeOrError)
import           GHC.Generics (Generic)


data NRS =
  NRS {
  statuteTree  :: Tree,
  nominalDate  :: Year,
  dateAccessed :: Day
} deriving (Generic, Show)

data Tree =
  Tree {
  chapter0   :: Chapter,
  treeTitles :: [Title]
} deriving (Generic, Show)

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
    sections      :: [Section],
    url           :: Text
} deriving (Generic, Show)

data Section =
  Section {
    sectionName   :: Text,
    sectionNumber :: Text,
    sectionBody   :: Text
} deriving (Generic, Show)

instance ToJSON NRS
instance ToJSON Tree
instance ToJSON Title
instance ToJSON Chapter
instance ToJSON Section

type Year = Integer
