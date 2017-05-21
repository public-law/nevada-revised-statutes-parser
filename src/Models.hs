{-# LANGUAGE DeriveGeneric #-}

module Models where

import           BasicPrelude
import           Data.Aeson   (ToJSON)
import           Data.Time    (Day)
import           GHC.Generics (Generic)
import           Year


data NRS =
  NRS {
  statuteTree  ∷ Tree,
  nominalDate  ∷ Year,
  dateAccessed ∷ Day
} deriving (Generic, Show)

data Tree =
  Tree {
  chapter0   ∷ Chapter,
  treeTitles ∷ [Title]
} deriving (Generic, Show)

-- The top-level organizational unit in the Nevada Revised Statutes
data Title =
  Title {
    titleName   ∷ Text,
    titleNumber ∷ Integer,
    chapters    ∷ [Chapter]
} deriving (Generic, Show)

data Chapter =
  Chapter {
    chapterName   ∷ Text,
    chapterNumber ∷ Text,
    chapterUrl    ∷ Text,
    subChapters   ∷ [SubChapter]
} deriving (Generic, Show)

data SubChapter =
  SubChapter {
    subChapterName     ∷ Text,
    subChapterChildren ∷ SubChapterChildList
} deriving (Generic, Show)

-- This datatype is purely an implementation detail.
data SubChapterChildList = SubChapterSections [Section]
                         | SubSubChapters     [SubSubChapter]
    deriving (Generic, Show)

data SubSubChapter =
  SubSubChapter {
    subSubChapterName     ∷ Text,
    subSubChapterSections ∷ [Section]
} deriving (Generic, Show)

data Section =
  Section {
    sectionName   ∷ Text,
    sectionNumber ∷ Text,
    sectionBody   ∷ Text
} deriving (Generic, Show)

instance ToJSON NRS
instance ToJSON Tree
instance ToJSON Title
instance ToJSON Chapter
instance ToJSON SubChapter
instance ToJSON SubChapterChildList
instance ToJSON SubSubChapter
instance ToJSON Section
