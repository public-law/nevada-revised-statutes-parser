module Models.SubChapter where

import           BasicPrelude
import           Data.Aeson           (ToJSON)
import           GHC.Generics         (Generic)

import           Models.Section
import           Models.SubSubChapter


data SubChapter =
  SubChapter {
    name     ∷ Text,
    children ∷ SubChapterChildList
} deriving (Generic, Show)


-- This datatype is purely an implementation detail.
data SubChapterChildList = SubChapterSections [Section]
                         | SubSubChapters     [SubSubChapter]
    deriving (Generic, Show)


instance ToJSON SubChapter
instance ToJSON SubChapterChildList
