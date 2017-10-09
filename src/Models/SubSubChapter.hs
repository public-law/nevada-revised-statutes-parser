{-# LANGUAGE DeriveGeneric #-}

module Models.SubSubChapter where

import           BasicPrelude
import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)
import           Models.Section

data SubSubChapter =
  SubSubChapter {
    subSubChapterName     ∷ Text,
    subSubChapterSections ∷ [Section]
} deriving (Generic, Show)

instance ToJSON SubSubChapter