{-# LANGUAGE DeriveGeneric #-}

module Models.Section where

import           BasicPrelude
import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)


data Section =
  Section {
    sectionName   ∷ Text,
    sectionNumber ∷ Text,
    sectionBody   ∷ Text
} deriving (Generic, Show)

instance ToJSON Section
