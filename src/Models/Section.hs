module Models.Section where

import           BasicPrelude
import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)


data Section =
  Section {
    name   ∷ Text,
    number ∷ Text,
    body   ∷ Text
} deriving (Generic, Show)

instance ToJSON Section
