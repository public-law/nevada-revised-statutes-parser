module Models.Section where

import           BasicPrelude
import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)

import           HtmlUtil     (Html)

data Section =
  Section {
    name   ∷ Text,
    number ∷ Text,
    body   ∷ Html
} deriving (Generic, Show)

instance ToJSON Section
