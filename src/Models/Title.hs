module Models.Title where

import           BasicPrelude
import           Data.Aeson     (ToJSON)
import           GHC.Generics   (Generic)

import           Models.Chapter


-- The top-level organizational unit in the Nevada Revised Statutes
data Title =
  Title {
    name     ∷ Text,
    number   ∷ Integer,
    chapters ∷ [Chapter]
} deriving (Generic, Show)


instance ToJSON Title
