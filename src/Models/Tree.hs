module Models.Tree where

import           BasicPrelude
import           Data.Aeson     (ToJSON)
import           GHC.Generics   (Generic)

import           Models.Chapter
import           Models.Title


data Tree =
    Tree {
    chapter0 ∷ Chapter,
    titles   ∷ [Title]
  } deriving (Generic, Show)

instance ToJSON Tree
