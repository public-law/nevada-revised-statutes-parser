{-# LANGUAGE DeriveGeneric #-}

module Models.Title where

import           BasicPrelude
import           Data.Aeson   (ToJSON)
import           Data.Time    (Day)
import           GHC.Generics (Generic)

import           Models.Chapter
import           Year



-- The top-level organizational unit in the Nevada Revised Statutes
data Title =
  Title {
    name   ∷ Text,
    number ∷ Integer,
    chapters    ∷ [Chapter]
} deriving (Generic, Show)


instance ToJSON Models.Title
