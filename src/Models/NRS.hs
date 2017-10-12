{-# LANGUAGE DeriveGeneric #-}

module Models.NRS where

import           BasicPrelude
import           Data.Aeson   (ToJSON)
import           Data.Time    (Day)
import           GHC.Generics (Generic)

import           Models.Tree
import           Year


data NRS =
    NRS {
    statuteTree  ∷ Tree,
    nominalDate  ∷ Year,
    dateAccessed ∷ Day
  } deriving (Generic, Show)

instance ToJSON NRS
