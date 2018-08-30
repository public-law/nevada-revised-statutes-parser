{-# LANGUAGE DeriveGeneric #-}

module Models.NRS where

import           BasicPrelude (Show)
import           Data.Aeson   (ToJSON)
import           Data.Time    (Day)
import           GHC.Generics (Generic)

import           Models.Tree
import           Year


data NRS =
    NRS {
    statuteTree  ∷ Tree,
    nominalDate  ∷ Year,  -- The "date" of this edition
    dateAccessed ∷ Day
  } deriving (Generic, Show)

instance ToJSON NRS
