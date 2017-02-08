{-# LANGUAGE DeriveGeneric #-}

module Models where

import           Data.Aeson   (ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)


data Title =
  Title {
    titleName   :: Text,
    titleNumber :: Int,
    chapters    :: [Chapter]
} deriving (Generic, Show)

data Chapter =
  Chapter {
    chapterName   :: Text,
    chapterNumber :: Text,
    url           :: Text
} deriving (Generic, Show)


instance ToJSON Title
instance ToJSON Chapter
