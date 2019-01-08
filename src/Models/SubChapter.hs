module Models.SubChapter where

import           BasicPrelude
import           Data.Aeson                     ( ToJSON )
import           GHC.Generics                   ( Generic )

import           Models.Section
import           Models.SubSubChapter


data SubChapter = SimpleSubChapter {
                    name :: Text,
                    sections :: [Section] }
                | ComplexSubChapter {
                    name :: Text,
                    subSubChapters :: [SubSubChapter] }
                deriving (Generic, Show)


instance ToJSON SubChapter
