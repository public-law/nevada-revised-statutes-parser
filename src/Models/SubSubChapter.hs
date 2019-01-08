module Models.SubSubChapter where

import           BasicPrelude
import           Data.Aeson                     ( ToJSON )
import           GHC.Generics                   ( Generic )
import           Models.Section

data SubSubChapter = SubSubChapter {
                       name     :: Text,
                       sections :: [Section] }
                   deriving (Generic, Show)

instance ToJSON SubSubChapter
