module Models.Chapter where

import           BasicPrelude
import           Data.Aeson                     ( ToJSON )
import           GHC.Generics                   ( Generic )

import           Models.SubChapter
import           Models.Section


data Chapter = SimpleChapter {
                name    :: Text,
                number  :: Text,
                url     :: Text,
                sections :: [Section] }
             | ComplexChapter {
                name    :: Text,
                number  :: Text,
                url     :: Text,
                subChapters :: [SubChapter] }
             deriving (Generic, Show)

instance ToJSON Chapter
