module Models.Chapter where

import           BasicPrelude                   ( Show
                                                , Text
                                                )
import           Data.Aeson                     ( ToJSON )
import           GHC.Generics                   ( Generic )

import           Models.SubChapter
import           Models.Section


data Chapter =
  Chapter {
    name    :: Text,
    number  :: Text,
    url     :: Text,
    content :: ChapterContent
  } deriving (Generic, Show)


data ChapterContent = SimpleChapterContent [Section]
                    | ComplexChapterContent [SubChapter]
                    deriving (Generic, Show)

instance ToJSON ChapterContent
instance ToJSON Chapter
