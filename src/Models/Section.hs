module Models.Section where

import           BasicPrelude
import           Data.Aeson                     ( ToJSON )
import           GHC.Generics                   ( Generic )
import qualified Data.Text                     as T

import           HtmlUtil                       ( Html )

data Section =
  Section {
    name   :: SectionName,
    number :: SectionNumber,
    body   :: Html
} deriving (Generic, Show)

instance ToJSON Section


newtype SectionName = MakeSectionName Text deriving ( Generic, Eq )
instance ToJSON SectionName
instance Show SectionName where
  show (MakeSectionName n) = T.unpack n

toSectionName :: Text -> SectionName
toSectionName n
  | T.length n > 70 || T.length n == 0
  = error
    $  "Name must be 1...70 characters ("
    ++ show (T.length n)
    ++ "): "
    ++ show n
  | otherwise
  = MakeSectionName n



newtype SectionNumber = MakeSectionNumber Text deriving ( Generic, Eq )
instance ToJSON SectionNumber
instance Show SectionNumber where
  show (MakeSectionNumber n) = T.unpack n

toSectionNumber :: Text -> SectionNumber
toSectionNumber n
  | T.length n > 8 || T.length n == 0
  = error
    $  "Number must be 1...8 characters ("
    ++ show (T.length n)
    ++ "): "
    ++ show n
  | otherwise
  = MakeSectionNumber n
