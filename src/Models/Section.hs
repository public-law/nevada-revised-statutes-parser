module Models.Section
  ( Section(Section, body, name, number)
  , toSectionName
  , toSectionNumber
  , toSectionBody
  , parseName
  )
where

import           BasicPrelude
import           Data.Aeson                     ( ToJSON )
import           GHC.Generics                   ( Generic )
import qualified Data.Text                     as T

import           HtmlUtil                       ( Html
                                                , toText
                                                )
import           TextUtil

data Section =
  Section {
    name   :: SectionName,
    number :: SectionNumber,
    body   :: SectionBody
} deriving (Generic, Show)

instance ToJSON Section


newtype SectionName = MakeSectionName Text deriving ( Generic, Eq )
instance ToJSON SectionName
instance Show SectionName where
  show (MakeSectionName n) = T.unpack n

toSectionName :: Text -> SectionName
toSectionName n
  | T.length parsedName > maxLen || T.length parsedName == 0
  = error
    $  "Name must be 1..."
    ++ show maxLen
    ++ " characters ("
    ++ show (T.length parsedName)
    ++ "): "
    ++ show parsedName
  | otherwise
  = MakeSectionName parsedName
 where
  maxLen     = 336
  parsedName = parseName n




newtype SectionNumber = MakeSectionNumber Text deriving ( Generic, Eq, IsString )
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


newtype SectionBody = MakeSectionBody Html deriving ( Generic, Eq )
instance ToJSON SectionBody
instance Show SectionBody where
  show (MakeSectionBody n) = T.unpack (toText n)

toSectionBody :: Html -> SectionBody
toSectionBody n
  | T.length (toText n) == 0
  = error
    $  "Body must be 1... characters ("
    ++ show (T.length $ toText n)
    ++ "): "
    ++ show n
  | otherwise
  = MakeSectionBody n


parseName :: Text -> Text
parseName = normalizeWhiteSpace . removeAnnotation
  where removeAnnotation = T.takeWhile isLegalNameChar

isLegalNameChar :: Char -> Bool
isLegalNameChar c = c /= '['
