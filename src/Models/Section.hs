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
import           Text.InterpolatedString.Perl6  ( qq )

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
  | actualLen > maxLen || actualLen == 0 = error
    [qq| Name must be 1...$maxLen chars ($actualLen): $parsedName |]
  | otherwise = MakeSectionName parsedName
 where
  maxLen     = 336
  parsedName = parseName n
  actualLen  = T.length parsedName



newtype SectionNumber = MakeSectionNumber Text deriving ( Generic, Eq, IsString )
instance ToJSON SectionNumber
instance Show SectionNumber where
  show (MakeSectionNumber n) = T.unpack n

toSectionNumber :: Text -> SectionNumber
toSectionNumber n
  | actualLen > 8 || actualLen == 0 = error
    [qq| Number must be 1...8 characters ($actualLen): $n |]
  | otherwise = MakeSectionNumber n
  where actualLen = T.length n


newtype SectionBody = MakeSectionBody Html deriving ( Generic, Eq )
instance ToJSON SectionBody
instance Show SectionBody where
  show (MakeSectionBody n) = T.unpack (toText n)

toSectionBody :: Html -> SectionBody
toSectionBody n
  | actualLen == 0 = error [qq| Body is blank |]
  | otherwise = MakeSectionBody n
  where actualLen = T.length $ toText n


parseName :: Text -> Text
parseName = normalizeWhiteSpace . removeAnnotation
  where removeAnnotation = T.takeWhile isLegalNameChar

  
isLegalNameChar :: Char -> Bool
isLegalNameChar c = c /= '['
