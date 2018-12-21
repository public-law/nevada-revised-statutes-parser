module Models.Section
  ( Section(Section, body, name, number)
  , SectionName
  , SectionNumber
  , SectionBody
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

maxNameLen :: Int
maxNameLen = 658

maxNumberLen :: Int
maxNumberLen = 11


newtype SectionName = MakeSectionName Text deriving ( Generic, Eq )
instance Show SectionName where
  show (MakeSectionName n) = T.unpack n

toSectionName :: Text -> Text -> Either String SectionName
toSectionName n context
  | actualLen > maxNameLen || actualLen == 0 = Left
    [qq| Section name must be 1...$maxNameLen chars ($actualLen): $parsedName. context: $context |]
  | otherwise = Right $ MakeSectionName parsedName
 where
  parsedName = parseName n
  actualLen  = T.length parsedName



newtype SectionNumber = MakeSectionNumber Text deriving ( Generic, Eq, IsString )
instance ToJSON SectionNumber
instance Show SectionNumber where
  show (MakeSectionNumber n) = T.unpack n

toSectionNumber :: Text -> Text -> Either String SectionNumber
toSectionNumber n context
  | actualLen > maxNumberLen || actualLen == 0 || not (T.isInfixOf "." n) = Left
    [qq| Section number must be 1..$maxNumberLen characters and have a dot: "$n"  context: $context |]
  | otherwise = Right $ MakeSectionNumber n
  where 
    actualLen = T.length n


newtype SectionBody = MakeSectionBody Html deriving ( Generic, Eq )
instance Show SectionBody where
  show (MakeSectionBody n) = T.unpack (toText n)

toSectionBody :: Html -> Text -> Either String SectionBody
toSectionBody n context
  | actualLen == 0 = Left [qq| Section body is blank. context: $context |]
  | otherwise      = Right $ MakeSectionBody n
  where actualLen = T.length $ toText n


parseName :: Text -> Text
parseName = normalizeWhiteSpace . removeAnnotation
  where removeAnnotation = T.takeWhile isLegalNameChar

  
isLegalNameChar :: Char -> Bool
isLegalNameChar c = c /= '['


instance ToJSON Section
instance ToJSON SectionBody
instance ToJSON SectionName
