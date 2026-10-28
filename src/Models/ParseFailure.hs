module Models.ParseFailure
  ( ParseFailure(..)
  , parseFailure
  , atTitle
  , atChapter
  , atSection
  , accumulate
  )
where

import           BasicPrelude
import           Data.Aeson                     ( ToJSON )
import           GHC.Generics                   ( Generic )


data ParseFailure = ParseFailure
  { sourceFile    :: Text
  , titleNumber   :: Maybe Integer
  , chapterNumber :: Maybe Text
  , sectionNumber :: Maybe Text
  , message       :: Text
  }
  deriving (Eq, Generic, Show)

instance ToJSON ParseFailure


accumulate
  :: (a -> Either String b)
  -> (a -> String -> ParseFailure)
  -> [a]
  -> ([b], [ParseFailure])
accumulate parse toFailure items =
  let (fails, oks) = partitionEithers (classify <$> items)
  in  (oks, fails)
  where
    classify a = case parse a of
      Right b -> Right b
      Left e  -> Left (toFailure a e)


parseFailure :: Text -> Text -> ParseFailure
parseFailure file msg = ParseFailure
  { sourceFile    = file
  , titleNumber   = Nothing
  , chapterNumber = Nothing
  , sectionNumber = Nothing
  , message       = msg
  }


atTitle :: Integer -> ParseFailure -> ParseFailure
atTitle n f = f { titleNumber = Just n }


atChapter :: Text -> ParseFailure -> ParseFailure
atChapter n f = f { chapterNumber = Just n }


atSection :: Text -> ParseFailure -> ParseFailure
atSection n f = f { sectionNumber = Just n }
