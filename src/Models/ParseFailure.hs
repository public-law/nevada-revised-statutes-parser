module Models.ParseFailure
  ( ParseFailure(..)
  , parseFailure
  , atTitle
  , atChapter
  , atSection
  , ParseResult
  , ParseResultT
  , accumulate
  )
where

import           BasicPrelude
import           Control.Monad.Writer.Strict    ( Writer
                                                , WriterT
                                                , tell
                                                )
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


-- Recoverable failures are collected in encounter order. The transformer also
-- lets structural parsers retain their existing Either String failure behavior.
type ParseResult = Writer [ParseFailure]
type ParseResultT m = WriterT [ParseFailure] m


accumulate
  :: Monad m
  => (a -> Either String b)
  -> (a -> String -> ParseFailure)
  -> [a]
  -> ParseResultT m [b]
accumulate parse toFailure items = catMaybes <$> mapM classify items
  where
    classify a = case parse a of
      Right b -> pure (Just b)
      Left e  -> tell [toFailure a e] >> pure Nothing


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
