module ParseReport
  ( ParseSummary(..)
  , summarize
  , renderStdout
  , renderStderr
  , renderParseResult
  )
where

import           BasicPrelude
import           Data.Aeson                     ( ToJSON
                                                , object
                                                , (.=)
                                                )
import qualified Data.Aeson                     as Aeson
import qualified Data.Aeson.Encode.Pretty      as Aeson
                                                ( encodePretty )
import qualified Data.ByteString.Lazy          as BL
import           GHC.Generics                   ( Generic )

import           Models.Chapter                as Chapter
import           Models.NRS                     ( NRS(..) )
import           Models.ParseFailure            ( ParseFailure(..) )
import           Models.Section                 ( Section )
import           Models.SubChapter             as SubChapter
import           Models.SubSubChapter          as SubSubChapter
import           Models.Title                  as Title
import           Models.Tree                    ( Tree(..) )


data ParseSummary = ParseSummary
  { chaptersAttempted  :: Int
  , chaptersSucceeded  :: Int
  , chaptersFailed     :: Int
  , sectionsAttempted  :: Int
  , sectionsSucceeded  :: Int
  , sectionsFailed     :: Int
  , sectionFailureRate :: Double
  , chapterFailureRate :: Double
  }
  deriving (Eq, Generic, Show)

instance ToJSON ParseSummary where
  toJSON s = object
    [ "kind"               .= ("summary" :: Text)
    , "chaptersAttempted"  .= chaptersAttempted s
    , "chaptersSucceeded"  .= chaptersSucceeded s
    , "chaptersFailed"     .= chaptersFailed s
    , "sectionsAttempted"  .= sectionsAttempted s
    , "sectionsSucceeded"  .= sectionsSucceeded s
    , "sectionsFailed"     .= sectionsFailed s
    , "sectionFailureRate" .= sectionFailureRate s
    , "chapterFailureRate" .= chapterFailureRate s
    ]


summarize :: NRS -> [ParseFailure] -> ParseSummary
summarize nrs fails =
  let chapters'          = allChapters (statuteTree nrs)
      chaptersAttempted' = length chapters'
      chapterLevelFails  =
        filter (\f -> isJust (chapterNumber f) && isNothing (sectionNumber f)) fails
      failedChapterNums  = nubOrd (mapMaybe chapterNumber chapterLevelFails)
      chaptersFailed'    =
        length $ filter (\ch -> Chapter.number ch `elem` failedChapterNums) chapters'
      chaptersSucceeded' = chaptersAttempted' - chaptersFailed'
      sectionsSucceeded' = length (concatMap chapterSections chapters')
      sectionsFailed'    = length (filter (isJust . sectionNumber) fails)
      sectionsAttempted' = sectionsSucceeded' + sectionsFailed'
  in  ParseSummary
        { chaptersAttempted  = chaptersAttempted'
        , chaptersSucceeded  = chaptersSucceeded'
        , chaptersFailed     = chaptersFailed'
        , sectionsAttempted  = sectionsAttempted'
        , sectionsSucceeded  = sectionsSucceeded'
        , sectionsFailed     = sectionsFailed'
        , sectionFailureRate = rate sectionsFailed' sectionsAttempted'
        , chapterFailureRate = rate chaptersFailed' chaptersAttempted'
        }


renderStdout :: NRS -> BL.ByteString
renderStdout = Aeson.encodePretty


renderStderr :: [ParseFailure] -> ParseSummary -> BL.ByteString
renderStderr fails summary =
  let failureLines = map (Aeson.encode . Aeson.toJSON) fails
      summaryLine  = Aeson.encode (Aeson.toJSON summary)
  in  BL.intercalate "\n" (failureLines ++ [summaryLine]) <> "\n"


renderParseResult :: NRS -> [ParseFailure] -> (BL.ByteString, BL.ByteString)
renderParseResult nrs fails =
  let summary = summarize nrs fails
  in  (renderStdout nrs, renderStderr fails summary)


allChapters :: Tree -> [Chapter]
allChapters tree =
  chapter0 tree : concatMap Title.chapters (titles tree)


chapterSections :: Chapter -> [Section]
chapterSections ch = case Chapter.content ch of
  SimpleChapterContent sections -> sections
  ComplexChapterContent subs    -> concatMap subChapterSections subs


subChapterSections :: SubChapter -> [Section]
subChapterSections sc = case SubChapter.children sc of
  SubChapterSections sections -> sections
  SubSubChapters subsubs      -> concatMap SubSubChapter.sections subsubs


rate :: Int -> Int -> Double
rate _ 0 = 0
rate n d = fromIntegral n / fromIntegral d


nubOrd :: Eq a => [a] -> [a]
nubOrd = go []
  where
    go acc [] = reverse acc
    go acc (x : xs)
      | x `elem` acc = go acc xs
      | otherwise    = go (x : acc) xs
