module IndexFile where

import           BasicPrelude
import           Data.Attoparsec.Text           ( parseOnly )
import           Data.Function                  ( (&) )
import           Data.List.Split                ( chunksOf
                                                , split
                                                , whenElt
                                                )
import qualified Data.Text                     as T
import           Text.HTML.TagSoup              ( fromAttrib
                                                , innerText
                                                , parseTags
                                                )
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token

import           ChapterFile                    ( ChapterMap
                                                , fillInEmptyChapter
                                                )
import           FileUtil                       ( toAbsoluteURL )
import           HtmlUtil
import           Models.Chapter                as Chapter
import           Models.ParseFailure            ( ParseFailure
                                                , accumulate
                                                , atTitle
                                                , parseFailure
                                                )
import           Models.Title                  as Title
import           TextUtil                       ( normalizeWhiteSpace
                                                , titleize
                                                )


parseTitlesAndChapters :: Html -> ChapterMap -> ([Title], [ParseFailure])
parseTitlesAndChapters indexHtml chapterMap =
  case parseTitles indexHtml of
    Left message ->
      ( []
      , [parseFailure "index.html" (T.pack message)]
      )
    Right (emptyTitles, indexFails) ->
      let filled = fmap (fillInEmptyTitle chapterMap) emptyTitles
          (titles, nestedFails) = unzip filled
      in  (titles, indexFails ++ concat nestedFails)


fillInEmptyTitle :: ChapterMap -> Title -> (Title, [ParseFailure])
fillInEmptyTitle chapterMap emptyTitle =
  let results = fmap (fillInEmptyChapter chapterMap) (chapters emptyTitle)
      (filledChapters, nestedFails) = unzip results
      titleFails = fmap (atTitle (Title.number emptyTitle)) (concat nestedFails)
  in  ( emptyTitle { Title.chapters = filledChapters }
      , titleFails
      )


parseTitles :: Html -> Either String ([Title], [ParseFailure])
parseTitles indexHtml = do
  content_rows <- contentRows indexHtml
  results      <- mapM newTitle (rowTuples content_rows)
  let (titles, nestedFails) = unzip results
  return (titles, concat nestedFails)


contentRows :: Html -> Either String [Node]
contentRows indexHtml = do
  let tags = parseTags $ toText indexHtml
  table <- findFirst "<table class=MsoNormalTable" tags
  return $ findAll "<tr>" table


newTitle :: [[Node]] -> Either String (Title, [ParseFailure])
newTitle [titleRows, chapterRows] = do
  titleRow <- requireHead "Empty title row group" titleRows
  first_b  <- findFirst "<b>" titleRow
  let title = innerText first_b
  (rawNumber, rawName) <- parseRawTitle title
  let (parsedChapters, rowFails) =
        accumulate newChapter indexRowFailure chapterRows
      locatedFails = fmap (atTitle rawNumber) rowFails
  return
    ( Title
      { Title.name     = titleize rawName
      , Title.number   = rawNumber
      , Title.chapters = parsedChapters
      }
    , locatedFails
    )
newTitle _ = Left "Expected a title row group and a chapter row group"


indexRowFailure :: Node -> String -> ParseFailure
indexRowFailure _ err = parseFailure "index.html" (T.pack err)


newChapter :: Node -> Either String Chapter
newChapter row = do
  first_anchor <- findFirst "<a>" row
  columns <- requireNonEmpty "Chapter row has no columns" (findAll "<td>" row)
  nameCol <- requireLast "Chapter row missing name column" columns
  numberCol <- requireHead "Chapter row missing number column" columns
  numberWords <- requireNonEmpty "Chapter number cell is blank"
    (words $ normalizeWhiteSpace $ innerText numberCol)
  number' <- requireLast "Chapter number cell has no words" numberWords
  hrefTag <- requireHead "Empty chapter anchor" first_anchor
  let name' = normalizeWhiteSpace $ innerText nameCol
  let chapter_url =
        fromAttrib "href" hrefTag
          & toAbsoluteURL "https://www.leg.state.nv.us/NRS/"
  return Chapter
    { Chapter.name    = name'
    , Chapter.number  = number'
    , Chapter.url     = chapter_url
    , Chapter.content = ComplexChapterContent []
    }


-- Input:  "TITLE\n  1 - STATE JUDICIAL DEPARTMENT\n  \n \n "
-- Output: (1, "STATE JUDICIAL DEPARTMENT")
-- The separator in the HTML is Unicode U+2014.
parseRawTitle :: Text -> Either String (Integer, Text)
parseRawTitle input =
  let p =
        (,)
          <$> (textSymbol "TITLE" *> integer)
          <*> (textSymbol "\x2014" *> (fromString <$> many anyChar))
  in  case parseOnly p input of
        Left  e -> Left e
        Right b -> Right b


rowTuples :: [Node] -> [[[Node]]]
rowTuples rows = case split (whenElt isTitleRow) rows of
  []       -> []
  (_:rest) -> chunksOf 2 rest


isTitleRow :: Node -> Bool
isTitleRow html = length (findAll "<td>" html) == 1


requireHead :: String -> [a] -> Either String a
requireHead _ (x:_) = Right x
requireHead msg []  = Left msg


requireLast :: String -> [a] -> Either String a
requireLast msg xs = case reverse xs of
  (x:_) -> Right x
  []    -> Left msg


requireNonEmpty :: String -> [a] -> Either String [a]
requireNonEmpty _ xs@(_:_) = Right xs
requireNonEmpty msg []     = Left msg
