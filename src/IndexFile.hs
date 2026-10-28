module IndexFile where

import           BasicPrelude
import           Data.Attoparsec.Text    (parseOnly)
import           Data.Function           ((&))
import           Data.List.Split         (chunksOf, split, whenElt)
import           Text.HTML.TagSoup       (fromAttrib, innerText, parseTags)
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token

import           ChapterFile             (ChapterMap, fillInEmptyChapter)
import           FileUtil                (toAbsoluteURL)
import           HtmlUtil
import           Models.Chapter          as Chapter
import           Models.Title            as Title
import           TextUtil                (normalizeWhiteSpace, titleize)



parseTitlesAndChapters :: Html -> ChapterMap -> [Title]
parseTitlesAndChapters indexHtml chapterMap =
  let emptyTitles = parseTitles indexHtml
   in case emptyTitles of
        Right ts -> map (fillInEmptyTitle chapterMap) ts
        Left s   -> error s


fillInEmptyTitle :: ChapterMap -> Title -> Title
fillInEmptyTitle chapterMap emptyTitle =
  Title
    { name = Title.name emptyTitle
    , number = Title.number emptyTitle
    , chapters = map (fillInEmptyChapter chapterMap) (chapters emptyTitle)
    }


parseTitles :: Html -> Either String [Title]
parseTitles indexHtml = do
  content_rows <- contentRows indexHtml
  mapM newTitle (rowTuples content_rows)


contentRows :: Html -> Either String [Node]
contentRows indexHtml = do
  let tags = parseTags $ toText indexHtml
  table <- findFirst "<table class=MsoNormalTable" tags
  return $ findAll "<tr>" table


newTitle :: [[Node]] -> Either String Title
newTitle [titleRows, chapterRows] = do
  titleRow <- requireHead "Empty title row group" titleRows
  first_b <- findFirst "<b>" titleRow
  let parsedChapters = fmap newChapter chapterRows
  let title = innerText first_b
  let (rawNumber, rawName) = parseRawTitle title
  return
    Title
      { Title.name = titleize rawName
      , Title.number = rawNumber
      , chapters = rights parsedChapters
      }
newTitle _ = Left "Expected a title row group and a chapter row group"


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
        fromAttrib "href" hrefTag &
        toAbsoluteURL "https://www.leg.state.nv.us/NRS/"
  return
    Chapter
      { Chapter.name = name'
      , Chapter.number = number'
      , Chapter.url = chapter_url
      , Chapter.content = ComplexChapterContent []
      }


-- Input:  "TITLE\n  1 - STATE JUDICIAL DEPARTMENT\n  \n \n "
-- Output: (1, "STATE JUDICIAL DEPARTMENT")
-- The separator in the HTML is a Unicode em dash (U+2014).
parseRawTitle :: Text -> (Integer, Text)
parseRawTitle input =
  let f = parseOnly p input
      p =
        (,) <$> (textSymbol "TITLE" *> integer) <*>
        (textSymbol "\x2014" *> (fromString <$> many anyChar))
   in case f of
        Left e  -> error e
        Right b -> b


rowTuples :: [Node] -> [[[Node]]]
rowTuples rows =
  case split (whenElt isTitleRow) rows of
    []       -> []
    (_:rest) -> chunksOf 2 rest


isTitleRow :: Node -> Bool
isTitleRow html = length (findAll "<td>" html) == 1


requireHead :: String -> [a] -> Either String a
requireHead _ (x:_) = Right x
requireHead msg []  = Left msg


requireLast :: String -> [a] -> Either String a
requireLast msg xs =
  case reverse xs of
    (x:_) -> Right x
    []    -> Left msg


requireNonEmpty :: String -> [a] -> Either String [a]
requireNonEmpty _ xs@(_:_) = Right xs
requireNonEmpty msg []     = Left msg
