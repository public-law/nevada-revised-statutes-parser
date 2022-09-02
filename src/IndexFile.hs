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
  rows <- contentRows indexHtml
  let tuples = rowTuples rows
  return $ rights $ fmap newTitle tuples

contentRows :: Html -> Either String [Node]
contentRows indexHtml = do
  let tags = parseTags $ toText indexHtml
  table <- findFirst "<table class=MsoNormalTable" tags
  return $ findAll "<tr>" table

newTitle :: [[Node]] -> Either String Title
newTitle tuple = do
  let titleRow = head (head tuple)
  first_b <- findFirst "<b>" titleRow
  let chapterRows = head $ tail tuple
  let parsedChapters = fmap newChapter chapterRows
  let title = innerText $ first_b
  let (rawNumber, rawName) = parseRawTitle title
  return
    Title
      { Title.name = titleize rawName
      , Title.number = rawNumber
      , chapters = rights parsedChapters
      }

newChapter :: Node -> Either String Chapter
newChapter row = do
  first_anchor <- findFirst "<a>" row
  let columns = findAll "<td>" row
  let name' = normalizeWhiteSpace $ innerText $ (last columns)
  let number' = head columns & innerText & normalizeWhiteSpace & words & last
  let chapter_url =
        first_anchor & head & fromAttrib "href" &
        toAbsoluteURL "https://www.leg.state.nv.us/NRS/"
  return
    Chapter
      { Chapter.name = name'
      , Chapter.number = number'
      , Chapter.url = chapter_url
      , Chapter.content = ComplexChapterContent []
      }

-- Input:  "TITLE\n  1 — STATE JUDICIAL DEPARTMENT\n  \n \n "
-- Output: (1, "STATE JUDICIAL DEPARTMENT")
parseRawTitle :: Text -> (Integer, Text)
parseRawTitle input =
  let f = parseOnly p input
      p =
        (,) <$> (textSymbol "TITLE" *> integer) <*>
        (textSymbol "—" *> (fromString <$> many anyChar))
   in case f of
        Left e  -> error e
        Right b -> b

rowTuples :: [Node] -> [[[Node]]]
rowTuples rows = split (whenElt isTitleRow) rows & tail & chunksOf 2

isTitleRow :: Node -> Bool
isTitleRow html = length (findAll "<td>" html) == 1
