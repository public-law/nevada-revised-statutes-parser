{-# LANGUAGE OverloadedStrings #-}

module NvStatutes where

import           BasicPrelude            hiding (takeWhile)
import           Data.Attoparsec.Text    (parseOnly, Parser, takeText, takeWhile)
import           Data.Char               (isSpace)
import           Data.Function           ((&))
import           Data.List.Split         (chunksOf, split, whenElt)
import           Data.Text               (strip)
import           Text.HTML.TagSoup       (Tag, fromAttrib, innerText, parseTags, partitions, fromTagText, (~==))
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token

import           HtmlUtil                (findFirst, findAll, titleText)
import           TextUtil                (titleize)
import           Models


type Html = Text

parseChapter :: Html -> Chapter
parseChapter chapterHtml =
  let tags           = parseTags chapterHtml
      rawTitle       = titleText tags
      (number, name) = parseChapterFileTitle rawTitle
      subChaps       = map newSubChapter (subchapterNames tags)
  in Chapter {
    chapterName   = name,
    chapterNumber = number,
    chapterUrl    = "https://www.leg.state.nv.us/nrs/NRS-" ++ number ++ ".html",
    subChapters   = subChaps
  }


newSubChapter :: Text -> SubChapter
newSubChapter name =
  SubChapter {
    subChapterName = name
  }


subchapterNames :: [Tag Text] -> [Text]
subchapterNames tags =
  let headingGroups = partitions (~== ("<p class=COHead2>" :: String)) tags
      names         = map ( titleize . fromTagText . (!! 1)) headingGroups
  in  names


titles :: Html → [Title]
titles indexHtml =
  let rows   = contentRows indexHtml
      tuples = rowTuples rows
  in fmap newTitle tuples


contentRows :: Html → [[Tag Text]]
contentRows indexHtml =
  let tags       = parseTags indexHtml
      table      = findFirst "<table class=MsoNormalTable" tags
  in findAll "<tr>" table


newTitle :: [[[Tag Text]]] -> Title
newTitle tuple =
  let titleRow       = head (head tuple)
      chapterRows    = head $ tail tuple
      parsedChapters = fmap newChapter chapterRows
      title          = innerText $ findFirst "<b>" titleRow
      (number, name) = parseRawTitle title
  in Title { titleName = titleize name, titleNumber = number, chapters = parsedChapters }


newChapter :: [Tag Text] → Chapter
newChapter row =
  Chapter {
    chapterName   = name,
    chapterNumber = number,
    chapterUrl    = url,
    subChapters   = []
  }
  where columns = findAll "<td>" row
        number  = head columns & innerText & strip & words & last
        name    = last columns & innerText & strip
        url     = findFirst "<a>" row & head & fromAttrib "href"


-- Input:  "TITLE\n  1 — STATE JUDICIAL DEPARTMENT\n  \n \n "
-- Output: (1, "STATE JUDICIAL DEPARTMENT")
parseRawTitle :: Text -> (Integer, Text)
parseRawTitle input =
  let f = parseOnly p input
      p = (,) <$>
        (textSymbol "TITLE" *> integer) <*>
        (textSymbol "—" *> (fromString <$> many (notChar '\n')))
  in case f of
    Left e  -> error e
    Right b -> b


-- Input:  "NRS: CHAPTER 432B - PROTECTION OF CHILDREN FROM ABUSE AND NEGLECT"
-- Output: ("432B", "Protection of Children from Abuse and Neglect")
parseChapterFileTitle :: Text -> (Text, Text)
parseChapterFileTitle input =
  case (parseOnly chapterTitleParser input) of
    Left e  -> error e
    Right b -> b
        

-- Input:  "NRS: CHAPTER 432B - PROTECTION OF CHILDREN FROM ABUSE AND NEGLECT"
-- Output: ("432B", "Protection of Children from Abuse and Neglect")
chapterTitleParser :: Parser (Text, Text)
chapterTitleParser = do
  _      <- string "NRS: CHAPTER "
  number <- takeWhile (not . isSpace)
  _      <- string " - "
  title  <- takeText
  return $ (number, titleize title)


rowTuples :: [[Tag Text]] → [[[[Tag Text]]]]
rowTuples rows =
  split (whenElt isTitleRow) rows
    & tail
    & chunksOf 2


isTitleRow :: [Tag Text] → Bool
isTitleRow html =
  length (findAll "<td>" html) == 1
