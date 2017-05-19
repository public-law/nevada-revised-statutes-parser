{-# LANGUAGE OverloadedStrings #-}

module ChapterFile where

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
      subChaps       = map (\n -> SubChapter { subChapterName = n }) (subchapterNames tags)
  in Chapter {
    chapterName   = name,
    chapterNumber = number,
    chapterUrl    = "https://www.leg.state.nv.us/nrs/NRS-" ++ number ++ ".html",
    subChapters   = subChaps
  }


-- newSubChapter :: Text -> SubChapter
-- newSubChapter name =
--   SubChapter {
--     subChapterName = name
--   }


subchapterNames :: [Tag Text] -> [Text]
subchapterNames tags =
  let headingGroups = partitions (~== ("<p class=COHead2>" :: String)) tags
      names         = map ( titleize . fromTagText . (!! 1)) headingGroups
  in  names


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


