{-# LANGUAGE OverloadedStrings #-}

module ChapterFile where

import           BasicPrelude            hiding (takeWhile)
import           Data.Attoparsec.Text    (parseOnly, Parser, takeText, takeWhile)
import           Data.Char               (isSpace)
import           Text.HTML.TagSoup       (innerText, Tag, parseTags, partitions, fromTagText, (~==))
import           Text.Parser.Char

import           HtmlUtil                (titleText)
import           TextUtil                (titleize)
import           Models


type Html = Text

parseChapter :: Html -> Chapter
parseChapter chapterHtml =
  let tags           = parseTags chapterHtml
      rawTitle       = titleText tags
      (number, name) = parseChapterFileTitle rawTitle
      subChaps       = fmap newSubChapter (headingGroups tags)
  in Chapter {
    chapterName   = name,
    chapterNumber = number,
    chapterUrl    = "https://www.leg.state.nv.us/nrs/NRS-" ++ number ++ ".html",
    subChapters   = subChaps
  }


newSubChapter :: [Tag Text] -> SubChapter
newSubChapter headingGroup =
  SubChapter {
    subChapterName = subChapterNameFromGroup headingGroup,
    subChapterChildren = Sections $ map (\n ->  Section {sectionName = n}) (sectionNamesFromGroup headingGroup)
  }


subchapterNames :: [Tag Text] -> [Text]
subchapterNames tags =
    map subChapterNameFromGroup (headingGroups tags)


subChapterNameFromGroup :: [Tag Text] -> Text
subChapterNameFromGroup = 
    titleize . fromTagText . (!! 1)


sectionNamesFromGroup :: [Tag Text] -> [Text]
sectionNamesFromGroup headingGroup =
    map sectionNameFromParagraph (partitions (~== ("<p class=COLeadline>" :: String)) headingGroup)


sectionNameFromParagraph :: [Tag Text] -> Text
sectionNameFromParagraph ptags = (titleize . innerText) ptags


headingGroups :: [Tag Text] -> [[Tag Text]]
headingGroups tags = 
    partitions (~== ("<p class=COHead2>" :: String)) tags


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


