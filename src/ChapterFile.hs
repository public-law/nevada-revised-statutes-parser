{-# LANGUAGE OverloadedStrings #-}

module ChapterFile where

import           BasicPrelude            hiding (takeWhile)
import           Data.Attoparsec.Text    (parseOnly, Parser, takeText, takeWhile)
import           Data.Char               (isSpace)
import           Data.Text               (replace, strip)
import           Text.HTML.TagSoup
import           Text.Parser.Char

import           HtmlUtil                (titleText)
import           TextUtil                (normalizeWhiteSpace, titleize)
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
    subChapterName     = subChapterNameFromGroup headingGroup,
    subChapterChildren = children
  }
  where children = if isSimpleSubChapter headingGroup
                     then Sections $ map (\n ->  Section {sectionName = n}) (sectionNamesFromGroup headingGroup)
                     else SubSubChapters $ parseSubSubChapters headingGroup


parseSubSubChapters :: [Tag Text] -> [SubSubChapter]
parseSubSubChapters headingGroup =
  map ( (\n -> SubSubChapter { subSubChapterName = n }) . normalizeWhiteSpace . (!!0) . lines . innerText) (partitions (~== ("<p class=COHead4>" :: String)) headingGroup)


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
sectionNameFromParagraph = 
  fixUnicodeChars . normalizeWhiteSpace . strip . innerText . (dropWhile (~/= ("</a>" :: String)))


fixUnicodeChars :: Text -> Text
fixUnicodeChars = (replace "\147" "\8220") . (replace "\148" "\8221")


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


isSimpleSubChapter :: [Tag Text] -> Bool
isSimpleSubChapter headingGroup =
  null (partitions (~== ("<p class=COHead4>" :: String)) headingGroup)
