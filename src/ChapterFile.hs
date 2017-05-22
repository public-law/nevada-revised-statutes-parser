module ChapterFile where

import           BasicPrelude
import qualified Data.Attoparsec.Text    (parseOnly, Parser, takeText, takeWhile)
import           Data.Char               (isSpace)
import           Data.Text               hiding (dropWhile, null, takeWhile)
import           Text.HTML.TagSoup
import           Text.Parser.Char

import           HtmlUtil                (titleText)
import           TextUtil                (fixUnicodeChars, normalizeWhiteSpace, titleize)
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
    chapterUrl    =  (pack "https://www.leg.state.nv.us/nrs/NRS-") ++ number ++ (pack ".html"),
    subChapters   = subChaps
  }


newSubChapter :: [Tag Text] -> SubChapter
newSubChapter headingGroup =
  SubChapter {
    subChapterName     = subChapterNameFromGroup headingGroup,
    subChapterChildren = children
  }
  where children = if isSimpleSubChapter headingGroup
                     then SubChapterSections $ parseSectionsFromHeadingGroup headingGroup
                     else SubSubChapters     $ parseSubSubChapters headingGroup


parseSectionsFromHeadingGroup :: [Tag Text] -> [Section]
parseSectionsFromHeadingGroup headingGroup =
  fmap parseSectionFromHeadingParagraph (partitions (~== ("<p class=COLeadline>" :: String)) headingGroup)


parseSectionFromHeadingParagraph :: [Tag Text] -> Section
parseSectionFromHeadingParagraph paragraph =
  Section {
    sectionName   = name,
    sectionNumber = number
  }
  where
    name   = fixUnicodeChars $ normalizeWhiteSpace $ innerText $ dropWhile (~/= ("</a>" :: String)) paragraph
    number = (!! 1) $ words  $ normalizeWhiteSpace $ innerText $ takeWhile (~/= ("</a>" :: String)) paragraph


parseSubSubChapters :: [Tag Text] -> [SubSubChapter]
parseSubSubChapters headingGroup =
  fmap parseSubSubChapter (subSubChapterHeadingGroups headingGroup)


subSubChapterHeadingGroups :: [Tag Text] -> [[Tag Text]]
subSubChapterHeadingGroups headingGroup =
  (partitions (~== ("<p class=COHead4>" :: String)) headingGroup)


parseSubSubChapter :: [Tag Text] -> SubSubChapter
parseSubSubChapter subSubChapterHeadingGroup =
  SubSubChapter {
    subSubChapterName     = name,
    subSubChapterSections = parseSectionsFromHeadingGroup subSubChapterHeadingGroup
  }
  where
    name = (normalizeWhiteSpace . (!!0) . lines . innerText) subSubChapterHeadingGroup


subchapterNames :: [Tag Text] -> [Text]
subchapterNames tags =
  fmap subChapterNameFromGroup (headingGroups tags)


subChapterNameFromGroup :: [Tag Text] -> Text
subChapterNameFromGroup = 
  titleize . fromTagText . (!! 1)


sectionNamesFromGroup :: [Tag Text] -> [Text]
sectionNamesFromGroup headingGroup =
  fmap sectionNameFromParagraph (partitions (~== ("<p class=COLeadline>" :: String)) headingGroup)


sectionNameFromParagraph :: [Tag Text] -> Text
sectionNameFromParagraph = 
  fixUnicodeChars . normalizeWhiteSpace . strip . innerText . (dropWhile (~/= ("</a>" :: String)))


headingGroups :: [Tag Text] -> [[Tag Text]]
headingGroups tags = 
  partitions (~== ("<p class=COHead2>" :: String)) tags


-- Input:  "NRS: CHAPTER 432B - PROTECTION OF CHILDREN FROM ABUSE AND NEGLECT"
-- Output: ("432B", "Protection of Children from Abuse and Neglect")
parseChapterFileTitle :: Text -> (Text, Text)
parseChapterFileTitle input =
  case (Data.Attoparsec.Text.parseOnly chapterTitleParser input) of
    Left e  -> error e
    Right b -> b
        

-- Input:  "NRS: CHAPTER 432B - PROTECTION OF CHILDREN FROM ABUSE AND NEGLECT"
-- Output: ("432B", "Protection of Children from Abuse and Neglect")
chapterTitleParser :: Data.Attoparsec.Text.Parser (Text, Text)
chapterTitleParser = do
  _      <- string "NRS: CHAPTER "
  number <- Data.Attoparsec.Text.takeWhile (not . isSpace)
  _      <- string " - "
  title  <- Data.Attoparsec.Text.takeText
  return $ (number, titleize title)


isSimpleSubChapter :: [Tag Text] -> Bool
isSimpleSubChapter headingGroup =
  null (partitions (~== ("<p class=COHead4>" :: String)) headingGroup)
