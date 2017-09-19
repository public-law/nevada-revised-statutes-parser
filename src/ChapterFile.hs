module ChapterFile where

import           BasicPrelude
import qualified Data.Attoparsec.Text (Parser, parseOnly, takeText, takeWhile)
import           Data.Char            (isSpace)
import qualified Data.Text            as T
import           Text.HTML.TagSoup
import           Text.Parser.Char

import           HtmlUtil             (shaveBackTagsToLastClosingP, titleText)
import           Models
import           TextUtil             (normalizeWhiteSpace, normalizedInnerText,
                                       titleize)


type Html = Text

chapterUrlPrefix :: Text
chapterUrlPrefix = T.pack "https://www.leg.state.nv.us/nrs/NRS-"


--
-- TODO: Any way to shorten this file?
--

parseChapter :: Html -> Chapter
parseChapter chapterHtml =
  Chapter {
    chapterName   = name,
    chapterNumber = number,
    chapterUrl    = chapterUrlPrefix ++ number ++ (T.pack ".html"),
    subChapters   = subChaps
  }
  where tags           = parseTags chapterHtml
        rawTitle       = titleText tags
        (number, name) = parseChapterFileTitle rawTitle
        subChaps       = fmap (newSubChapter tags) (headingGroups tags)


newSubChapter :: [Tag Text] -> [Tag Text] -> SubChapter
newSubChapter dom headingGroup =
  SubChapter {
    subChapterName     = subChapterNameFromGroup headingGroup,
    subChapterChildren = children
  }
  where children = if isSimpleSubChapter headingGroup
                     then SubChapterSections $ parseSectionsFromHeadingGroup dom headingGroup
                     else SubSubChapters     $ parseSubSubChapters dom headingGroup


parseSectionsFromHeadingGroup :: [Tag Text] -> [Tag Text] -> [Section]
parseSectionsFromHeadingGroup dom headingGroup =
  fmap (parseSectionFromHeadingParagraph dom) (headingParagraphsWithContent headingGroup)


-- Some COLeadline P's have no content; they're just used for vertical spacing.
headingParagraphsWithContent :: [Tag Text] -> [[Tag Text]]
headingParagraphsWithContent headingGroup = filter (\tags -> length tags > 4) (partitions (~== "<p class=COLeadline>") headingGroup)


parseSectionFromHeadingParagraph :: [Tag Text] -> [Tag Text] -> Section
parseSectionFromHeadingParagraph dom paragraph =
  Section {
    sectionName   = name,
    sectionNumber = number,
    sectionBody   = body
  }
  where
    name   = normalizedInnerText $ dropWhile (~/= "</a>") paragraph
    number = parseNumberFromRawNumberText (normalizedInnerText $ takeWhile (~/= "</a>") paragraph) (renderTags paragraph)
    body   = parseSectionBody number dom


parseNumberFromRawNumberText :: Text -> Text -> Text
parseNumberFromRawNumberText numberText name =
  case words numberText of
    (_:x:_) -> x
    _       -> error ("Expected section \"" ++ (T.unpack name) ++ "\" raw number \"" ++ (T.unpack numberText) ++ "\" to have at least two words")


parseSubSubChapters :: [Tag Text] ->[Tag Text] -> [SubSubChapter]
parseSubSubChapters dom headingGroup =
  fmap (parseSubSubChapter dom) (subSubChapterHeadingGroups headingGroup)


subSubChapterHeadingGroups :: [Tag Text] -> [[Tag Text]]
subSubChapterHeadingGroups headingGroup =
  (partitions (~== "<p class=COHead4>") headingGroup)


parseSubSubChapter :: [Tag Text] ->[Tag Text] -> SubSubChapter
parseSubSubChapter dom subSubChapterHeadingGroup =
  SubSubChapter {
    subSubChapterName     = name,
    subSubChapterSections = parseSectionsFromHeadingGroup dom subSubChapterHeadingGroup
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
  fmap sectionNameFromParagraph (partitions (~== "<p class=COLeadline>") headingGroup)


sectionNameFromParagraph :: [Tag Text] -> Text
sectionNameFromParagraph =
  normalizedInnerText . (dropWhile (~/= "</a>"))


headingGroups :: [Tag Text] -> [[Tag Text]]
headingGroups tags =
  partitions (~== "<p class=COHead2>") tags


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
  null (partitions (~== "<p class=COHead4>") headingGroup)


parseSectionBody :: Text -> [Tag Text] -> Text
parseSectionBody number dom =
  sectionText
  where sectionGroups   = partitions (~== "<span class=Section") dom
        rawSectionGroup = shaveBackTagsToLastClosingP $ (!! 0) $ filter (isSectionBodyNumber number) sectionGroups
        sectionText     = normalizeWhiteSpace $ T.pack "<p class=SectBody>" ++ (renderTags rawSectionGroup)


isSectionBodyNumber :: Text -> [Tag Text] -> Bool
isSectionBodyNumber number dom =
  parseSectionBodyNumber dom == number


parseSectionBodyNumber :: [Tag Text] -> Text
parseSectionBodyNumber dom =
  innerText $ takeWhile (~/= "</span>") dom
