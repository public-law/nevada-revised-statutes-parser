module ChapterFile where

import           BasicPrelude
import qualified Data.Attoparsec.Text           ( Parser
                                                , parseOnly
                                                , takeText
                                                , takeWhile
                                                )
import           Data.Char                      ( isAlpha
                                                , isSpace
                                                )
import qualified Data.HashMap.Lazy             as HM
import qualified Data.Text                     as T
import           Text.HTML.TagSoup
import           Text.Parser.Char
import           Text.Printf
import           Text.InterpolatedString.Perl6  ( qq )

import           Config
import           FileUtil                       ( RelativePath
                                                , toRelativePath
                                                )
import           HtmlUtil                       ( Html
                                                , titleText
                                                , toText
                                                )
import           SimpleChapterFile              ( parseSectionsFromJustHtml
                                                , parseSectionFromHeadingParagraph
                                                , isSimpleSubChapter
                                                )
import           Models.Chapter                as Chapter
import           Models.Section                as Section
import           Models.SubChapter             as SubChapter
import           Models.SubSubChapter          as SubSubChapter
import           TextUtil                       ( normalizeWhiteSpace
                                                , titleize
                                                )


type ChapterMap = HashMap RelativePath Html
type TagList    = [Tag Text]

leadlineP :: String
leadlineP = "<p class=COLeadline>"

heading4P :: String
heading4P = "<p class=COHead4>"


fillInEmptyChapter :: ChapterMap -> Chapter -> Chapter
fillInEmptyChapter chapterMap emptyChapter =
  let key       = chapterNumberToFilename (Chapter.number emptyChapter)
      maybeHtml = HM.lookup key chapterMap
  in  if Chapter.number emptyChapter `notElem` chaptersToSkip
        then case maybeHtml of
          Just html -> parseChapter html
          Nothing   -> error $ "Chapter " ++ show key ++ " not found."
        else emptyChapter


chapterNumberToFilename :: Text -> RelativePath
chapterNumberToFilename chapterNumber =
  let format | isAlpha (T.last chapterNumber) = "%04s"
             | otherwise                      = "%03s"
  in  toRelativePath $ "NRS-" ++ printf format chapterNumber ++ ".html"


parseChapter :: Html -> Chapter
parseChapter chapterHtml = Chapter
  { Chapter.name    = rawName
  , Chapter.number  = rawNumber
  , Chapter.url     = chapterUrlPrefix ++ rawNumber ++ ".html"
  , Chapter.content = sectionsOrSubChapters
  }
 where
  fullPage              = parseTags $ toText chapterHtml
  rawTitle              = titleText fullPage
  (rawNumber, rawName)  = parseChapterFileTitle rawTitle
  sectionsOrSubChapters = chapterContent fullPage


chapterContent :: TagList -> ChapterContent
chapterContent fullPage = case foundSubChapters of
  [] -> SimpleChapterContent foundSections
  xs -> ComplexChapterContent xs
 where
  groups           = headingGroups fullPage
  foundSubChapters = fmap (newSubChapter fullPage) groups
  foundSections    = parseSectionsFromJustHtml fullPage


newSubChapter :: TagList -> TagList -> SubChapter
newSubChapter fullPage headingGroup = SubChapter
  { SubChapter.name = subChapterNameFromGroup headingGroup
  , children        = if isSimpleSubChapter headingGroup
    then SubChapterSections
      $ parseSectionsFromHeadingGroup fullPage headingGroup
    else SubSubChapters $ parseSubSubChapters fullPage headingGroup
  }


parseSectionsFromHeadingGroup :: TagList -> TagList -> [Section]
parseSectionsFromHeadingGroup fullPage headingGroup = fmap
  (parseSectionFromHeadingParagraph fullPage)
  (headingParagraphsWithContent headingGroup)


-- Some COLeadline P's have no content; they're just used for vertical spacing.
headingParagraphsWithContent :: TagList -> [TagList]
headingParagraphsWithContent headingGroup =
  filter (\tags -> length tags > 4) (partitions (~== leadlineP) headingGroup)


parseSubSubChapters :: TagList -> TagList -> [SubSubChapter]
parseSubSubChapters fullPage headingGroup =
  fmap (parseSubSubChapter fullPage) (subSubChapterHeadingGroups headingGroup)


subSubChapterHeadingGroups :: TagList -> [TagList]
subSubChapterHeadingGroups headingGroup =
  (partitions (~== heading4P) headingGroup)


parseSubSubChapter :: TagList -> TagList -> SubSubChapter
parseSubSubChapter fullPage subSubChapterHeadingGroup = SubSubChapter
  { SubSubChapter.name     = extractSubSubChapterName subSubChapterHeadingGroup
  , SubSubChapter.sections = parseSectionsFromHeadingGroup
    fullPage
    subSubChapterHeadingGroup
  }


extractSubSubChapterName :: TagList -> Text
extractSubSubChapterName headingGroup =
  let linesOfText = lines $ innerText headingGroup
  in  case linesOfText of
        (x : _) -> normalizeWhiteSpace x
        _ ->
          error [qq|Couldn't parse sub sub chapter name from: $headingGroup|]


subChapterNameFromGroup :: TagList -> Text
subChapterNameFromGroup (_ : y : _) = titleize $ fromTagText y
subChapterNameFromGroup tags =
  error [qq|Couldn't get a chapter name from the group: $tags|]


headingGroups :: TagList -> [TagList]
headingGroups fullPage =
  partitions (~== ("<p class=COHead2>" :: String)) fullPage


-- Input:  "NRS: CHAPTER 432B - PROTECTION OF CHILDREN FROM ABUSE AND NEGLECT"
--    or:  "NRS: PRELIMINARY CHAPTER - GENERAL PROVISIONS"
-- Output: ("432B", "Protection of Children from Abuse and Neglect")
parseChapterFileTitle :: Text -> (Text, Text)
parseChapterFileTitle input = if input == chapterZeroTitle
  then ("0", "Preliminary Chapter – General Provisions")
  else case (Data.Attoparsec.Text.parseOnly chapterTitleParser input) of
    Left  e -> error [qq|Couldn't parse chapter file title $input $e|]
    Right b -> b


-- Input:  "NRS: CHAPTER 432B - PROTECTION OF CHILDREN FROM ABUSE AND NEGLECT"
-- Output: ("432B", "Protection of Children from Abuse and Neglect")
chapterTitleParser :: Data.Attoparsec.Text.Parser (Text, Text)
chapterTitleParser = do
  _     <- string "NRS: CHAPTER "
  num   <- Data.Attoparsec.Text.takeWhile (not . isSpace)
  _     <- string " - "
  title <- Data.Attoparsec.Text.takeText
  return (num, titleize title)
