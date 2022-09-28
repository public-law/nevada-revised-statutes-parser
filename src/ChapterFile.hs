module ChapterFile where

import BasicPrelude
import Config
import qualified Data.Attoparsec.Text
  ( Parser,
    parseOnly,
    takeText,
    takeWhile,
  )
import Data.Char
  ( isAlpha,
    isSpace,
  )
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import FileUtil
  ( RelativePath,
    toRelativePath,
  )
import HtmlUtil
  ( Html,
    titleText,
    toText,
  )
import Models.Chapter as Chapter
import Models.SubChapter as SubChapter
import Models.SubSubChapter as SubSubChapter
import Parsing
  ( ChapterData (..),
    TagList,
  )
import qualified SimpleChapterFile
  ( isSimpleSubChapter,
    parseSectionsFromHeadingGroup,
    parseSectionsFromJustHtml,
  )
import Text.HTML.TagSoup
import Text.InterpolatedString.Perl6 (qq)
import Text.Parser.Char
import Text.Printf
import TextUtil
  ( normalizeWhiteSpace,
    titleize,
  )

type ChapterMap = HashMap RelativePath Html

-- TODO: Use these to refactor.
-- newtype SubChapterTOC = MakeSubChapterGroup TagList
-- newtype SubSubChapterTOC = MakeSubSubChapterGroup TagList

leadlineP :: String
leadlineP = "<p class=COLeadline>"

heading2P :: String
heading2P = "<p class=COHead2>"

heading4P :: String
heading4P = "<p class=COHead4>"

horizontalRule :: String
horizontalRule = "<p class=J-Dash"

-- TODO: Refactor. Change this method by creating an EmptyChapter type.
fillInEmptyChapter :: ChapterMap -> Chapter -> Chapter
fillInEmptyChapter chapterMap emptyChapter =
  let key = chapterNumberToFilename (Chapter.number emptyChapter)
      maybeHtml = HM.lookup key chapterMap
   in if Chapter.number emptyChapter `notElem` chaptersToSkip
        then case maybeHtml of
          Just html -> case parseChapter html of
            Right aChapter -> aChapter
            Left message -> error [qq| $message in $key |]
          Nothing -> error [qq| Chapter $key not found |]
        else emptyChapter

chapterNumberToFilename :: Text -> RelativePath
chapterNumberToFilename chapterNumber =
  let format
        | isAlpha (T.last chapterNumber) = "%04s"
        | otherwise = "%03s"
   in toRelativePath $ "NRS-" ++ printf format chapterNumber ++ ".html"

parseChapter :: Html -> Either String Chapter
parseChapter chapterHtml = do
  let chapterData = makeChapterData chapterHtml
  rawTitle <- titleText (headings chapterData)
  let (rawNumber, rawName) = parseChapterFileTitle rawTitle
  sectionsOrSubChapters <- chapterContent chapterData

  return
    Chapter
      { Chapter.name = rawName,
        Chapter.number = rawNumber,
        Chapter.url = chapterUrlPrefix ++ rawNumber ++ ".html",
        Chapter.content = sectionsOrSubChapters
      }

makeChapterData :: Html -> ChapterData
makeChapterData chapterHtml =
  let fullPage = parseTags $ toText chapterHtml
      topHalf' = topHalf fullPage
      bottomHalf' = bottomHalf fullPage
   in ChapterData
        { headings = topHalf',
          content = bottomHalf',
          sectionGroups = findSectionGroups bottomHalf'
        }

findSectionGroups :: TagList -> [TagList]
findSectionGroups = partitions (~== ("<span class=Section" :: String))

topHalf :: TagList -> TagList
topHalf = takeWhile (~/= horizontalRule)

bottomHalf :: TagList -> TagList
bottomHalf = dropWhile (~/= horizontalRule)

chapterContent :: ChapterData -> Either String ChapterContent
chapterContent chapterData = do
  let groups = subChapterHeadingGroups chapterData
  foundSubChapters <- mapM (newSubChapter chapterData) groups
  foundSections <- SimpleChapterFile.parseSectionsFromJustHtml chapterData
  return $ case foundSubChapters of
    [] -> SimpleChapterContent foundSections
    xs -> ComplexChapterContent xs

newSubChapter :: ChapterData -> TagList -> Either String SubChapter
newSubChapter chapterData headingGroup = do
  scs <-
    SimpleChapterFile.parseSectionsFromHeadingGroup
      chapterData
      headingGroup
  ssc <- parseSubSubChapters chapterData headingGroup
  let name' = subChapterNameFromGroup headingGroup
  let children' =
        if SimpleChapterFile.isSimpleSubChapter headingGroup
          then SubChapterSections scs
          else SubSubChapters ssc
  return SubChapter {SubChapter.name = name', SubChapter.children = children'}

parseSubSubChapters :: ChapterData -> TagList -> Either String [SubSubChapter]
parseSubSubChapters chapterData headingGroup =
  let parser = parseSubSubChapter chapterData
      groups = subSubChapterHeadingGroups headingGroup
   in mapM parser groups

parseSubSubChapter :: ChapterData -> TagList -> Either String SubSubChapter
parseSubSubChapter chapterData subSubChapterHeadingGroup = do
  let name' = extractSubSubChapterName subSubChapterHeadingGroup
  sections' <-
    SimpleChapterFile.parseSectionsFromHeadingGroup
      chapterData
      subSubChapterHeadingGroup
  return
    SubSubChapter
      { SubSubChapter.name = name',
        SubSubChapter.sections = sections'
      }

extractSubSubChapterName :: TagList -> Text
extractSubSubChapterName headingGroup =
  let linesOfText = lines $ innerText headingGroup
   in case linesOfText of
        (x : _) -> normalizeWhiteSpace x
        _ ->
          error [qq|Couldn't parse sub sub chapter name from: $headingGroup|]

subChapterNameFromGroup :: TagList -> Text
subChapterNameFromGroup (_ : y : _) = titleize $ fromTagText y
subChapterNameFromGroup tags =
  error [qq|Couldn't get a chapter name from the group: $tags|]

-- A Sub Chapter heading with all of its following content.
subChapterHeadingGroups :: ChapterData -> [TagList]
subChapterHeadingGroups chapterData =
  partitions (~== heading2P) (headings chapterData)

subSubChapterHeadingGroups :: TagList -> [TagList]
subSubChapterHeadingGroups = partitions (~== heading4P)

-- Input:  "NRS: CHAPTER 432B - PROTECTION OF CHILDREN FROM ABUSE AND NEGLECT"
--    or:  "NRS: PRELIMINARY CHAPTER - GENERAL PROVISIONS"
-- Output: ("432B", "Protection of Children from Abuse and Neglect")
parseChapterFileTitle :: Text -> (Text, Text)
parseChapterFileTitle input =
  if input == chapterZeroTitle
    then ("0", "Preliminary Chapter – General Provisions")
    else case Data.Attoparsec.Text.parseOnly chapterTitleParser input of
      Left e -> error [qq|Couldn't parse chapter file title $input $e|]
      Right b -> b

-- Input:  "NRS: CHAPTER 432B - PROTECTION OF CHILDREN FROM ABUSE AND NEGLECT"
-- Output: ("432B", "Protection of Children from Abuse and Neglect")
chapterTitleParser :: Data.Attoparsec.Text.Parser (Text, Text)
chapterTitleParser = do
  _ <- string "NRS: CHAPTER "
  num <- Data.Attoparsec.Text.takeWhile (not . isSpace)
  _ <- string " - "
  title <- Data.Attoparsec.Text.takeText
  return (num, normalizeWhiteSpace $ titleize title)
