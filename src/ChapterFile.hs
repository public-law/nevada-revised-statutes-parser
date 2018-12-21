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
                                                , parseSectionsFromHeadingGroup
                                                , isSimpleSubChapter
                                                )
import           Models.Chapter                as Chapter
import           Models.SubChapter             as SubChapter
import           Models.SubSubChapter          as SubSubChapter
import           TextUtil                       ( normalizeWhiteSpace
                                                , titleize
                                                )


type ChapterMap = HashMap RelativePath Html
type TagList    = [Tag Text]

newtype SubChapterTOC = MakeSubChapterGroup TagList
newtype SubSubChapterTOC = MakeSubSubChapterGroup TagList


leadlineP :: String
leadlineP = "<p class=COLeadline>"

heading4P :: String
heading4P = "<p class=COHead4>"


-- TODO: Refactor. Change this method by creating an EmptyChapter type.
fillInEmptyChapter :: ChapterMap -> Chapter -> Chapter
fillInEmptyChapter chapterMap emptyChapter =
  let key       = chapterNumberToFilename (Chapter.number emptyChapter)
      maybeHtml = HM.lookup key chapterMap
  in  if Chapter.number emptyChapter `notElem` chaptersToSkip
        then case maybeHtml of
          Just html -> case parseChapter html of
            Right aChapter -> aChapter
            Left  message  -> error [qq| $message in $key |]
          Nothing -> error [qq| Chapter $key not found |]
        else emptyChapter


chapterNumberToFilename :: Text -> RelativePath
chapterNumberToFilename chapterNumber =
  let format | isAlpha (T.last chapterNumber) = "%04s"
             | otherwise                      = "%03s"
  in  toRelativePath $ "NRS-" ++ printf format chapterNumber ++ ".html"


parseChapter :: Html -> Either String Chapter
parseChapter chapterHtml = do
  let fullPage             = parseTags $ toText chapterHtml
  let rawTitle             = titleText fullPage
  let (rawNumber, rawName) = parseChapterFileTitle rawTitle
  sectionsOrSubChapters <- chapterContent fullPage
  return Chapter
    { Chapter.name    = rawName
    , Chapter.number  = rawNumber
    , Chapter.url     = chapterUrlPrefix ++ rawNumber ++ ".html"
    , Chapter.content = sectionsOrSubChapters
    }


-- TODO: What is a "heading group"?
chapterContent :: TagList -> Either String ChapterContent
chapterContent fullPage = do
  let groups = headingGroups fullPage
  foundSubChapters <- mapM (newSubChapter fullPage) groups
  foundSections    <- parseSectionsFromJustHtml fullPage
  return $ case foundSubChapters of
    [] -> SimpleChapterContent foundSections
    xs -> ComplexChapterContent xs


newSubChapter :: TagList -> TagList -> Either String SubChapter
newSubChapter fullPage headingGroup = do
  scs <- parseSectionsFromHeadingGroup fullPage headingGroup
  ssc <- parseSubSubChapters fullPage headingGroup
  let name' = subChapterNameFromGroup headingGroup
  let children' = if isSimpleSubChapter headingGroup
        then SubChapterSections scs
        else SubSubChapters ssc
  return SubChapter { SubChapter.name = name', SubChapter.children = children' }


parseSubSubChapters :: TagList -> TagList -> Either String [SubSubChapter]
parseSubSubChapters fullPage headingGroup =
  let parser = parseSubSubChapter fullPage
      groups = subSubChapterHeadingGroups headingGroup
  in  mapM parser groups


parseSubSubChapter :: TagList -> TagList -> Either String SubSubChapter
parseSubSubChapter fullPage subSubChapterHeadingGroup = do
  let name' = extractSubSubChapterName subSubChapterHeadingGroup
  sections' <- parseSectionsFromHeadingGroup fullPage subSubChapterHeadingGroup
  return SubSubChapter
    { SubSubChapter.name     = name'
    , SubSubChapter.sections = sections'
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


-- A Heading Group is a Sub Chapter heading with all of its following
-- content.
headingGroups :: TagList -> [TagList]
headingGroups fullPage =
  partitions (~== ("<p class=COHead2>" :: String)) fullPage


subSubChapterHeadingGroups :: TagList -> [TagList]
subSubChapterHeadingGroups headingGroup =
  (partitions (~== heading4P) headingGroup)


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
