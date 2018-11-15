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


import           Config
import           FileUtil                       ( RelativePath
                                                , toRelativePath
                                                )
import qualified FileUtil                      as Util
import           HtmlUtil                       ( Html(NewHtml)
                                                , shaveBackTagsToLastClosingP
                                                , titleText
                                                , toText
                                                )
import           Models.Chapter                as Chapter
import           Models.Section                as Section
import           Models.SubChapter             as SubChapter
import           Models.SubSubChapter          as SubSubChapter
import           TextUtil                       ( normalizeWhiteSpace
                                                , normalizedInnerText
                                                , titleize
                                                )


--
-- TODO: Any way to shorten this file?
--


type ChapterMap = HashMap RelativePath Html
type TagList    = [Tag Text]

closingA :: String
closingA = "</a>"

closingP :: String
closingP = "</p>"

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


parseSectionsFromJustHtml :: TagList -> [Section]
parseSectionsFromJustHtml fullPage =
  parseSectionsFromHeadingGroup fullPage fullPage


parseSectionsFromHeadingGroup :: TagList -> TagList -> [Section]
parseSectionsFromHeadingGroup fullPage headingGroup = fmap
  (parseSectionFromHeadingParagraph fullPage)
  (headingParagraphsWithContent headingGroup)


-- Some COLeadline P's have no content; they're just used for vertical spacing.
headingParagraphsWithContent :: TagList -> [TagList]
headingParagraphsWithContent headingGroup =
  filter (\tags -> length tags > 4) (partitions (~== leadlineP) headingGroup)


parseSectionFromHeadingParagraph :: TagList -> TagList -> Section
parseSectionFromHeadingParagraph fullPage paragraph = Section
  { Section.name   = secName
  , Section.number = secNumber
  , Section.body   = secBody
  }
 where
  secName = normalizedInnerText $ takeWhile (~/= closingP) $ dropWhile
    (~/= closingA)
    paragraph
  rawNumberText = normalizedInnerText $ takeWhile (~/= closingA) paragraph
  secNumber     = parseNumberFromRawNumberText rawNumberText secName
  secBody       = parseSectionBody secNumber fullPage


parseNumberFromRawNumberText :: Text -> Text -> Text
parseNumberFromRawNumberText numberText secName = case words numberText of
  (_ : x : _) -> x
  _ ->
    Util.error
      $  "Expected section \""
      ++ secName
      ++ "\" raw number \""
      ++ numberText
      ++ "\" to have at least two words"


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
          error
            $  "Could not parse sub sub chapter name from: "
            ++ (show headingGroup)


subnames :: TagList -> [Text]
subnames tags = fmap subChapterNameFromGroup (headingGroups tags)


subChapterNameFromGroup :: TagList -> Text
subChapterNameFromGroup (_ : y : _) = titleize $ fromTagText y
subChapterNameFromGroup tags =
  error $ "Could not get a chapter name from the group: " ++ (show tags)


sectionNamesFromGroup :: TagList -> [Text]
sectionNamesFromGroup headingGroup =
  fmap sectionNameFromParagraph (partitions (~== leadlineP) headingGroup)


sectionNameFromParagraph :: TagList -> Text
sectionNameFromParagraph = normalizedInnerText . (dropWhile (~/= closingA))


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
    Left e ->
      error
        $  "Could not parse chapter file title '"
        ++ (show input)
        ++ "'\n"
        ++ e
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


isSimpleSubChapter :: TagList -> Bool
isSimpleSubChapter headingGroup =
  null (partitions (~== heading4P) headingGroup)


parseSectionBody :: Text -> TagList -> Html
parseSectionBody secNumber fullPage = sectionHtml
 where
  sectionGroups   = partitions (~== ("<span class=Section" :: String)) fullPage
  rawSectionGroup = rawSectionGroupFromSectionGroups secNumber sectionGroups
  sectionHtml     = NewHtml $ "<p class=SectBody>" ++ normalizeWhiteSpace
    (renderTags $ drop 6 $ dropWhile (~/= ("<span class=Leadline>" :: String))
                                     rawSectionGroup
    )


rawSectionGroupFromSectionGroups :: Text -> [TagList] -> TagList
rawSectionGroupFromSectionGroups secNumber sectionGroups =
  let bodyNumbers = filter (isSectionBodyNumber secNumber) sectionGroups
  in  case bodyNumbers of
        (x : _) -> shaveBackTagsToLastClosingP x
        _ ->
          error
            $  "Error, could not find section body number "
            ++ (T.unpack secNumber)
            ++ " in section groups: "
            ++ (show sectionGroups)


isSectionBodyNumber :: Text -> TagList -> Bool
isSectionBodyNumber secNumber sectionGroup =
  parseSectionBodyNumber sectionGroup == secNumber


parseSectionBodyNumber :: TagList -> Text
parseSectionBodyNumber sectionGroup =
  innerText $ takeWhile (~/= ("</span>" :: String)) sectionGroup
