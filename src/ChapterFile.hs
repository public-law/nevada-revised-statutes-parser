module ChapterFile where

import           BasicPrelude
import qualified Data.Attoparsec.Text (Parser, parseOnly, takeText, takeWhile)
import           Data.Char            (isAlpha, isSpace)
import qualified Data.HashMap.Lazy    as HM
import qualified Data.Text            as T
import           Text.HTML.TagSoup
import           Text.Parser.Char
import           Text.Printf


import           FileUtil             (RelativePath, toRelativePath, toString)
import           HtmlUtil             (Html, shaveBackTagsToLastClosingP,
                                       titleText, toText)
import           Models.Chapter       as Chapter
import           Models.Section       as Section
import           Models.SubChapter    as SubChapter
import           Models.SubSubChapter as SubSubChapter
import           TextUtil             (normalizeWhiteSpace, normalizedInnerText,
                                       titleize)


--
-- TODO: Any way to shorten this file?
--


type ChapterMap = HashMap RelativePath Html

chapterUrlPrefix :: Text
chapterUrlPrefix = T.pack "https://www.leg.state.nv.us/nrs/NRS-"

chapterZeroTitle :: Text
chapterZeroTitle = T.pack "NRS: PRELIMINARY CHAPTER - GENERAL PROVISIONS"


fillInEmptyChapter :: ChapterMap -> Chapter -> Chapter
fillInEmptyChapter chapterMap emptyChapter  =
    let key       = chapterNumberToFilename (Chapter.number emptyChapter)
        maybeHtml = HM.lookup key chapterMap
    in case maybeHtml of
        Just html -> parseChapter html
        Nothing   -> error $ "Chapter " ++ (toString key) ++ " not found."


chapterNumberToFilename :: Text -> RelativePath
chapterNumberToFilename chapterNumber =
    let num = T.unpack chapterNumber
        format
            | isAlpha (last num) = "%04s"
            | otherwise          = "%03s"
    in
        toRelativePath $ "NRS-" ++ printf format num ++ ".html"


parseChapter :: Html -> Chapter
parseChapter chapterHtml =
    Chapter
    { Chapter.name = rawName
    , Chapter.number = rawNumber
    , Chapter.url = chapterUrlPrefix ++ rawNumber ++ (T.pack ".html")
    , Chapter.subChapters = subChaps
    }
  where
    tags = parseTags $ toText chapterHtml
    rawTitle = titleText tags
    (rawNumber, rawName) = parseChapterFileTitle rawTitle
    subChaps = fmap (newSubChapter tags) (headingGroups tags)

newSubChapter :: [Tag Text] -> [Tag Text] -> SubChapter
newSubChapter dom headingGroup =
    SubChapter
    { SubChapter.name = subChapterNameFromGroup headingGroup
    , children =
          if isSimpleSubChapter headingGroup
              then SubChapterSections $
                   parseSectionsFromHeadingGroup dom headingGroup
              else SubSubChapters $ parseSubSubChapters dom headingGroup
    }

parseSectionsFromHeadingGroup :: [Tag Text] -> [Tag Text] -> [Section]
parseSectionsFromHeadingGroup dom headingGroup =
    fmap
        (parseSectionFromHeadingParagraph dom)
        (headingParagraphsWithContent headingGroup)

-- Some COLeadline P's have no content; they're just used for vertical spacing.
headingParagraphsWithContent :: [Tag Text] -> [[Tag Text]]
headingParagraphsWithContent headingGroup =
    filter
        (\tags -> length tags > 4)
        (partitions (~== "<p class=COLeadline>") headingGroup)

parseSectionFromHeadingParagraph :: [Tag Text] -> [Tag Text] -> Section
parseSectionFromHeadingParagraph dom paragraph =
    Section
    {Section.name = secName, Section.number = secNumber, Section.body = secBody}
  where
    secName = normalizedInnerText $ dropWhile (~/= "</a>") paragraph
    secNumber =
        parseNumberFromRawNumberText
            (normalizedInnerText $ takeWhile (~/= "</a>") paragraph)
            (renderTags paragraph)
    secBody = parseSectionBody secNumber dom

parseNumberFromRawNumberText :: Text -> Text -> Text
parseNumberFromRawNumberText numberText secName =
    case words numberText of
        (_:x:_) -> x
        _ ->
            error
                ("Expected section \"" ++
                 (T.unpack secName) ++
                 "\" raw number \"" ++
                 (T.unpack numberText) ++ "\" to have at least two words")


parseSubSubChapters :: [Tag Text] -> [Tag Text] -> [SubSubChapter]
parseSubSubChapters dom headingGroup =
    fmap (parseSubSubChapter dom) (subSubChapterHeadingGroups headingGroup)


subSubChapterHeadingGroups :: [Tag Text] -> [[Tag Text]]
subSubChapterHeadingGroups headingGroup =
    (partitions (~== "<p class=COHead4>") headingGroup)


parseSubSubChapter :: [Tag Text] -> [Tag Text] -> SubSubChapter
parseSubSubChapter dom subSubChapterHeadingGroup =
    SubSubChapter
    {
        SubSubChapter.name     = extractSubSubChapterName subSubChapterHeadingGroup,
        SubSubChapter.sections = parseSectionsFromHeadingGroup dom subSubChapterHeadingGroup
    }


extractSubSubChapterName :: [Tag Text] -> Text
extractSubSubChapterName headingGroup =
    let linesOfText = lines $ innerText headingGroup
    in case linesOfText of
        (x:_) -> normalizeWhiteSpace x
        _     -> error $ "Could not parse sub sub chapter name from: " ++ (show headingGroup)


subnames :: [Tag Text] -> [Text]
subnames tags = fmap subChapterNameFromGroup (headingGroups tags)


subChapterNameFromGroup :: [Tag Text] -> Text
subChapterNameFromGroup (_:y:_) = titleize $ fromTagText y
subChapterNameFromGroup tags    = error $ "Could not get a chapter name from the group: " ++ (show tags)


sectionNamesFromGroup :: [Tag Text] -> [Text]
sectionNamesFromGroup headingGroup =
    fmap
        sectionNameFromParagraph
        (partitions (~== "<p class=COLeadline>") headingGroup)


sectionNameFromParagraph :: [Tag Text] -> Text
sectionNameFromParagraph = normalizedInnerText . (dropWhile (~/= "</a>"))


headingGroups :: [Tag Text] -> [[Tag Text]]
headingGroups tags = partitions (~== "<p class=COHead2>") tags


-- Input:  "NRS: CHAPTER 432B - PROTECTION OF CHILDREN FROM ABUSE AND NEGLECT"
--    or:  "NRS: PRELIMINARY CHAPTER - GENERAL PROVISIONS"
-- Output: ("432B", "Protection of Children from Abuse and Neglect")
parseChapterFileTitle :: Text -> (Text, Text)
parseChapterFileTitle input =
    if input == chapterZeroTitle
        then (T.pack "0", T.pack "Preliminary Chapter – General Provisions")
        else case (Data.Attoparsec.Text.parseOnly chapterTitleParser input) of
                 Left e ->
                     error $
                     "Could not parse chapter file title '" ++
                     (show input) ++ "'\n" ++ e
                 Right b -> b


-- Input:  "NRS: CHAPTER 432B - PROTECTION OF CHILDREN FROM ABUSE AND NEGLECT"
-- Output: ("432B", "Protection of Children from Abuse and Neglect")
chapterTitleParser :: Data.Attoparsec.Text.Parser (Text, Text)
chapterTitleParser = do
    _ <- string "NRS: CHAPTER "
    num <- Data.Attoparsec.Text.takeWhile (not . isSpace)
    _ <- string " - "
    title <- Data.Attoparsec.Text.takeText
    return $ (num, titleize title)


isSimpleSubChapter :: [Tag Text] -> Bool
isSimpleSubChapter headingGroup =
    null (partitions (~== "<p class=COHead4>") headingGroup)


parseSectionBody :: Text -> [Tag Text] -> Text
parseSectionBody secNumber dom = sectionText
  where
    sectionGroups   = partitions (~== "<span class=Section") dom
    rawSectionGroup = rawSectionGroupFromSectionGroups secNumber sectionGroups
    sectionText     =
        normalizeWhiteSpace $
        T.pack "<p class=SectBody>" ++ (renderTags rawSectionGroup)


isSectionBodyNumber :: Text -> [Tag Text] -> Bool
isSectionBodyNumber secNumber dom = parseSectionBodyNumber dom == secNumber


parseSectionBodyNumber :: [Tag Text] -> Text
parseSectionBodyNumber dom = innerText $ takeWhile (~/= "</span>") dom


rawSectionGroupFromSectionGroups :: Text -> [[Tag Text]] -> [Tag Text]
rawSectionGroupFromSectionGroups secNumber sectionGroups =
    let bodyNumbers = filter (isSectionBodyNumber secNumber) sectionGroups
        in case bodyNumbers of
            (x:_) -> shaveBackTagsToLastClosingP x
            _     -> error $ "Error, could not find section body number " ++ (T.unpack secNumber) ++ " in section groups: " ++ (show sectionGroups)
