module SimpleChapterFile
  ( isSimpleSubChapter
  , parseSectionsFromJustHtml
  , parseSectionFromHeadingParagraph
  , parseSectionBody
  , parseSectionsFromHeadingGroup
  )
where

import           BasicPrelude
import           Text.HTML.TagSoup
import           Text.InterpolatedString.Perl6  ( qq )

import           HtmlUtil                       ( Html(NewHtml)
                                                , shaveBackTagsToLastClosingP
                                                )
import           Models.Section                as Section
import           Parsing                        ( ChapterData(..)
                                                , TagList
                                                )
import qualified SectionParser
import           TextUtil                       ( normalizeWhiteSpace
                                                , normalizedInnerText
                                                )



closingA :: String
closingA = "</a>"

closingP :: String
closingP = "</p>"

leadlineP :: String
leadlineP = "<p class=COLeadline>"

heading4P :: String
heading4P = "<p class=COHead4>"

isSimpleSubChapter :: TagList -> Bool
isSimpleSubChapter headingGroup =
  null (partitions (~== heading4P) headingGroup)


parseSectionsFromJustHtml :: ChapterData -> Either String [Section]
parseSectionsFromJustHtml chapterData =
  let parser     = parseSectionFromHeadingParagraph chapterData
      paragraphs = headingParagraphsWithContent chapterData
  in  mapM parser paragraphs


parseSectionsFromHeadingGroup :: ChapterData -> TagList -> Either String [Section]
parseSectionsFromHeadingGroup chapterData headingGroup =
  let paragraphs = headingParagraphsWithContent' headingGroup
      parser = parseSectionFromHeadingParagraph chapterData
  in
    mapM parser paragraphs


-- Some COLeadline P's have no content; they're just used for vertical spacing.
headingParagraphsWithContent :: ChapterData -> [TagList]
headingParagraphsWithContent chapterData = filter SectionParser.isTOCEntry
                                                  putativeTOCEntries
 where
  putativeTOCEntries =
    (takeWhile (~/= closingP))
      <$> (partitions (~== leadlineP) (headings chapterData))


headingParagraphsWithContent' :: TagList -> [TagList]
headingParagraphsWithContent' headingGroup = filter SectionParser.isTOCEntry
                                                  putativeTOCEntries
  where
  putativeTOCEntries =
    (takeWhile (~/= closingP))
      <$> (partitions (~== leadlineP) headingGroup)

      
parseSectionFromHeadingParagraph
  :: ChapterData -> TagList -> Either String Section
parseSectionFromHeadingParagraph chapterData paragraph = do
  let secName = normalizedInnerText $ takeWhile (~/= closingP) $ dropWhile
        (~/= closingA)
        paragraph
  let rawNumberText = normalizedInnerText $ takeWhile (~/= closingA) paragraph
  let secNumber     = parseNumberFromRawNumberText rawNumberText secName
  secBody <- parseSectionBody secNumber chapterData

  (name', number', body') <- toThreeSectionFields secName secNumber secBody
  return Section
    { Section.name   = name'
    , Section.number = number'
    , Section.body   = body'
    }


toThreeSectionFields
  :: Text
  -> Text
  -> Html
  -> Either String (SectionName, SectionNumber, SectionBody)
toThreeSectionFields name' number' body' = do
  name''   <- toSectionName name' [qq| $number' $body' |]
  number'' <- toSectionNumber number' [qq| $name' $body' |]
  body''   <- toSectionBody body' [qq| $name' $number' |]
  return (name'', number'', body'')


parseNumberFromRawNumberText :: Text -> Text -> Text
parseNumberFromRawNumberText numberText secName = case words numberText of
  (_ : x : _) -> case toSectionNumber x [qq|name: $secName|] of
    Left  message -> error [qq|while parsing $numberText - $message|]
    Right _       -> x
  _ -> error [qq|Expected sec. $numberText $secName to have >= 2 words|]


parseSectionBody :: Text -> ChapterData -> Either String Html
parseSectionBody secNumber chapterData = sectionHtml
 where
  rawSectionGroup = rawSectionGroupFromSectionGroups secNumber (sectionGroups chapterData)
  sectionHtml = case rawSectionGroup of
    Right secGroup -> Right $ NewHtml $ "<p class=SectBody>" ++ normalizeWhiteSpace (renderTags $ drop 6 $ dropWhile (~/= ("<span class=Leadline>" :: String)) secGroup)
    Left x  -> Left x


rawSectionGroupFromSectionGroups :: Text -> [TagList] -> Either String TagList
rawSectionGroupFromSectionGroups secNumber secGroups =
  let bodyNumbers = filter (isSectionBodyNumber secNumber) secGroups
  in
    case bodyNumbers of
      (x : _) -> Right $ shaveBackTagsToLastClosingP x
      _       -> Left $ [qq|Couldn't find sec. body for num. "$secNumber" in sec. groups.|]


isSectionBodyNumber :: Text -> TagList -> Bool
isSectionBodyNumber secNumber sectionGroup =
  parseSectionBodyNumber sectionGroup == secNumber


parseSectionBodyNumber :: TagList -> Text
parseSectionBodyNumber sectionGroup =
  innerText $ takeWhile (~/= ("</span>" :: String)) sectionGroup

