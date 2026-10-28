module SimpleChapterFile
  ( isSimpleSubChapter
  , parseSectionsFromJustHtml
  , parseSectionFromHeadingParagraph
  , parseSectionBody
  , parseSectionsFromHeadingGroup
  , parseNumberFromRawNumberText
  )
where

import           BasicPrelude
import qualified Data.Text                     as T
import           Text.HTML.TagSoup
import           Text.InterpolatedString.Perl6  ( qq )

import           HtmlUtil                       ( Html(NewHtml)
                                                , shaveBackTagsToLastClosingP
                                                )
import           Models.ParseFailure            ( ParseFailure
                                                , accumulate
                                                , atSection
                                                , parseFailure
                                                )
import           Models.Section                as Section
import           Parsing                        ( ChapterData(..)
                                                , TagList
                                                )
import qualified SectionParser
import           TextUtil                       ( normalizeWhiteSpace
                                                , normalizedInnerText
                                                )
import           Util                           ( (|>) )


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


parseSectionsFromJustHtml :: ChapterData -> ([Section], [ParseFailure])
parseSectionsFromJustHtml chapterData =
  accumulateSections chapterData (headingParagraphsWithContent chapterData)


parseSectionsFromHeadingGroup
  :: ChapterData -> TagList -> ([Section], [ParseFailure])
parseSectionsFromHeadingGroup chapterData headingGroup =
  accumulateSections chapterData (headingParagraphsWithContent' headingGroup)


accumulateSections :: ChapterData -> [TagList] -> ([Section], [ParseFailure])
accumulateSections chapterData =
  accumulate (parseSectionFromHeadingParagraph chapterData) sectionFailure


sectionFailure :: TagList -> String -> ParseFailure
sectionFailure paragraph err =
  let base = parseFailure "" (T.pack err)
  in  case sectionNumberHint paragraph of
        Just n  -> base |> atSection n
        Nothing -> base


sectionNumberHint :: TagList -> Maybe Text
sectionNumberHint paragraph =
  let secName       = sectionNameFromParagraph paragraph
      rawNumberText = rawNumberTextFromParagraph paragraph
  in  case parseNumberFromRawNumberText rawNumberText secName of
        Right n -> Just n
        Left _  -> Nothing


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
    (takeWhile (~/= closingP)) <$> (partitions (~== leadlineP) headingGroup)


parseSectionFromHeadingParagraph
  :: ChapterData -> TagList -> Either String Section
parseSectionFromHeadingParagraph chapterData paragraph = do
  let secName       = sectionNameFromParagraph paragraph
  let rawNumberText = rawNumberTextFromParagraph paragraph
  secNumber <- parseNumberFromRawNumberText rawNumberText secName
  secBody   <- parseSectionBody secNumber chapterData
  (name', number', body') <- toThreeSectionFields secName secNumber secBody
  return Section
    { Section.name   = name'
    , Section.number = number'
    , Section.body   = body'
    }


sectionNameFromParagraph :: TagList -> Text
sectionNameFromParagraph paragraph =
  normalizedInnerText $ takeWhile (~/= closingP) $ dropWhile (~/= closingA)
                                                             paragraph


rawNumberTextFromParagraph :: TagList -> Text
rawNumberTextFromParagraph paragraph =
  normalizedInnerText $ takeWhile (~/= closingA) paragraph


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


parseNumberFromRawNumberText :: Text -> Text -> Either String Text
parseNumberFromRawNumberText numberText secName = case words numberText of
  (_ : x : _) -> case toSectionNumber x [qq|name: $secName|] of
    Left  message -> Left [qq|while parsing $numberText - $message|]
    Right _       -> Right x
  _ -> Left [qq|Expected sec. $numberText $secName to have >= 2 words|]


parseSectionBody :: Text -> ChapterData -> Either String Html
parseSectionBody secNumber chapterData = sectionHtml
 where
  rawSectionGroup =
    rawSectionGroupFromSectionGroups secNumber (sectionGroups chapterData)
  sectionHtml = case rawSectionGroup of
    Right secGroup ->
      Right $ NewHtml $ "<p class=SectBody>" ++ normalizeWhiteSpace
        (renderTags $ drop 6 $ dropWhile
          (~/= ("<span class=Leadline>" :: String))
          secGroup
        )
    Left x -> Left x


rawSectionGroupFromSectionGroups :: Text -> [TagList] -> Either String TagList
rawSectionGroupFromSectionGroups secNumber secGroups =
  let bodyNumbers = filter (isSectionBodyNumber secNumber) secGroups
  in  case bodyNumbers of
        (x : _) -> Right $ shaveBackTagsToLastClosingP x
        _ ->
          Left
            [qq|Couldn't find sec. body for num. "$secNumber" in sec. groups.|]


isSectionBodyNumber :: Text -> TagList -> Bool
isSectionBodyNumber secNumber sectionGroup =
  parseSectionBodyNumber sectionGroup == secNumber


parseSectionBodyNumber :: TagList -> Text
parseSectionBodyNumber sectionGroup =
  innerText $ takeWhile (~/= ("</span>" :: String)) sectionGroup
