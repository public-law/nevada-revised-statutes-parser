module SimpleChapterFile(isSimpleSubChapter, parseSectionsFromJustHtml, parseSectionFromHeadingParagraph, parseSectionBody, parseSectionsFromHeadingGroup) where

  import           BasicPrelude
  import           Text.HTML.TagSoup
  import           Text.InterpolatedString.Perl6  ( qq )


  import           HtmlUtil                       ( Html(NewHtml)
                                                  , shaveBackTagsToLastClosingP
                                                  )
  import           Models.Section                as Section
  import           TextUtil                       ( normalizeWhiteSpace
                                                  , normalizedInnerText
                                                  )



  type TagList    = [Tag Text]

  closingA :: String
  closingA = "</a>"

  closingP :: String
  closingP = "</p>"

  leadlineP :: String
  leadlineP = "<p class=COLeadline>"

  heading4P :: String
  heading4P = "<p class=COHead4>"

  horizontalRule :: String
  horizontalRule = "<p class=\"J-Dash\""


  isSimpleSubChapter :: TagList -> Bool
  isSimpleSubChapter headingGroup =
    null (partitions (~== heading4P) headingGroup)


  parseSectionsFromJustHtml :: TagList -> Either String [Section]
  parseSectionsFromJustHtml fullPage = do
    let topHalf    = takeWhile (~/= horizontalRule) fullPage
    let bottomHalf = dropWhile (~/= horizontalRule) fullPage
    sections' <- parseSectionsFromHeadingGroup bottomHalf topHalf
    return sections'


  parseSectionsFromHeadingGroup :: TagList -> TagList -> Either String [Section]
  parseSectionsFromHeadingGroup contentHalf headingsHalf =
    let parser     = parseSectionFromHeadingParagraph contentHalf
        paragraphs = headingParagraphsWithContent headingsHalf
    in mapM parser paragraphs


  -- Some COLeadline P's have no content; they're just used for vertical spacing.
  headingParagraphsWithContent :: TagList -> [TagList]
  headingParagraphsWithContent headingParagraphs =
    filter (\tags -> length tags > 4) (partitions (~== leadlineP) headingParagraphs)


  parseSectionFromHeadingParagraph :: TagList -> TagList -> Either String Section
  parseSectionFromHeadingParagraph contentHalf paragraph = do
    let secName       = normalizedInnerText $ takeWhile (~/= closingP) $ dropWhile (~/= closingA) paragraph
    let rawNumberText = normalizedInnerText $ takeWhile (~/= closingA) paragraph
    let secNumber     = parseNumberFromRawNumberText rawNumberText secName
    let secBody       = parseSectionBody secNumber contentHalf
    (name', number', body') <- toThreeSectionFields secName secNumber secBody
    return Section
      { Section.name   = name'
      , Section.number = number'
      , Section.body   = body'
      }


  toThreeSectionFields :: Text -> Text -> Html -> Either String (SectionName, SectionNumber, SectionBody)
  toThreeSectionFields name' number' body' = do
    name''   <- toSectionName name' [qq| $number' $body' |]
    number'' <- toSectionNumber number' [qq| $name' $body' |]
    body''   <- toSectionBody body' [qq| $name' $number' |]
    return (name'', number'', body'')


  parseNumberFromRawNumberText :: Text -> Text -> Text
  parseNumberFromRawNumberText numberText secName = case words numberText of
    (_ : x : _) -> case toSectionNumber x [qq|name: $secName|] of
      Left message -> error [qq|while parsing $numberText - $message|]
      Right _ -> x
    _ -> error [qq|Expected sec. $numberText $secName to have >= 2 words|]


  parseSectionBody :: Text -> TagList -> Html
  parseSectionBody secNumber contentHalf = sectionHtml
   where
    sectionGroups   = partitions (~== ("<span class=Section" :: String)) contentHalf
    rawSectionGroup = rawSectionGroupFromSectionGroups secNumber sectionGroups
    sectionHtml     = NewHtml $ "<p class=SectBody>" ++ normalizeWhiteSpace
      (renderTags $ drop 6 $ dropWhile (~/= ("<span class=Leadline>" :: String))
                                       rawSectionGroup
      )


  rawSectionGroupFromSectionGroups :: Text -> [TagList] -> TagList
  rawSectionGroupFromSectionGroups secNumber secGroups =
    let bodyNumbers = filter (isSectionBodyNumber secNumber) secGroups
    in  case bodyNumbers of
          (x : _) -> shaveBackTagsToLastClosingP x
          _ ->
            error [qq|Couldn't find sec. body for num. "$secNumber" in sec. groups: ...|]


  isSectionBodyNumber :: Text -> TagList -> Bool
  isSectionBodyNumber secNumber sectionGroup =
    parseSectionBodyNumber sectionGroup == secNumber


  parseSectionBodyNumber :: TagList -> Text
  parseSectionBodyNumber sectionGroup =
    innerText $ takeWhile (~/= ("</span>" :: String)) sectionGroup
  