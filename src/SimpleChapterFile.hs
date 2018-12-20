module SimpleChapterFile(isSimpleSubChapter, parseSectionsFromJustHtml) where

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


  parseSectionsFromJustHtml :: TagList -> [Section]
  parseSectionsFromJustHtml fullPage =
    parseSectionsFromHeadingGroup bottomHalf topHalf
    where
      topHalf    = takeWhile (~/= horizontalRule) fullPage
      bottomHalf = dropWhile (~/= horizontalRule) fullPage


  parseSectionsFromHeadingGroup :: TagList -> TagList -> [Section]
  parseSectionsFromHeadingGroup contentHalf headingsHalf = fmap
    (parseSectionFromHeadingParagraph contentHalf)
    (headingParagraphsWithContent headingsHalf)


  -- Some COLeadline P's have no content; they're just used for vertical spacing.
  headingParagraphsWithContent :: TagList -> [TagList]
  headingParagraphsWithContent headingParagraphs =
    filter (\tags -> length tags > 4) (partitions (~== leadlineP) headingParagraphs)


  parseSectionFromHeadingParagraph :: TagList -> TagList -> Section
  parseSectionFromHeadingParagraph contentHalf paragraph = Section
    { Section.name   = toSectionName secName
    , Section.number = toSectionNumber secNumber
    , Section.body   = toSectionBody secBody
    }
   where
    secName = normalizedInnerText $ takeWhile (~/= closingP) $ dropWhile
      (~/= closingA)
      paragraph
    rawNumberText = normalizedInnerText $ takeWhile (~/= closingA) paragraph
    secNumber     = parseNumberFromRawNumberText rawNumberText secName
    secBody       = parseSectionBody secNumber contentHalf


  parseNumberFromRawNumberText :: Text -> Text -> Text
  parseNumberFromRawNumberText numberText secName = case words numberText of
    (_ : x : _) -> x
    _ ->
      error [qq|Expected sec. $numberText $secName to have >= 2 words|]


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
            error [qq|Couldn't find sec. body num. $secNumber in sec. groups: $secGroups|]


  isSectionBodyNumber :: Text -> TagList -> Bool
  isSectionBodyNumber secNumber sectionGroup =
    parseSectionBodyNumber sectionGroup == secNumber


  parseSectionBodyNumber :: TagList -> Text
  parseSectionBodyNumber sectionGroup =
    innerText $ takeWhile (~/= ("</span>" :: String)) sectionGroup
  