module SimpleChapterFile(isSimpleSubChapter, parseSectionsFromJustHtml) where

  import           BasicPrelude
  import qualified Data.Text                     as T
  import           Text.HTML.TagSoup
  
  
  import qualified FileUtil                      as Util
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
  parseSectionsFromHeadingGroup fullPage headingGroup = fmap
    (parseSectionFromHeadingParagraph fullPage)
    (headingParagraphsWithContent headingGroup)
  
  
  -- Some COLeadline P's have no content; they're just used for vertical spacing.
  headingParagraphsWithContent :: TagList -> [TagList]
  headingParagraphsWithContent headingGroup =
    filter (\tags -> length tags > 4) (partitions (~== leadlineP) headingGroup)
  
  
  parseSectionFromHeadingParagraph :: TagList -> TagList -> Section
  parseSectionFromHeadingParagraph fullPage paragraph = Section
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
  