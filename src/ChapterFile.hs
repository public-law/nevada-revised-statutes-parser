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
import qualified SimpleChapterFile              ( parseSectionsFromJustHtml
                                                , parseSectionsFromHeadingGroup
                                                , isSimpleSubChapter
                                                )
import           Models.Chapter                as Chapter
import           Models.ParseFailure            ( ParseFailure
                                                , atChapter
                                                )
import           Models.SubChapter             as SubChapter
import           Models.SubSubChapter          as SubSubChapter
import           Parsing                        ( ChapterData(..)
                                                , TagList
                                                )
import           TextUtil                       ( normalizeWhiteSpace
                                                , titleize
                                                )

type ChapterMap = HashMap RelativePath Html


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
  let key       = chapterNumberToFilename (Chapter.number emptyChapter)
      maybeHtml = HM.lookup key chapterMap
  in  if Chapter.number emptyChapter `notElem` chaptersToSkip
        then case maybeHtml of
          Just html -> case parseChapter html of
            Right (aChapter, _) -> aChapter
            Left  message       -> error [qq| $message in $key |]
          Nothing -> error [qq| Chapter $key not found |]
        else emptyChapter


chapterNumberToFilename :: Text -> RelativePath
chapterNumberToFilename chapterNumber =
  let format | isAlpha (T.last chapterNumber) = "%04s"
             | otherwise                      = "%03s"
  in  toRelativePath $ "NRS-" ++ printf format chapterNumber ++ ".html"


parseChapter :: Html -> Either String (Chapter, [ParseFailure])
parseChapter chapterHtml = do
  let chapterData = makeChapterData chapterHtml
  rawTitle <- titleText (headings chapterData)
  (rawNumber, rawName) <- parseChapterFileTitle rawTitle
  (sectionsOrSubChapters, fails) <- chapterContent chapterData
  let locatedFails = fmap (atChapter rawNumber) fails
  return
    ( Chapter
      { Chapter.name    = rawName
      , Chapter.number  = rawNumber
      , Chapter.url     = chapterUrlPrefix ++ rawNumber ++ ".html"
      , Chapter.content = sectionsOrSubChapters
      }
    , locatedFails
    )


makeChapterData :: Html -> ChapterData
makeChapterData chapterHtml =
  let fullPage    = parseTags $ toText chapterHtml
      topHalf'    = topHalf fullPage
      bottomHalf' = bottomHalf fullPage
  in  ChapterData
        { headings      = topHalf'
        , content       = bottomHalf'
        , sectionGroups = findSectionGroups bottomHalf'
        }


findSectionGroups :: TagList -> [TagList]
findSectionGroups contentHalf =
  partitions (~== ("<span class=Section" :: String)) contentHalf


topHalf :: TagList -> TagList
topHalf fullPage = takeWhile (~/= horizontalRule) fullPage

bottomHalf :: TagList -> TagList
bottomHalf fullPage = dropWhile (~/= horizontalRule) fullPage


chapterContent :: ChapterData -> Either String (ChapterContent, [ParseFailure])
chapterContent chapterData = do
  let groups = subChapterHeadingGroups chapterData
  subResults <- mapM (newSubChapter chapterData) groups
  let (foundSubChapters, nestedFails) = unzip subResults
      subFails = concat nestedFails
      (foundSections, sectionFails) =
        SimpleChapterFile.parseSectionsFromJustHtml chapterData
  return $ case foundSubChapters of
    [] -> (SimpleChapterContent foundSections, sectionFails)
    xs -> (ComplexChapterContent xs, subFails)


newSubChapter
  :: ChapterData -> TagList -> Either String (SubChapter, [ParseFailure])
newSubChapter chapterData headingGroup = do
  name' <- subChapterNameFromGroup headingGroup
  if SimpleChapterFile.isSimpleSubChapter headingGroup
    then do
      let (scs, fails) =
            SimpleChapterFile.parseSectionsFromHeadingGroup chapterData
                                                            headingGroup
      return
        ( SubChapter { SubChapter.name     = name'
                     , SubChapter.children = SubChapterSections scs
                     }
        , fails
        )
    else do
      (ssc, fails) <- parseSubSubChapters chapterData headingGroup
      return
        ( SubChapter { SubChapter.name     = name'
                     , SubChapter.children = SubSubChapters ssc
                     }
        , fails
        )


parseSubSubChapters
  :: ChapterData -> TagList -> Either String ([SubSubChapter], [ParseFailure])
parseSubSubChapters chapterData headingGroup = do
  let groups = subSubChapterHeadingGroups headingGroup
  results <- mapM (parseSubSubChapter chapterData) groups
  let (subs, nestedFails) = unzip results
  return (subs, concat nestedFails)


parseSubSubChapter
  :: ChapterData -> TagList -> Either String (SubSubChapter, [ParseFailure])
parseSubSubChapter chapterData subSubChapterHeadingGroup = do
  name' <- extractSubSubChapterName subSubChapterHeadingGroup
  let (sections', fails) =
        SimpleChapterFile.parseSectionsFromHeadingGroup
          chapterData
          subSubChapterHeadingGroup
  return
    ( SubSubChapter { SubSubChapter.name     = name'
                    , SubSubChapter.sections = sections'
                    }
    , fails
    )


extractSubSubChapterName :: TagList -> Either String Text
extractSubSubChapterName headingGroup =
  let linesOfText = lines $ innerText headingGroup
  in  case linesOfText of
        (x : _) -> Right $ normalizeWhiteSpace x
        _ ->
          Left [qq|Couldn't parse sub sub chapter name from: $headingGroup|]


subChapterNameFromGroup :: TagList -> Either String Text
subChapterNameFromGroup (_ : y : _) = Right $ titleize $ fromTagText y
subChapterNameFromGroup tags =
  Left [qq|Couldn't get a chapter name from the group: $tags|]


subChapterHeadingGroups :: ChapterData -> [TagList]
subChapterHeadingGroups chapterData =
  partitions (~== heading2P) (headings chapterData)


subSubChapterHeadingGroups :: TagList -> [TagList]
subSubChapterHeadingGroups subChapterHeadingGroup =
  partitions (~== heading4P) subChapterHeadingGroup


-- Input:  "NRS: CHAPTER 432B - PROTECTION OF CHILDREN FROM ABUSE AND NEGLECT"
--    or:  "NRS: PRELIMINARY CHAPTER - GENERAL PROVISIONS"
-- Output: ("432B", "Protection of Children from Abuse and Neglect")
parseChapterFileTitle :: Text -> Either String (Text, Text)
parseChapterFileTitle input = if input == chapterZeroTitle
  then Right ("0", "Preliminary Chapter – General Provisions")
  else case Data.Attoparsec.Text.parseOnly chapterTitleParser input of
    Left  e -> Left [qq|Couldn't parse chapter file title $input $e|]
    Right b -> Right b


-- Input:  "NRS: CHAPTER 432B - PROTECTION OF CHILDREN FROM ABUSE AND NEGLECT"
-- Output: ("432B", "Protection of Children from Abuse and Neglect")
chapterTitleParser :: Data.Attoparsec.Text.Parser (Text, Text)
chapterTitleParser = do
  _     <- string "NRS: CHAPTER "
  num   <- Data.Attoparsec.Text.takeWhile (not . isSpace)
  _     <- string " - "
  title <- Data.Attoparsec.Text.takeText
  return (num, normalizeWhiteSpace $ titleize title)
