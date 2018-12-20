module TreeParser
  ( parseTree
  )
where

import           BasicPrelude                   ( Maybe(Just, Nothing)
                                                , error
                                                , head
                                                , ($)
                                                )
import qualified Data.HashMap.Lazy             as HM
import           Text.InterpolatedString.Perl6  ( qq )

import           ChapterFile                    ( ChapterMap
                                                , parseChapter
                                                )
import           HtmlUtil                       ( Html )
import           IndexFile                      ( parseTitlesAndChapters )

import           Config
import           Models.Chapter
import           Models.Tree


parseTree :: Html -> ChapterMap -> Tree
parseTree indexFile chapterMap = Tree
  { chapter0 = parseChapterZero chapterMap
  , titles   = parseTitlesAndChapters indexFile (allButChapterZero chapterMap)
  }


parseChapterZero :: ChapterMap -> Chapter
parseChapterZero chapterMap =
  let path = chapterZeroPathname
  in  case HM.lookup path chapterMap of
        Just html -> parseChapter html
        Nothing ->
          error [qq|Chap. Zero $path not found in {head $ HM.keys chapterMap}|]


allButChapterZero :: ChapterMap -> ChapterMap
allButChapterZero = HM.delete chapterZeroPathname
