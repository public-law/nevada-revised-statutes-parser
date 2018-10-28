module TreeParser
  ( parseTree
  )
where

import           BasicPrelude                   ( Maybe(Just, Nothing)
                                                , error
                                                , head
                                                , show
                                                , ($)
                                                , (++)
                                                )
import qualified Data.HashMap.Lazy             as HM

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
parseChapterZero chapterMap = case HM.lookup chapterZeroPathname chapterMap of
  Just html -> parseChapter html
  Nothing ->
    error
      $  "Chapter Zero "
      ++ (show chapterZeroPathname)
      ++ " not found in "
      ++ (show $ head $ HM.keys chapterMap)


allButChapterZero :: ChapterMap -> ChapterMap
allButChapterZero = HM.delete chapterZeroPathname
