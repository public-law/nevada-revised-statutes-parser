{-# LANGUAGE OverloadedStrings #-}

module TreeParser(parseTree) where

import           BasicPrelude
import qualified Data.HashMap.Lazy as HM

import           ChapterFile       (parseChapter)
import           FileUtil
import           HtmlUtil          (Html, toText)
import           IndexFile         (parseTitles)
import           Models.Chapter
import           Models.Tree

type ChapterMap = HashMap RelativePath Html

parseTree :: Html -> ChapterMap -> Tree
parseTree indexFile chapterMap =
    Tree {
        chapter0 = getChapterZero chapterMap,
        titles   = parseTitles indexFile
    }


getChapterZero :: ChapterMap -> Chapter
getChapterZero chapterMap =
    case HM.lookup chapterZeroPathname chapterMap of
        Just html -> parseChapter $ toText html
        Nothing   -> error $ "Chapter Zero " ++ (show chapterZeroPathname) ++ " not found in " ++ (show $ head $ HM.keys chapterMap)


chapterZeroPathname :: RelativePath
chapterZeroPathname = (./) "NRS-000.html"

-- everythingButZero
