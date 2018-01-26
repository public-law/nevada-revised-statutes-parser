{-# LANGUAGE OverloadedStrings #-}

module TreeParser(parseTree) where

import           BasicPrelude
import qualified Data.HashMap.Lazy as HM

import           ChapterFile       (parseChapter)
import           FileUtil
import           HtmlUtil          (Html, toText)
import           Models.Chapter
import           Models.Tree

type ChapterMap = HashMap RelativePath Html

parseTree :: Html -> ChapterMap -> Tree
parseTree _indexFile chapterMap =
    Tree {
        chapter0 = chapterZero chapterMap,
        titles   = []
    }


chapterZero :: ChapterMap -> Chapter
chapterZero chapterMap =
    case HM.lookup chapterZeroPathname chapterMap of
        Just html -> parseChapter $ toText html
        Nothing   -> error $ "Chapter Zero " ++ (show chapterZeroPathname) ++ " not found in " ++ (show $ head $ HM.keys chapterMap)


chapterZeroPathname :: RelativePath
chapterZeroPathname = toRelativePath "NRS-000.html"

-- everythingButZero
