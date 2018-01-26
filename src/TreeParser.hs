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
    case HM.lookup "NRS-000.html" chapterMap of
        Just html -> parseChapter $ toText html
        Nothing   -> error $ "Chapter Zero not found in " ++ (show $ HM.keys chapterMap)

-- everythingButZero
