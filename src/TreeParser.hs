{-# LANGUAGE OverloadedStrings #-}

module TreeParser(parseTree) where

import           BasicPrelude
import qualified Data.HashMap.Lazy as HM

import           HtmlUtil       (Html, toText)
import           Models.Chapter
import           Models.Tree
import           ChapterFile    (parseChapter)

type ChapterMap = HashMap FilePath Html

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
        Nothing   -> error "Chapter Zero not found"

-- everythingButZero