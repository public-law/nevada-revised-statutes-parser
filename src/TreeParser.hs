{-# LANGUAGE OverloadedStrings #-}

module TreeParser(parseTree) where

import           FileUtil       (Filename)
import           Models.Chapter
import           Models.Tree


parseTree :: Filename -> [Filename] -> Tree
parseTree indexFile chapterFiles =
    Tree {
        chapter0 = parseChapterZero chapterFiles,
        titles =   []
    }

parseChapterZero :: [Filename] -> Chapter
parseChapterZero filenames =
    parseChapter
