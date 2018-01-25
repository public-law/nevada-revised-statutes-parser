{-# LANGUAGE OverloadedStrings #-}

module TreeParser(parseTree) where

import           FileUtil       (Filename)
import           Models.Chapter
import           Models.Tree


-- This is all going to change with Html coming down,
-- not filenames.
parseTree :: Filename -> [Filename] -> Tree
parseTree indexFile chapterFiles =
    Tree {
        -- chapter0 = parseChapterZero chapterFiles,
        -- titles =   []
    }

-- parseChapterZero :: [Filename] -> Chapter
-- parseChapterZero filenames =
--     parseChapter
