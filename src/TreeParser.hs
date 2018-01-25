{-# LANGUAGE OverloadedStrings #-}

module TreeParser(parseTree) where

import           HtmlUtil       (Html)
import           Models.Chapter
import           Models.Tree


parseTree :: Html -> [Html] -> Tree
parseTree indexFile chapterFiles =
    Tree {
        -- chapter0 = parseChapterZero chapterFiles,
        -- titles =   []
    }

-- parseChapterZero :: [Filename] -> Chapter
-- parseChapterZero filenames =
--     parseChapter
