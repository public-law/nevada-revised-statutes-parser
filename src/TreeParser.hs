{-# LANGUAGE OverloadedStrings #-}

module TreeParser(parseTree) where

import           BasicPrelude
import           HtmlUtil       (Html)
import           Models.Tree


parseTree :: Html -> HashMap FilePath Html -> Tree
parseTree indexFile chapterFiles =
    Tree {
        -- chapter0 = parseChapterZero chapterFiles,
        -- titles =   []
    }

-- parseChapterZero :: [Filename] -> Chapter
-- parseChapterZero filenames =
--     parseChapter

chapterZeroFilename :: FilePath
chapterZeroFilename = "NRS-000.html"