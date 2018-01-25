{-# LANGUAGE OverloadedStrings #-}

module TreeParser(parseTree) where

import           BasicPrelude
import           Models.Tree
import           FileUtil     (Filename)


parseTree :: Filename -> [Filename] -> Tree
parseTree indexFile chapterFiles =
    Tree {

    }
