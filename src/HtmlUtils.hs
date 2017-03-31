{-# LANGUAGE OverloadedStrings #-}

module HtmlUtils where

import           BasicPrelude
import           Text.HTML.TagSoup (Tag, partitions, (~==))


-- Return all the occurrences of an HTML tag within the given
-- HTML chunk.
findAll ∷ String → [Tag Text] → [[Tag Text]]
findAll searchTerm =
  partitions (~== s searchTerm)


-- Lower-ceremony way to declare a string
s ∷ String → String
s = id
