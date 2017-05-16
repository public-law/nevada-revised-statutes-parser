{-# LANGUAGE OverloadedStrings #-}

module HtmlUtils where

import           BasicPrelude
import           Text.HTML.TagSoup (Tag, partitions, (~==), innerText)



titleText :: [Tag Text] -> Text
titleText tags = innerText $ [(findFirst "<title>" tags) !! 1]


-- Return the first occurrence of an HTML tag within the given
-- HTML chunk.
findFirst ∷ String → [Tag Text] → [Tag Text]
findFirst searchTerm html =
  head $ findAll searchTerm html


-- Return all the occurrences of an HTML tag within the given
-- HTML chunk.
findAll ∷ String → [Tag Text] → [[Tag Text]]
findAll searchTerm =
  partitions (~== s searchTerm)


-- Lower-ceremony way to declare a string
s ∷ String → String
s = id
