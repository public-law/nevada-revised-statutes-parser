module SectionParser
  ( isTOCEntry
  )
where

import           BasicPrelude
import           Text.HTML.TagSoup

import           TextUtil                       ( normalizeWhiteSpace )


-- Not all paragraphs in the Table of Contents with the COLeadline
-- class are actual leadline entries. There are empty paragraphs as
-- well as annotations marked with this CSS class. This predicate 
-- helps filter out the unwanted ones.
isTOCEntry :: [Tag Text] -> Bool
isTOCEntry tags =
  length tags
    == 5
    && isTagOpenName "p" (tags !! 0)
    && isTagOpenName "a" (tags !! 1)
    && isTagText (tags !! 4)
    && normalizeWhiteSpace (innerText tags)
    /= ""
