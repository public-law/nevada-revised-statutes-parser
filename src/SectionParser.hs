module SectionParser
  ( isTOCEntry
  )
where

import           BasicPrelude
import           Text.HTML.TagSoup

isTOCEntry :: [Tag Text] -> Bool
isTOCEntry _ = False
