module Parsing
  ( ChapterData(..)
  , TagList
  )
where
--
-- Common parsing data types
--
import           BasicPrelude
import           Text.HTML.TagSoup




type TagList    = [Tag Text]

-- A datatype for parameter passing: low-level parsing results
-- which the detailed parser functions use as input.
data ChapterData = ChapterData {
  headings :: TagList,
  content  :: TagList,
  sectionGroups :: [TagList]
}
