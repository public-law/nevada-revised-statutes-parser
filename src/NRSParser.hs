module NRSParser where

import           ChapterFile                    ( ChapterMap )
import           Data.Time
import           HtmlUtil                       ( Html )
import           Models.NRS
import           TreeParser                     ( parseTree )
import           Year                           ( toYear )

parseNRS :: Html -> ChapterMap -> Day -> NRS
parseNRS indexFile chapterMap currentDate = NRS
  { statuteTree  = parseTree indexFile chapterMap
  , nominalDate  = toYear 2018
  , dateAccessed = currentDate
  }
