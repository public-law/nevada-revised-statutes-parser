{-# LANGUAGE OverloadedStrings #-}

module NRSParser where

import           BasicPrelude
import           Data.Time
import           FileUtil     (RelativePath)
import           HtmlUtil     (Html)
import           Models.NRS
import           TreeParser   (parseTree)
import           Year         (toYear)

parseNRS :: Html -> HashMap RelativePath Html -> Day -> NRS
parseNRS indexFile chapterFiles currentDate =
    NRS {
        statuteTree  = parseTree indexFile chapterFiles,
        nominalDate  = toYear 2018,
        dateAccessed = currentDate
    }
