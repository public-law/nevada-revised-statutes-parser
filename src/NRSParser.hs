{-# LANGUAGE OverloadedStrings #-}

module NRSParser where

import           Data.Time
import           HtmlUtil   (Html)
import           Models.NRS
import           TreeParser (parseTree)
import           Year       (toYear)

parseNRS :: Html -> [Html] -> Day -> NRS
parseNRS indexFile chapterFiles currentDate =
    NRS {
        statuteTree  = parseTree indexFile chapterFiles,
        nominalDate  = toYear 2018,
        dateAccessed = currentDate
    }
