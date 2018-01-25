{-# LANGUAGE OverloadedStrings #-}

module NRSParser where

import           BasicPrelude
import           Data.Time
import           Models.NRS
import           Models.Tree
import           Year         (toYear)
import          FileUtil        (Filename)
import TreeParser (parseTree)

parseNRS :: Filename -> [Filename] -> Day -> NRS
parseNRS indexFile chapterFiles currentDate =
    NRS {
        statuteTree  = parseTree indexFile chapterFiles,
        nominalDate  = toYear 2018,
        dateAccessed = currentDate
    }
