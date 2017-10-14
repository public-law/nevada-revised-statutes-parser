{-# LANGUAGE OverloadedStrings #-}

module NRSParser where

import           BasicPrelude
import           Data.Time
import           Models.NRS
import           Models.Tree
import           Year         (toYear)

parseNRS :: Text -> [Text] -> Day -> NRS
parseNRS indexFile chapterFiles currentDate =
    NRS {
        statuteTree  = Tree { },
        nominalDate  = toYear 2017,
        dateAccessed = currentDate
    }
