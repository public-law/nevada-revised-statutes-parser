{-# LANGUAGE OverloadedStrings #-}

import          BasicPrelude
import          Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B

import          FileUtil
import          ChapterFile


main :: IO ()
main = do
    html <- readFileAsUtf8 (fixture "nrs-432b.html") "LATIN1"
    let chapter = parseChapter html
    B.putStr $ encodePretty chapter
