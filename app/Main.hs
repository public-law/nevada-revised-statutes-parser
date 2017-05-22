{-# LANGUAGE OverloadedStrings #-}

import          BasicPrelude
import          Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B

import          FileUtil
import          ChapterFile


main :: IO ()
main = do
    html <- readFileLatin1 (fixture "nrs-432b.html")
    B.putStr $ encodePretty $ parseChapter html
