{-# LANGUAGE OverloadedStrings #-}

import           BasicPrelude
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           Data.Eq.Unicode

import           ChapterFile
import           FileUtil


main :: IO ()
main = do
    args ← getArgs
    when (length args ≠ 1)
      (fail "Usage: parse-nevada [directory]")
    let sourceDir = head args

    html ← readFileLatin1 (fixture "nrs-432b.html")
    B.putStr $ encodePretty $ parseChapter html
