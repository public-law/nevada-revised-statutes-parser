{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

import           BasicPrelude
import           ChapterFile
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           Data.Eq.Unicode
import           FileUtil
import           System.Environment       (getArgs)


main :: IO ()
main = do
    args ← System.Environment.getArgs
    when (length args ≠ 1)
        (fail "Usage: parse-nevada [filename]")

    let sourceDir = head args

    html ← readFileLatin1 (fixture "nrs-432b.html")
    B.putStr $ encodePretty $ parseChapter html
