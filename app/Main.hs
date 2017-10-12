{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

import           BasicPrelude
import           ChapterFile
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           Data.Eq.Unicode
import           Data.Function            ((&))
import qualified Data.Text                as T
import           FileUtil
import           System.Environment       (getArgs)


main :: IO ()
main = do
    args ← System.Environment.getArgs
    when (length args ≠ 1)
        (fail "Usage: parse-nevada [filename]")

    let nevadaJson = head args & parseFiles & encodePretty
    B.putStr nevadaJson


parseFiles :: String -> Text
parseFiles sourceDir = "{nothing: 5}"
