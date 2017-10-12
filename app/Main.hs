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
import           Models.NRS
import           NRSParser
import           System.Environment       (getArgs)

main :: IO ()
main = do
    args ← System.Environment.getArgs
    when (length args ≠ 1)
        (fail "Usage: parse-nevada [directory]")

    let nevadaJson = head args
                        & parseFiles
                        & encodePretty
    B.putStr nevadaJson


parseFiles :: String -> NRS
parseFiles sourceDir =
    let (indexFile, chapterFiles) = filesInDirectory $ T.pack sourceDir
    in parseNRS indexFile chapterFiles


filesInDirectory :: Text -> (Text, [Text])
-- Return the index filename and a list of chapter filenames.
filesInDirectory _sourceDir =
    ("nrs.html", ["nrs-001.html", "nrs-432b.html"])
