{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

import           BasicPrelude
import qualified Data.Aeson.Encode.Pretty as Aeson (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           Data.Function            ((&))
import qualified Data.Text                as T
import           Data.Time                (Day, getZonedTime, localDay,
                                           zonedTimeToLocalTime)
import           Models.NRS
import           NRSParser


main :: IO ()
main = do
    today ← todaysDate
    let source_dir = "/tmp/www.leg.state.nv.us/"

    let nevadaJson = source_dir
                        & parseFiles today
                        & Aeson.encodePretty

    B.putStr nevadaJson


parseFiles :: Day -> String -> NRS
parseFiles today sourceDir  =
    let (indexFile, chapterFiles) = filesInDirectory $ T.pack sourceDir
    in parseNRS indexFile chapterFiles today


-- Return the index filename and a list of chapter filenames.
filesInDirectory :: Text -> (Text, [Text])
filesInDirectory _sourceDir =
    ("nrs.html", ["nrs-001.html", "nrs-432b.html"])


todaysDate :: IO Day
todaysDate = fmap (localDay . zonedTimeToLocalTime) getZonedTime
