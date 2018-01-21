{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

import           BasicPrelude
import qualified Data.Aeson.Encode.Pretty as Aeson (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           Data.Function            ((&))
import           Data.Time                (Day, getZonedTime, localDay,
                                           zonedTimeToLocalTime)
import           Models.NRS
import           NRSParser
import           FileUtil


main :: IO ()
main = do
    today ← todaysDate
    let source_dir = "/tmp/www.leg.state.nv.us/"

    let nevadaJson = source_dir
                        & parseFiles today
                        & Aeson.encodePretty

    B.putStr nevadaJson


parseFiles :: Day -> Filename -> NRS
parseFiles today sourceDir  =
    let (indexFile, chapterFiles) = filesInDirectory sourceDir
    in parseNRS indexFile chapterFiles today


-- Return the index filename and a list of chapter filenames.
filesInDirectory :: Filename -> (Filename, [Filename])
filesInDirectory _sourceDir =
    ("nrs.html", ["nrs-001.html", "nrs-432b.html"])


todaysDate :: IO Day
todaysDate = fmap (localDay . zonedTimeToLocalTime) getZonedTime