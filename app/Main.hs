{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

import           BasicPrelude
import qualified Data.Aeson.Encode.Pretty as Aeson (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           Data.Time                (Day, getZonedTime, localDay,
                                           zonedTimeToLocalTime)
import           FileUtil
import           HtmlUtil
import           Models.NRS
import           NRSParser


main :: IO ()
main = do
    nrs <- parseFiles source_dir
    let nevadaJson = Aeson.encodePretty nrs

    B.putStr nevadaJson


parseFiles :: Filename -> IO NRS
parseFiles sourceDir = do
    let (indexFile, chapterFiles) = filesInDirectory sourceDir
    today        <- todaysDate
    indexHtml    <- readHtmlFile indexFile
    chaptersHtml <- mapM readHtmlFile chapterFiles
    return $ parseNRS indexHtml chaptersHtml today


-- Return the abs paths to the index file and a list of chapters.
filesInDirectory :: Filename -> (Filename, [Filename])
filesInDirectory _sourceDir =
    ("/tmp/www.leg.state.nv.us/NRS/index.html",
      ["/tmp/www.leg.state.nv.us/NRS/nrs-001.html",
       "/tmp/www.leg.state.nv.us/NRS/nrs-432b.html"
      ])


todaysDate :: IO Day
todaysDate = fmap (localDay . zonedTimeToLocalTime) getZonedTime



chapterZeroFilename :: Filename
chapterZeroFilename = "NRS-000.html"

source_dir :: Filename
source_dir = "/tmp/www.leg.state.nv.us/NRS"
