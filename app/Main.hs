{-# LANGUAGE OverloadedStrings #-}

import           BasicPrelude
import qualified Data.Aeson.Encode.Pretty as Aeson (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           Data.Time                (Day, getZonedTime, localDay,
                                           zonedTimeToLocalTime)
import           System.Directory
import           HtmlUtil
import           Models.NRS
import           NRSParser


main :: IO ()
main = do
    nrs <- parseFiles sourceDir
    let nevadaJson = Aeson.encodePretty nrs

    B.putStr nevadaJson


parseFiles :: FilePath -> IO NRS
parseFiles source = do
    let indexFile = source </> indexFilename
    chapterFiles <- listDirectory sourceDir
    today        <- todaysDate
    indexHtml    <- readHtmlFile indexFile
    chaptersHtml <- mapM readHtmlFile chapterFiles
    return $ parseNRS indexHtml chaptersHtml today


todaysDate :: IO Day
todaysDate = fmap (localDay . zonedTimeToLocalTime) getZonedTime


indexFilename :: FilePath
indexFilename = "index.html"

chapterZeroFilename :: FilePath
chapterZeroFilename = "NRS-000.html"

sourceDir :: FilePath
sourceDir = "/tmp/www.leg.state.nv.us/NRS"
