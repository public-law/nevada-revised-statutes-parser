{-# LANGUAGE OverloadedStrings #-}

import           BasicPrelude
import qualified Data.Aeson.Encode.Pretty as Aeson (encodePretty)
import qualified Data.ByteString.Lazy     as B
import qualified Data.HashMap.Lazy        as HM
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
    let chaptersMap  = HM.fromList $ zip chapterFiles chaptersHtml
    return $ parseNRS indexHtml chaptersMap today


todaysDate :: IO Day
todaysDate = fmap (localDay . zonedTimeToLocalTime) getZonedTime


indexFilename :: FilePath
indexFilename = "index.html"

sourceDir :: FilePath
sourceDir = "/tmp/www.leg.state.nv.us/NRS"
