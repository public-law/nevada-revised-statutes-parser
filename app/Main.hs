{-# LANGUAGE OverloadedStrings #-}

import           BasicPrelude
import qualified Data.Aeson.Encode.Pretty as Aeson (encodePretty)
import qualified Data.ByteString.Lazy     as B
import qualified Data.HashMap.Lazy        as HM
import           Data.Time                (Day, getZonedTime, localDay,
                                           zonedTimeToLocalTime)

import           FileUtil                 (listFilesInDirectory, toAbsolutePath, toRelativePath, toFilePath)
import           HtmlUtil
import           Models.NRS
import           NRSParser
import           System.FilePath


main :: IO ()
main = do
    nrs <- parseFiles sourceDir
    let nevadaJson = Aeson.encodePretty nrs
    B.putStr nevadaJson


parseFiles :: FilePath -> IO NRS
parseFiles source = do
    let indexFile = toAbsolutePath $ source </> "index.html"
    chapterFilenames <- listFilesInDirectory sourceDir
    let relativeChapterFilenames = map (toRelativePath . takeFileName . toFilePath) chapterFilenames
    today            <- todaysDate
    indexHtml        <- readHtmlFile indexFile
    chaptersHtml     <- mapM readHtmlFile chapterFilenames
    let chaptersMap  = HM.fromList $ zip relativeChapterFilenames chaptersHtml
    return $ parseNRS indexHtml chaptersMap today


todaysDate :: IO Day
todaysDate = fmap (localDay . zonedTimeToLocalTime) getZonedTime


sourceDir :: FilePath
sourceDir = "/tmp/www.leg.state.nv.us/NRS"
