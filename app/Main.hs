{-# LANGUAGE OverloadedStrings #-}

import           BasicPrelude
import qualified Data.Aeson.Encode.Pretty as Aeson (encodePretty)
import qualified Data.ByteString.Lazy     as B
import qualified Data.HashMap.Lazy        as HM
import           Data.Time                (Day, getZonedTime, localDay,
                                           zonedTimeToLocalTime)

import           FileUtil                 (AbsolutePath, listFilesInDirectory,
                                           toFilePath, (./), (//))
import           HtmlUtil
import           Models.NRS
import           NRSParser
import           System.FilePath


main :: IO ()
main = do
    nrs <- parseFiles sourceDir
    let nevadaJson = Aeson.encodePretty nrs
    B.putStr nevadaJson


parseFiles :: AbsolutePath -> IO NRS
parseFiles dir = do
    let indexFile = (//) $ (toFilePath dir) </> "index.html"
    chapterFilenames <- listFilesInDirectory dir
    let relativeChapterFilenames = map ((./) . takeFileName . toFilePath) chapterFilenames
    today            <- todaysDate
    indexHtml        <- readHtmlFile indexFile
    chaptersHtml     <- mapM readHtmlFile chapterFilenames
    let chapterMap   = HM.fromList $ zip relativeChapterFilenames chaptersHtml
    return $ parseNRS indexHtml chapterMap today


todaysDate :: IO Day
todaysDate = fmap (localDay . zonedTimeToLocalTime) getZonedTime


sourceDir :: AbsolutePath
sourceDir = (//) "/tmp/www.leg.state.nv.us/NRS"


filesToSkip :: [AbsolutePath]
filesToSkip = [(//) $ (toFilePath sourceDir) </> "NRS-218E.html"]
