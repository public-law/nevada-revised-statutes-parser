{-# LANGUAGE OverloadedStrings #-}

module NRSParser where

import           BasicPrelude
import qualified Data.HashMap.Lazy             as HM
import           System.FilePath

import           ChapterFile                    ( ChapterMap )
import           Data.Time
import           HtmlUtil                       ( Html
                                                , readHtmlFile
                                                )
import           Models.NRS
import           TreeParser                     ( parseTree )
import           Year                           ( toYear )
import           FileUtil                       ( AbsolutePath
                                                , listFilesInDirectory
                                                , toFilePath
                                                , (./)
                                                , (//)
                                                )
import           DateUtil                       ( todaysDate )


parseFiles :: AbsolutePath -> IO NRS
parseFiles dir = do
  let indexFile = (//) $ toFilePath dir </> "index.html"
  chapterFilenames <- listFilesInDirectory dir
  let relativeChapterFilenames =
        (./) . takeFileName . toFilePath <$> chapterFilenames
  today        <- todaysDate
  indexHtml    <- readHtmlFile indexFile
  chaptersHtml <- mapM readHtmlFile chapterFilenames
  let chapterMap = HM.fromList $ zip relativeChapterFilenames chaptersHtml
  return $ parseNRS indexHtml chapterMap today


parseNRS :: Html -> ChapterMap -> Day -> NRS
parseNRS indexFile chapterMap currentDate = NRS
  { statuteTree  = parseTree indexFile chapterMap
  , nominalDate  = toYear 2018
  , dateAccessed = currentDate
  }
