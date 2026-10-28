module NRSParser where

import           BasicPrelude
import qualified Data.HashMap.Lazy             as HM
import           Data.Time
import           System.FilePath

import           ChapterFile                    ( ChapterMap )
import           DateUtil                       ( todaysDate )
import           FileUtil                       ( AbsolutePath
                                                , listFilesInDirectory
                                                , toFilePath
                                                , (./)
                                                , (//)
                                                )
import           HtmlUtil                       ( Html
                                                , readHtmlFile
                                                )
import           Models.NRS
import           Models.ParseFailure            ( ParseFailure )
import           TreeParser                     ( parseTree )
import           Year                           ( toYear )


parseFiles :: AbsolutePath -> IO (NRS, [ParseFailure])
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


parseNRS :: Html -> ChapterMap -> Day -> (NRS, [ParseFailure])
parseNRS indexFile chapterMap currentDate =
  let (tree, fails) = parseTree indexFile chapterMap
  in  ( NRS
        { statuteTree  = tree
        , nominalDate  = toYear 2018
        , dateAccessed = currentDate
        }
      , fails
      )
