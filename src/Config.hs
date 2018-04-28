module Config where

import           BasicPrelude
import qualified Data.Text            as T

import FileUtil


chaptersToSkip      = map T.pack ["218E"
                                 ,"388C"
                                 ,"460"
                                 ,"519A"
                                 ]
chapterUrlPrefix    = T.pack "https://www.leg.state.nv.us/nrs/NRS-"
chapterZeroTitle    = T.pack "NRS: PRELIMINARY CHAPTER - GENERAL PROVISIONS"
chapterZeroPathname = (./) "NRS-000.html"


chaptersToSkip :: [Text]
chapterUrlPrefix :: Text
chapterZeroTitle :: Text
chapterZeroPathname :: RelativePath
