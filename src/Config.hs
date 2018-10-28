module Config where

import           BasicPrelude                   ( Text )

import           FileUtil


chaptersToSkip = ["218E", "388C", "460", "519A"]
chapterUrlPrefix = "https://www.leg.state.nv.us/nrs/NRS-"
chapterZeroTitle = "NRS: PRELIMINARY CHAPTER - GENERAL PROVISIONS"
chapterZeroPathname = (./) "NRS-000.html"


chaptersToSkip :: [Text]
chapterUrlPrefix :: Text
chapterZeroTitle :: Text
chapterZeroPathname :: RelativePath
