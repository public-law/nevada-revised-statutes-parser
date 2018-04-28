module Config where

import           BasicPrelude
import qualified Data.Text            as T


chaptersToSkip :: [Text]
chaptersToSkip = map T.pack ["218E"
                            ,"388C"
                            ,"460"
                            ,"519A"
                            ]

chapterUrlPrefix :: Text
chapterUrlPrefix = T.pack "https://www.leg.state.nv.us/nrs/NRS-"

chapterZeroTitle :: Text
chapterZeroTitle = T.pack "NRS: PRELIMINARY CHAPTER - GENERAL PROVISIONS"
