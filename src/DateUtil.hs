module DateUtil where

import           BasicPrelude                   ( IO
                                                , fmap
                                                , (.)
                                                )
import           Data.Time                      ( Day
                                                , getZonedTime
                                                , localDay
                                                , zonedTimeToLocalTime
                                                )

todaysDate :: IO Day
todaysDate = fmap (localDay . zonedTimeToLocalTime) getZonedTime
