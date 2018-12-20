module DateUtil where

import           BasicPrelude                   ( IO
                                                , (<$>)
                                                , (.)
                                                )
import           Data.Time                      ( Day
                                                , getZonedTime
                                                , localDay
                                                , zonedTimeToLocalTime
                                                )

todaysDate :: IO Day
todaysDate = localDay . zonedTimeToLocalTime <$> getZonedTime
