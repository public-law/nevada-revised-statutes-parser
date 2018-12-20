module Year
  ( Year
  , toYear
  , fromYear
  )
where

import           BasicPrelude                   ( Integer
                                                , Show
                                                , error
                                                , otherwise
                                                , (<)
                                                , (>)
                                                )
import           Data.Aeson                     ( ToJSON )
import           GHC.Generics                   ( Generic )
import           Text.InterpolatedString.Perl6  ( qq )


toYear :: Integer -> Year
toYear i | i < 1800  = error [qq| Can't create years before 1800: $i |]
         | i > 2025  = error [qq| Can't create years after 2025: $i |]
         | otherwise = MakeYear i


fromYear :: Year -> Integer
fromYear (MakeYear i) = i


newtype Year = MakeYear Integer deriving ( Generic, Show )
instance ToJSON Year
