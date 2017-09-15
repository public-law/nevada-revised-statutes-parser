{-# LANGUAGE DeriveGeneric #-}

module Year (Year, toYear, fromYear) where

-- TODO: Fix this column-spacing
import           BasicPrelude
import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)


-- TODO: Include value i in error message
toYear :: Integer -> Year
toYear i | i < 1800  = error "Can't create years before 1800"
         | i > 2025  = error "Can't create years after 2025"
         | otherwise = MakeYear i


fromYear :: Year -> Integer
fromYear (MakeYear i) = i


newtype Year = MakeYear Integer deriving ( Generic, Show )
instance ToJSON Year
