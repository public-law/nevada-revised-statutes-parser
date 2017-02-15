{-# LANGUAGE DeriveGeneric #-}

module Year (Year, toYear, fromYear) where

import           BasicPrelude
import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)


newtype Year = MakeYear Integer
  deriving ( Generic, Show )

instance ToJSON Year


toYear :: Integer -> Year
toYear x | x < 1800  = error "Can't create years before 1800"
         | x > 2025  = error "Can't create years after 2025"
         | otherwise = MakeYear x


fromYear :: Year -> Integer
fromYear (MakeYear y) = y
