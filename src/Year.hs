{-# LANGUAGE DeriveGeneric #-}

module Year (Year, toYear, fromYear) where

import           BasicPrelude
import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)


newtype Year = MakeYear Integer
  deriving ( Generic, Show )

instance ToJSON Year


toYear :: Integer -> Year
toYear i | i < 1800  = error "Can't create years before 1800"
         | i > 2025  = error "Can't create years after 2025"
         | otherwise = MakeYear i


fromYear :: Year -> Integer
fromYear (MakeYear i) = i
