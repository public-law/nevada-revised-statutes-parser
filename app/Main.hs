{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (confCompare, defConfig,
                                           encodePretty', keyOrder)
import qualified Data.ByteString.Lazy     as B
import           Data.Function            ((&))
import           Data.List.Split          (chunksOf, split, whenElt)
import           GHC.Generics


data Container =
  Container {
    header :: Integer,
    body   :: [Integer]
  } deriving (Generic, Show)

instance ToJSON Container


main =
  [1, 10, 20, 30, 2, 99, 99, 99]
    & buildContainerList
    & toJson
    & B.putStr


buildContainerList :: [Integer] -> [Container]
buildContainerList items =
  map newContainerFromTuple (chunksOf 2 $ tail $ split (whenElt (< 10)) items)


newContainerFromTuple :: [[Integer]] -> Container
newContainerFromTuple tuple =
  Container {header=head (head tuple), body=head (tail tuple)}


toJson =
  encodePretty' (defConfig {confCompare=keyOrder ["header"]})
