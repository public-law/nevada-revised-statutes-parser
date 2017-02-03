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
  split (whenElt (< 10)) items
    & tail
    & chunksOf 2
    & map newContainerFromTuple


-- Input is a tuple with the header and content, like this:
-- [[0], [10, 20, 30]]
newContainerFromTuple :: [[Integer]] -> Container
newContainerFromTuple tuple =
  let name    = head (head tuple)
      content = head (tail tuple)
  in
    Container {header=name, body=content}


toJson =
  encodePretty' (defConfig {confCompare=keyOrder ["header"]})
