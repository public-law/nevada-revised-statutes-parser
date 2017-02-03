{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (confCompare, defConfig,
                                           encodePretty', keyOrder)
import qualified Data.ByteString.Lazy     as B
import           Data.Function            ((&))
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
buildContainerList = buildList addNumberToContainers


buildList :: ([container] -> elem -> [container]) -> [elem] -> [container]
buildList appendFunction elements =
  reverse $ foldl appendFunction [] elements


addNumberToContainers :: [Container] -> Integer -> [Container]
addNumberToContainers containers item =
  if isHeader item
    then addNew item containers
    -- TODO: Refactor this; it 'adds' an item to the most recent container:
    else Container {header=(header . head) containers, body=(body . head) containers ++ [item] } : tail containers


isHeader :: Integer -> Bool
isHeader thing =
  thing < 10


addNew :: Integer -> [Container] -> [Container]
addNew name containers =
  Container {header=name, body=[]} : containers


toJson =
  encodePretty' (defConfig {confCompare=keyOrder ["header"]})
