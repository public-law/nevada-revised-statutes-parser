{-# LANGUAGE DeriveGeneric #-}

import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           Data.Function            ((&))
import           GHC.Generics


data Container =
  Container {
    badge   :: Integer,
    details :: [Integer]
  } deriving (Generic, Show)

instance ToJSON Container


main =
  [1, 10, 20, 30, 2, 99, 99, 99]
    & buildContainerList
    & encodePretty
    & B.putStr


buildContainerList :: [Integer] -> [Container]
buildContainerList numbers =
  buildList addNumberToContainers numbers


buildList :: ([b] -> a -> [b]) -> [a] -> [b]
buildList appendFunction items =
  reverse $ foldl appendFunction [] items


addNumberToContainers :: [Container] -> Integer -> [Container]
addNumberToContainers containers item =
  if isHeader item
    then addNew item containers
    -- TODO: Refactor this; it 'adds' an item to the most recent container:
    else Container {badge=(badge . head) containers, details=(details . head) containers ++ [item] } : tail containers


isHeader :: Integer -> Bool
isHeader thing =
  thing < 10


addNew :: Integer -> [Container] -> [Container]
addNew name containers =
  Container {badge=name, details=[]} : containers
