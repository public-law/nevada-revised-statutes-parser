{-# LANGUAGE DeriveGeneric #-}

import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           GHC.Generics


data Container =
  Container {
    badge   :: Integer,
    details :: [Integer]
  } deriving (Generic, Show)

instance ToJSON Container


main =
  B.putStr $ encodePretty $ shallowContainerTree [1, 10, 20, 30, 2, 99, 99, 99]


shallowContainerTree :: [Integer] -> [Container]
shallowContainerTree = shallowTree addNumberToContainers


shallowTree :: ([b] -> a -> [b]) -> [a] -> [b]
shallowTree func items =
  reverse $ foldl func [] items


addNumberToContainers :: [Container] -> Integer -> [Container]
addNumberToContainers containers item =
  if isHeader item
    then addNew containers item
    -- TODO: Refactor this; it 'adds' an item to the most recent container:
    else Container {badge=(badge . head) containers, details=(details . head) containers ++ [item] } : tail containers


isHeader :: Integer -> Bool
isHeader thing =
  thing < 10


addNew :: [Container] -> Integer -> [Container]
addNew containers name =
  Container {badge=name, details=[]} : containers
