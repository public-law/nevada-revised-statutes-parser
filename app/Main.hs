{-# LANGUAGE DeriveGeneric #-}

import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (Config, encodePretty)
import qualified Data.ByteString.Lazy     as B
import           GHC.Generics


data Container =
  Container {
    badge   :: Integer,
    details :: [Integer]
  } deriving (Generic, Show)

instance ToJSON Container


main = do
  let containers = shallowTree [1, 10, 20, 30, 40, 50, 2, 99, 99, 99]
  B.putStr (encodePretty containers)


shallowTree :: [Integer] -> [Container]
shallowTree items =
  reverse $ foldl addItem [] items


addItem :: [Container] -> Integer -> [Container]
addItem containers item =
  if isHeader item
    then addNew containers item
    else Container {badge=(badge . head) containers, details=(details . head) containers ++ [item] } : tail containers


isHeader thing =
  thing < 10


addNew :: [Container] -> Integer -> [Container]
addNew containers name =
  Container {badge=name, details=[]} : containers
