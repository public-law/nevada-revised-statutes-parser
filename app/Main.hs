{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UnicodeSyntax #-}

import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as B
import           GHC.Generics
import           Text.HandsomeSoup
import           Text.XML.HXT.Core


class Treeable a where
  isHeader :: a -> Bool

data Title =
  Title {
    header :: Int,
    body   :: [Int]
  } deriving (Generic, Show)

instance ToJSON Title


main = do
  html <- readFile "nrs.html"
  let doc = readString [withParseHTML yes, withWarnings no] html
  items <- runX $ doc >>> css "td"
  let things = foldl addItem [] [0, 1, 2, 3, 4, 5, 0, 9, 9, 9]
  B.putStr (encodePretty things)


addItem :: [Title] -> Int -> [Title]
addItem things item =
  if isHeader item
    then Title {header=item, body=[]} : things
    else Title {header=(header . head) things, body=(body . head) things ++ [item] } : tail things


instance Treeable Integer where
  isHeader i  =   i == 0
