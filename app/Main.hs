import           Text.HandsomeSoup
import           Text.XML.HXT.Core


data Title = Title {
  header :: Int,
  body   :: [Int]
} deriving (Show)


main = do
  html <- readFile "nrs.html"
  print (foldl addItem [] [0, 1, 2, 3, 4, 5, 0, 9, 9, 9])


addItem :: [Title] -> Int -> [Title]
addItem things item =
  if isHeader item
    then Title {header=item, body=[]} : things
    else Title {header=(header . head) things, body=(body . head) things ++ [item] } : tail things


isHeader :: Int -> Bool
isHeader n =
  n == 0
