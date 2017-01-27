import           Text.HandsomeSoup
import           Text.XML.HXT.Core


data Title = Title {
  header :: Char,
  body   :: [Char]
} deriving (Show)

main = do
  html <- readFile "nrs.html"
  print (foldl addRawData [] ['A', 'a', 'b', 'c', 'd', 'B', '1', '2', '3'])


addRawData :: [Title] -> Char -> [Title]
addRawData titles aLine =
  if isHeader aLine
    then Title {header=aLine, body=[]} : titles
    else Title {header=(header . head) titles, body=(body . head) titles ++ [aLine] } : tail titles


isHeader :: Char -> Bool
isHeader ch =
  ch == 'A' || ch == 'B'
