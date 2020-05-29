module HtmlUtil where

import           BasicPrelude
import           Data.Aeson        (ToJSON)
import qualified Data.Text         as T
import           GHC.Generics      (Generic)
import           Text.HTML.TagSoup (Tag, innerText, partitions, (~/=), (~==))

import           FileUtil          (AbsolutePath, fixture, readFileLatin1,
                                    toFilePath)

type Node = [Tag Text]

htmlFixture :: FilePath -> IO Html
htmlFixture fname = do
  text <- readFileLatin1 (fixture fname)
  return $ makeHtml text

readHtmlFile :: AbsolutePath -> IO Html
readHtmlFile file = NewHtml <$> readFileLatin1 (toFilePath file)

-- Return the text content of an HTML title.
titleText :: [Tag Text] -> Either String Text
titleText tags =
  case findFirst "<title>" tags of
    Right (_:y:_) -> Right $ innerText [y]
    _             -> Left "Couldn't parse the title"

-- Return the first occurrence of an HTML tag within the given
-- HTML chunk.
findFirst :: Text -> [Tag Text] -> Either String Node
findFirst searchTerm tags =
  case findAll searchTerm tags of
    (x:_) -> Right x
    _     -> Left "Couldn't find it"

-- Return all the occurrences of an HTML tag within the given
-- HTML chunk.
findAll :: Text -> Node -> [Node]
findAll searchTerm = partitions (~== T.unpack searchTerm)

shaveBackTagsToLastClosingP :: Node -> Node
shaveBackTagsToLastClosingP input =
  reverse $ dropWhile (~/= ("</p>" :: String)) $ reverse input

--
-- HTML New Type
--
newtype Html =
  NewHtml Text
  deriving (Eq, Generic, IsString, Show)

instance ToJSON Html

makeHtml :: Text -> Html
makeHtml = NewHtml

toText :: Html -> Text
toText (NewHtml t) = t
