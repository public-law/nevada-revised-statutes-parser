{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HtmlUtil  where

import           BasicPrelude
import           Text.HTML.TagSoup (Tag, innerText, partitions, (~/=), (~==))

import           FileUtil          (AbsolutePath, readFileLatin1, toFilePath)


readHtmlFile :: AbsolutePath -> IO Html
readHtmlFile file = NewHtml <$> (readFileLatin1 $ toFilePath file)


-- Return the text content of an HTML title.
titleText :: [Tag Text] -> Text
titleText tags = innerText [findFirst "<title>" tags !! 1]


-- Return the first occurrence of an HTML tag within the given
-- HTML chunk.
findFirst ∷ String → [Tag Text] → [Tag Text]
findFirst searchTerm html =
    head $ findAll searchTerm html


-- Return all the occurrences of an HTML tag within the given
-- HTML chunk.
findAll ∷ String → [Tag Text] → [[Tag Text]]
findAll searchTerm =
    partitions (~== searchTerm)


shaveBackTagsToLastClosingP :: [Tag Text] -> [Tag Text]
shaveBackTagsToLastClosingP input =
    reverse $ dropWhile (~/= "</p>") $ reverse input


--
-- HTML New Type
--
newtype Html = NewHtml Text
    deriving ( IsString, Show )

toText :: Html -> Text
toText (NewHtml t) = t
