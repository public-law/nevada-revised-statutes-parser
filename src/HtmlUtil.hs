{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HtmlUtil  where

import           BasicPrelude
import           Text.HTML.TagSoup (Tag, innerText, partitions, (~/=), (~==))

import           FileUtil          (AbsolutePath, readFileLatin1, toFilePath)


readHtmlFile :: AbsolutePath -> IO Html
readHtmlFile file = NewHtml <$> (readFileLatin1 $ toFilePath file)


-- Return the text content of an HTML title.
-- TODO: Make safe by using pattern matching.
titleText :: [Tag Text] -> Text
titleText tags
    | (length (findFirst "<title>" tags) >= 2) = innerText [findFirst "<title>" tags !! 1]
    | otherwise                                = error $ "Not a title: " ++ (show tags)


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

makeHtml :: Text -> Html
makeHtml text = NewHtml text

toText :: Html -> Text
toText (NewHtml t) = t
