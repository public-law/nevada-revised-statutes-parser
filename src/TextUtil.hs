{-# LANGUAGE OverloadedStrings #-}

module TextUtil where

import          BasicPrelude
import          Data.Char                       (isAlpha)
import          Data.Text                       (replace, span, toLower, unwords, words)
import          Data.Text.Titlecase             (titlecase)
import          Data.Text.Titlecase.Internal    (unTitlecase)
import          Text.HTML.TagSoup


dontCapitalize :: [Text]
dontCapitalize  = ["a", "an", "and", "at", "but", "by", "for", "in", "nor", "of", "on", "or", "out", "so", "the", "to", "up", "yet"]


shouldCapitalize :: Text -> Bool
shouldCapitalize word = 
    notElem (toLower word) dontCapitalize


titleize :: Text -> Text
titleize phrase =
    unwords $ fmap titleizeWord (words phrase)


titleizeWord :: Text -> Text
titleizeWord word = 
    let (symbols, remainder) = Data.Text.span (not . isAlpha) word
    in symbols ++ (upcaseFirst remainder)


upcaseFirst :: Text -> Text
upcaseFirst = unTitlecase . titlecase . toLower

        
isHyphen :: Char -> Bool
isHyphen '-' = True
isHyphen  _  = False


normalizedInnerText :: [Tag Text] -> Text
normalizedInnerText = fixUnicodeChars . normalizeWhiteSpace . innerText


normalizeWhiteSpace :: Text -> Text
normalizeWhiteSpace = unwords . words


fixUnicodeChars :: Text -> Text
fixUnicodeChars = (replace "\147" "\8220") . (replace "\148" "\8221")
