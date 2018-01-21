{-# LANGUAGE OverloadedStrings #-}

module TextUtil where

import           BasicPrelude
import           Data.Char         (isAlpha)
import qualified Data.Text         as T
import           Text.HTML.TagSoup


-- Words which are not usually capitalized in titles.
minorWords :: [Text]
minorWords  = ["a", "an", "and", "at", "but", "by", "for", "from", "in", "nor", "of", "on", "or", "out", "so", "the", "to", "up", "yet"]


titleize :: Text -> Text
titleize phrase =
    unwords (firstWord : remainingWords)
    where (x:xs) = words phrase
          firstWord      = titleizeWord x
          remainingWords = map conditionallyTitleizeWord xs


-- Titleize the word only if it's usually capitalized. Else,
-- return it in lower case.
conditionallyTitleizeWord :: Text -> Text
conditionallyTitleizeWord word =
    if isUsuallyUncapitalized word
        then T.toLower word
        else titleizeWord word


-- Titleize the word without being fooled by leading punctuation.
-- I.e., Convert the first alpha character to upper case.
titleizeWord :: Text -> Text
titleizeWord word =
    let (punctuation, remainder) = T.span (not . isAlpha) word
    in punctuation ++ T.toTitle remainder


isUsuallyUncapitalized :: Text -> Bool
isUsuallyUncapitalized word =
    T.toLower word `elem` minorWords


isHyphen :: Char -> Bool
isHyphen '-' = True
isHyphen  _  = False


normalizedInnerText :: [Tag Text] -> Text
normalizedInnerText = fixUnicodeChars . normalizeWhiteSpace . innerText


normalizeWhiteSpace :: Text -> Text
normalizeWhiteSpace = unwords . words


fixUnicodeChars :: Text -> Text
fixUnicodeChars = T.replace "\147" "\8220" . T.replace "\148" "\8221"
