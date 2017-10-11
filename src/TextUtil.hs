{-# LANGUAGE OverloadedStrings #-}

module TextUtil where

import           BasicPrelude
import           Data.Char         (isAlpha)
import qualified Data.Text         as T
import           Text.HTML.TagSoup


minorWords :: [Text]
minorWords  = ["a", "an", "and", "at", "but", "by", "for", "from", "in", "nor", "of", "on", "or", "out", "so", "the", "to", "up", "yet"]


titleize :: Text -> Text
titleize phrase =
    unwords (firstWord : remainingWords)
    where (x:xs) = words phrase
          firstWord      = titleizeWord x
          remainingWords = map conditionallyTitleizeWord xs


conditionallyTitleizeWord :: Text -> Text
conditionallyTitleizeWord word =
    if isUsuallyUncapitalized word
        then T.toLower word
        else titleizeWord word


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
