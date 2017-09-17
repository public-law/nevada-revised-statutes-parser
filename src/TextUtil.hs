{-# LANGUAGE OverloadedStrings #-}

module TextUtil where

import          BasicPrelude
import          Data.Char                       (isAlpha)
import          Data.Text                       (replace, span, toLower)
import          Data.Text.Titlecase             (titlecase)
import          Data.Text.Titlecase.Internal    (unTitlecase)
import          Text.HTML.TagSoup


titleize :: Text -> Text
titleize = unTitlecase . titlecase . toLower


titleizeWord :: Text -> Text
titleizeWord word = 
    let (symbols, remainder) = Data.Text.span (not . isAlpha) word
    in symbols ++ (titleize remainder)


isHyphen :: Char -> Bool
isHyphen '-' = True
isHyphen  _  = False


normalizedInnerText :: [Tag Text] -> Text
normalizedInnerText = fixUnicodeChars . normalizeWhiteSpace . innerText


normalizeWhiteSpace :: Text -> Text
normalizeWhiteSpace = unwords . words


fixUnicodeChars :: Text -> Text
fixUnicodeChars = (replace "\147" "\8220") . (replace "\148" "\8221")
