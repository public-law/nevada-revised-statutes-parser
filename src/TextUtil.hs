{-# LANGUAGE OverloadedStrings #-}

module TextUtil where

import          BasicPrelude
import          Data.Text                       (replace, toLower)
import          Data.Text.Titlecase             (titlecase)
import          Data.Text.Titlecase.Internal    (unTitlecase)
import          Text.HTML.TagSoup


titleize :: Text -> Text
titleize = unTitlecase . titlecase . toLower


isHyphen :: Char -> Bool
isHyphen '-' = True
isHyphen  _  = False


normalizedInnerText :: [Tag Text] -> Text
normalizedInnerText = fixUnicodeChars . normalizeWhiteSpace . innerText


normalizeWhiteSpace :: Text -> Text
normalizeWhiteSpace = unwords . words


fixUnicodeChars :: Text -> Text
fixUnicodeChars = (replace "\147" "\8220") . (replace "\148" "\8221")
