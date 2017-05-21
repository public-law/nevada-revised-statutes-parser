{-# LANGUAGE OverloadedStrings #-}

module TextUtil where

import          BasicPrelude
import          Data.Text                       (toLower)
import          Data.Text.Titlecase             (titlecase)
import          Data.Text.Titlecase.Internal    (unTitlecase)


titleize :: Text -> Text
titleize = unTitlecase . titlecase . toLower


isHyphen :: Char -> Bool
isHyphen '-' = True
isHyphen  _  = False


normalizeWhiteSpace :: Text -> Text
normalizeWhiteSpace = unwords . words
