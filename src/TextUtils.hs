{-# LANGUAGE OverloadedStrings #-}

module TextUtils where

import          BasicPrelude
import          Data.Text                       (toLower)
import          Data.Text.Titlecase             (titlecase)
import          Data.Text.Titlecase.Internal    (unTitlecase)


titleize :: Text -> Text
titleize = unTitlecase . titlecase . toLower


isHyphen :: Char -> Bool
isHyphen c = c == '-'
