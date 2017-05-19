{-# LANGUAGE OverloadedStrings #-}

module FileUtil where

import           BasicPrelude
import           Data.Text (pack)
import           System.Process


-- Accepts encodings such as LATIN1.
readFileAsUtf8 :: String -> String -> IO Text
readFileAsUtf8 pathname sourceEncoding = do
  let stdin' = ""
  stdout' <- readProcess "iconv" ["-f", sourceEncoding, "-t", "utf-8", pathname] stdin'
  return $ pack stdout'
