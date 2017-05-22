{-# LANGUAGE OverloadedStrings #-}

module FileUtil where

import           BasicPrelude
import           Data.Text (pack)
import           System.Process

import System.IO (withFile, IOMode(ReadMode))
import Data.ByteString (hGetContents)
import Data.Text.Encoding (decodeLatin1)



readFileLatin1 :: FilePath -> IO Text
readFileLatin1 pathname =
    fmap decodeLatin1 (withFile pathname ReadMode hGetContents)


-- Accepts encodings such as LATIN1.
-- Not currently in use.
readFileAsUtf8 :: String -> String -> IO Text
readFileAsUtf8 pathname sourceEncoding = do
  let stdin' = ""
  stdout' <- readProcess "iconv" ["-f", sourceEncoding, "-t", "utf-8", pathname] stdin'
  return $ pack stdout'


fixture :: String -> String
fixture filename =
  "test/fixtures/" ++ filename
