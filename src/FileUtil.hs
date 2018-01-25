{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FileUtil where

import           BasicPrelude
import           Data.ByteString    (hGetContents)
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeLatin1)
import           System.IO          (IOMode (ReadMode), withFile)
import           System.Process


-- Does not work when invoked from the fish shell.
-- Not sure why not.
readFileLatin1 :: FilePath -> IO Text
readFileLatin1 pathname =
    fmap decodeLatin1 (withFile pathname ReadMode hGetContents)


-- Accepts encodings such as LATIN1.
-- Not currently in use.
readFileAsUtf8 :: FilePath -> String -> IO Text
readFileAsUtf8 pathname sourceEncoding = do
    let stdin' = ""
    stdout' <- readProcess "iconv" ["-f", sourceEncoding, "-t", "utf-8", pathname] stdin'
    return $ T.pack stdout'


-- Compute a full fixture path
fixture :: FilePath -> FilePath
fixture filename =
    "test/fixtures/" ++ filename


newtype Filename = NewFilename Text
    deriving ( IsString, Show )
