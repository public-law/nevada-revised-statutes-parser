{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FileUtil where

import           BasicPrelude
import           Data.ByteString    (hGetContents)
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeLatin1)
import qualified System.Directory   as Dir
import           System.FilePath    (isAbsolute, isRelative)
import           System.IO          (IOMode (ReadMode), withFile)
import           System.Process



listFilesInDirectory :: AbsolutePath -> IO [AbsolutePath]
listFilesInDirectory dir = do
    rawList <- Dir.listDirectory $ toFilePath dir
    paths   <- filterM Dir.doesFileExist (map ((toFilePath dir) </>) rawList)
    return $ map toAbsolutePath paths


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

--
-- AbsolutePath and RelativePath
--

newtype AbsolutePath = MakeAbsolutePath FilePath
    deriving ( Eq, Hashable, IsString, Show )

toAbsolutePath :: FilePath -> AbsolutePath
toAbsolutePath p | isAbsolute p = MakeAbsolutePath p
                 | otherwise = error "Not an absolute path"

toFilePath :: AbsolutePath -> FilePath
toFilePath (MakeAbsolutePath p) = p


newtype RelativePath = MakeRelativePath FilePath
    deriving ( Eq, Hashable, IsString, Show )

toRelativePath :: FilePath -> RelativePath
toRelativePath p | isRelative p = MakeRelativePath p
                 | otherwise = error "Not a relative path"
