module FileUtil where

import           BasicPrelude                   ( Eq
                                                , FilePath
                                                , Hashable
                                                , IO
                                                , IsString
                                                , Show
                                                , Text
                                                , filterM
                                                , otherwise
                                                , return
                                                , ($)
                                                , (++)
                                                , (<$>)
                                                , (</>)
                                                , (.)
                                                )
import qualified BasicPrelude                  as BasicPrelude
import           Data.ByteString                ( hGetContents )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeLatin1 )
import qualified System.Directory              as Dir
import           System.FilePath                ( isAbsolute
                                                , isRelative
                                                )
import           System.IO                      ( IOMode(ReadMode)
                                                , withFile
                                                )


-- Re-write error() to work with Text instead of String.
error :: Text -> a
error = BasicPrelude.error . T.unpack


listFilesInDirectory :: AbsolutePath -> IO [AbsolutePath]
listFilesInDirectory dir = do
  rawList <- Dir.listDirectory $ toFilePath dir
  paths   <- filterM Dir.doesFileExist ((toFilePath dir </>) <$> rawList)
  return $ toAbsolutePath <$> paths


readFileLatin1 :: FilePath -> IO Text
readFileLatin1 pathname =
  decodeLatin1 <$> withFile pathname ReadMode hGetContents



-- Compute a full fixture path
fixture :: FilePath -> FilePath
fixture filename = "test/fixtures/" ++ filename


--
-- AbsolutePath and RelativePath
--

newtype AbsolutePath = MakeAbsolutePath FilePath
    deriving ( Eq, Hashable, IsString, Show )

toAbsolutePath :: FilePath -> AbsolutePath
toAbsolutePath p | isAbsolute p = MakeAbsolutePath p
                 | otherwise    = error "Not an absolute path"

(//) :: FilePath -> AbsolutePath
(//) = toAbsolutePath

-- TODO: Move to separate file so that RelativePath can also have
-- a toFilePath.
toFilePath :: AbsolutePath -> FilePath
toFilePath (MakeAbsolutePath fp) = fp


newtype RelativePath = MakeRelativePath FilePath
    deriving ( Eq, Hashable, IsString, Show )

toRelativePath :: FilePath -> RelativePath
toRelativePath p | isRelative p = MakeRelativePath p
                 | otherwise    = error "Not a relative path"

(./) :: FilePath -> RelativePath
(./) = toRelativePath
