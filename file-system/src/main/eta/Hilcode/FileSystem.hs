module Hilcode.FileSystem (
    FileEntry(..),
    listFiles
) where

import Data.Strings (sEndsWith, sLen, sTake)

import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath (FilePath, (</>))
import qualified System.FilePath as FilePath
import qualified System.Directory as Directory
import Control.Monad (liftM)

import Hilcode.ShowText (ShowText(..))

newtype RelativeToCurrentDir
    = RelativeToCurrentDir FilePath
        deriving (Eq, Ord, Show)
newtype RelativeToRootDir
    = RelativeToRootDir FilePath
        deriving (Eq, Ord, Show)

data Path
    = Root Text
    | Path Path Text
        deriving (Eq, Ord, Show)

toFilePath :: Path -> FilePath
toFilePath (Root filePath)
    = Text.unpack filePath ++ [FilePath.pathSeparator]
toFilePath (Path path filePath)
    = toFilePath path ++ Text.unpack filePath ++ [FilePath.pathSeparator]

data FileEntry
    = Directory Path Text [FileEntry]
    | File Path Text
        deriving (Eq, Ord, Show)

listFiles :: Text -> IO [FileEntry]
listFiles filePath
    = if filePath `sEndsWith` "/"
        then listFiles' (Root (sTake ((sLen filePath) - 1) filePath))
        else listFiles' (Root filePath)

listFiles' :: Path -> IO [FileEntry]
listFiles' path' = do
    filePaths' <- x path'
    sequence $ z path' filePaths'
      where
        x :: Path -> IO [FilePath]
        x path = Directory.listDirectory $ toFilePath path
        z :: Path -> [FilePath] -> [IO FileEntry]
        z path filePaths = do
            (y path) <$> filePaths
        y :: Path -> FilePath -> IO FileEntry
        y path filePath = do
            isDirectory <- Directory.doesDirectoryExist $ (toFilePath path) ++ filePath
            if isDirectory
                then mkDirectory path (Text.pack filePath)
                else return $ mkFile path (Text.pack filePath)
        mkFile :: Path -> Text -> FileEntry
        mkFile path fileName
            = File path fileName
        mkDirectory :: Path -> Text -> IO FileEntry
        mkDirectory path dirName = do
            fileEntries <- listFiles' (Path path dirName)
            return $ Directory path dirName fileEntries
