module Main where

import qualified System.Environment as Environment
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Hilcode.FileSystem as FileSystem

main :: IO ()
main = do
    argv :: [String] <- Environment.getArgs
    xs <- FileSystem.listFiles (rootDir argv)
    putStrLn $ show xs
      where
        rootDir :: [String] -> Text
        rootDir argv = case argv of
            []      -> "."
            arg : _ -> Text.pack arg

{-
import Prelude hiding (show)
import qualified Text.Show as Show
import qualified Data.Function as Function
import Control.Monad (liftM)
import System.FilePath (FilePath)
import qualified System.FilePath as FilePath
import qualified System.Directory as SysDir
import qualified System.Environment as SysEnv
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Template as Template
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Sequence ( ViewL ((:<)), Seq)
import qualified Data.Sequence as Sequence
import qualified Data.Traversable as Traversable
import qualified Data.Foldable as Foldable
import Data.Dequeue (BankersDequeue)
-}


{-
newtype Leaf a
    = Leaf a
        deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
instance Applicative Leaf where
    pure = Leaf
    (<*>) (Leaf f) (Leaf x) = Leaf (f x)
instance Monad Leaf where
    return = Leaf
    (>>=) (Leaf x) f = f x
   
data Tree a
    = Tree a [Tree a] [Leaf a]
        deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
-}
        {-
        fileDirectoryEntries :: FileEntryPath -> IO [FileEntryPath]
        fileDirectoryEntries directoryPath = do
            filePaths <- (toFileEntryPath directoryPath) <$> (SysDir.listDirectory $ toFilePath directoryPath)
            return filePaths
        toFilePath :: FileEntryPath -> FilePath
        toFilePath fileEntryPath
            = Text.unpack $ showT fileEntryPath
        toFileEntryPath :: FileEntryPath -> FilePath -> IO FileEntryPath
        toFileEntryPath parent filePath = do
            isDirectory <- SysDir.doesDirectoryExist $ (Text.unpack $ toFilePath parent) ++ filePath
            return (Child parent (if isDirectory then IsDirectory else IsFile) (Text.pack filePath))
        mapFileEntry :: FileEntryPath -> Text -> FilePath -> IO [FileEntryPath]
        mapFileEntry parent name filePath = do
            isDirectory <- SysDir.doesDirectoryExist filePath
            if isDirectory
                then return (Child parent IsDirectory name) : \fp -> mapFileEntry (Child parent IsDirectory name) (Text.pack fp) (Text.unpack $ showT (Child parent IsDirectory name) ++ fp) `liftM` fileEntries
        -}

{-
mkTree :: forall a. a -> (a -> ([a], [a])) -> (a -> a -> a) -> Tree a
mkTree origin findDirectBranchesAndLeafs mkNextBranch
    = Tree origin branches leafs
      where
        (branches', leafs') = findDirectBranchesAndLeafs origin
        leafs :: [Leaf a]
        leafs = Leaf <$> leafs'
        mkNextBranch' :: a -> a
        mkNextBranch' = mkNextBranch origin
        mkBranches :: [a] -> (a -> a) -> [Tree a]
        mkBranches directBranches mkNextBranchName = (Tree mkNextBranchName
-}

{-
data File
    = File DirectoryName FileName
        deriving (Eq)
instance ShowText File where
    showT (File dirName fileName) = showT dirName </> showT fileName

data Directory
    = Directory DirectoryName [Directory] [File]
        deriving (Eq)
instance ShowText Directory where
    showT (Directory dirName _ _) = showT dirName

mkDirectory :: FilePath -> IO Directory
mkDirectory filePath
    = listDir filePath
        >>= partitionFilePaths
        >>= mkDirectoryFromPartition filePath

listDir :: FilePath -> IO [FilePath]
listDir filePath = SysDir.listDirectory filePath

data Partition a = Partition [a] [a] deriving (Show, Eq)

instance Semigroup (Partition a) where
    (<>) (Partition lftas rgtas) (Partition lftas' rgtas') = Partition (lftas ++ lftas') (rgtas ++ rgtas')

instance Monoid (Partition a) where
    mempty = Partition [] []

partitionFilePaths :: [FilePath] -> IO (Partition FilePath)
partitionFilePaths files =
    case files of
    [] -> return $ Partition [] []
    file:rest -> do
        isDirectory <- SysDir.doesDirectoryExist file
        partitions <- partitionFilePaths rest
        if isDirectory then do
            return $ Partition [file] [] <> partitions
        else
            return $ Partition [] [file] <> partitions

mkDirectoryFromPartition :: FilePath -> Partition FilePath -> IO Directory
mkDirectoryFromPartition dirName (Partition dirNamesInDir fileNamesInDir)
    = z
      where
        directoryName :: DirectoryName
        directoryName = DirectoryName (Text.pack dirName)
        files :: [File]
        files
            = (File directoryName) . FileName . Text.pack <$> fileNamesInDir
        mapFilePath2Directory :: FilePath -> IO Directory
        mapFilePath2Directory filePath
            = zz filePath
        xx :: FilePath -> Partition FilePath -> IO Directory
        xx filePath
            = mkDirectoryFromPartition (dirName ++ [FilePath.pathSeparator] ++ filePath)
        yy :: FilePath -> [FilePath] -> IO Directory
        yy filePath filePaths
            = partitionFilePaths filePaths >>= (xx filePath)
        zz :: FilePath -> IO Directory
        zz filePath
            = listDir filePath >>= (yy filePath)
        toDirectory :: [File] -> [Directory] -> Directory
        toDirectory files' dirs'
            = Directory directoryName dirs' files'
        x :: [Directory] -> Directory
        x subdirs'
            = toDirectory files subdirs'
        y :: IO [Directory]
        y
            = sequence $ mapFilePath2Directory <$> dirNamesInDir
        z :: IO Directory
        z
            = liftM x y

data First
    = IsFirst
    | IsNotFirst
        deriving (Show)
data Last
    = IsLast
    | IsNotLast
        deriving (Show)
data LoopItem a
    = LoopItem Int First Last a
        deriving (Show, Functor)

loop :: (LoopItem a -> b) -> [a] -> [b]
loop function source
    = function <$> firstLoopStep source
      where
        firstLoopStep :: [a] -> [LoopItem a]
        firstLoopStep []     = []
        firstLoopStep [x]    = [LoopItem 1 IsFirst IsLast x]
        firstLoopStep (x:xs) = (LoopItem 1 IsFirst IsNotLast x) : nextLoopStep 1 xs
        nextLoopStep :: Int -> [a] -> [LoopItem a]
        nextLoopStep _     []     = []
        nextLoopStep index [x]    = [LoopItem (index + 1) IsNotFirst IsLast x]
        nextLoopStep index (x:xs) = LoopItem (index + 1) IsNotFirst IsNotLast x : nextLoopStep (index + 1) xs

flatLoop :: (LoopItem a -> [b]) -> [a] -> [b]
flatLoop function source
    = concat $ function <$> firstLoopStep source
      where
        firstLoopStep :: [a] -> [LoopItem a]
        firstLoopStep []     = []
        firstLoopStep [x]    = [LoopItem 1 IsFirst IsLast x]
        firstLoopStep (x:xs) = (LoopItem 1 IsFirst IsNotLast x) : nextLoopStep 1 xs
        nextLoopStep :: Int -> [a] -> [LoopItem a]
        nextLoopStep _     []     = []
        nextLoopStep index [x]    = [LoopItem (index + 1) IsNotFirst IsLast x]
        nextLoopStep index (x:xs) = LoopItem (index + 1) IsNotFirst IsNotLast x : nextLoopStep (index + 1) xs


(</>) :: Text -> Text -> Text
(</>) lftPath rgtPath = lftPath <> Text.singleton FilePath.pathSeparator <> rgtPath

linesToText :: [Line] -> Text
linesToText []          = ""
linesToText (line:rest) = showT line <> "\n" <> linesToText rest

tree :: Directory -> Text
tree directory
    = treeToText [] directory
      where
        treeToText :: [Prefix] -> Directory -> Text
        treeToText prefixes (Directory (DirectoryName directoryName) dirs files)
            = linesToText $ (Line prefixes directoryName) : ((flatLoop (doSubdirectory prefixes) dirs) ++ (loop (doFile prefixes) files))
        doSubdirectory :: [Prefix] -> LoopItem Directory -> [Line]
        doSubdirectory prefixes (LoopItem _ _ IsLast (Directory (DirectoryName dirName) dirs files))
            = (Line (prefixes ++ [if noFiles files then Last else Normal]) dirName) : ((flatLoop (doSubdirectory (prefixes ++ [WTF_2])) dirs) ++ (loop (doFile (prefixes ++ [Continue])) files))
        doSubdirectory prefixes (LoopItem _ _ IsNotLast (Directory (DirectoryName dirName) dirs files))
            = (Line (prefixes ++ [Normal]) dirName) : ((flatLoop (doSubdirectory (prefixes ++ [WTF_5])) dirs) ++ (loop (doFile (prefixes ++ [Continue])) files))
        doFile :: [Prefix] -> LoopItem File -> Line
        doFile prefixes (LoopItem _ _ IsNotLast (File _ (FileName fileName)))
            = Line (prefixes ++ [Normal]) fileName
        doFile prefixes (LoopItem _ _ IsLast (File _ (FileName fileName)))
            = Line (prefixes ++ [Last]) fileName
        noFiles :: [File] -> Bool
        noFiles files
            = case files of
                [] -> True
                _  -> False
            
data Line
    = Line [Prefix] Text

instance ShowText Line where
    showT (Line prefixes text)
        = toText prefixes <> text
      where
        toText :: [Prefix] -> Text
        toText []            = ""
        toText (prefix:rest) = showT prefix <> toText rest

data Prefix
    = Normal
    | Continue
    | Last
    | Empty
    | WTF_1
    | WTF_2
    | WTF_3
    | WTF_4
    | WTF_5
    | WTF_6
        deriving (Eq)

instance ShowText Prefix where
    showT Normal
        = "├── "
    showT Continue
        = "│   "
    showT Last
        = "└── "
    showT Empty
        = "    "
    showT WTF_1
        = "111 "
    showT WTF_2
        = "222 "
    showT WTF_3
        = "333 "
    showT WTF_4
        = "444 "
    showT WTF_5
        = "555 "
    showT WTF_6
        = "666 "
-}
{-
src
|-- main
|   \-- eta
|       |-- Main.hs
|       \-- template
\-- test
    \-- eta
        \-- Main.hs

src
NNN main
CCC LLL eta
CCC EEE NNN Main.hs
CCC EEE LLL template
LLL test
EEE LLL eta
EEE EEE LLL Main.hs
-}
{-
main' :: IO ()
main' = do
    argv :: [String] <- SysEnv.getArgs
    files <- SysDir.listDirectory $ rootDir argv
    putStrLn $ concat files
      where
        rootDir argv = case argv of
            []      -> "."
            arg : _ -> arg
-}
{-
main :: IO ()
main = do
    argv :: [String] <- SysEnv.getArgs
    directory :: Directory <- mkDirectory $ rootDir argv
    TextIO.putStrLn $ tree directory
--    filePaths' <- Text.pack <$> filePaths
--    return $ TextIO.putStrLn <$> filePaths'
--    TextIO.putStrLn $ Template.toText template
--      where
--        template = Template.parse "-----\nhello\nworld\n!"
      where
        rootDir argv = case argv of
            []      -> "."
            arg : _ -> arg
-}
