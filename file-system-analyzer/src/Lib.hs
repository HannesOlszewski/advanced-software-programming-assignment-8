module Lib (
    FileStats(..),
    emptyStats,
    updateStats,
    combineStats,
    analyzeDirectoryWithSimpleProcessor,
    prettyPrintSize
) where

import System.Directory
import System.FilePath
import Control.Monad (foldM, forM)
import qualified Data.Map as Map

-- Data structure to hold file statistics
data FileStats = FileStats {
    totalSize :: Integer,
    totalFiles :: Integer,
    fileTypeCounts :: Map.Map String Integer
} deriving (Show, Eq)

-- Initialize empty statistics
emptyStats :: FileStats
emptyStats = FileStats 0 0 Map.empty

-- Function to update statistics with a new file
updateStats :: Integer -> String -> FileStats -> FileStats
updateStats fileSize extension stats =
    let updatedSize = totalSize stats + fileSize
        updatedFiles = totalFiles stats + 1
        updatedCounts = Map.insertWith (+) extension 1 (fileTypeCounts stats)
    in FileStats updatedSize updatedFiles updatedCounts

-- IO function to get file information
getFileInfo :: FilePath -> IO (Integer, String)
getFileInfo path = do
    fileSize <- getFileSize path
    let extension = takeExtension path
    return (fileSize, extension)

-- Function to combine two FileStats structures
combineStats :: FileStats -> FileStats -> FileStats
combineStats a b = FileStats {
    totalSize = totalSize a + totalSize b,
    totalFiles = totalFiles a + totalFiles b,
    fileTypeCounts = Map.unionWith (+) (fileTypeCounts a) (fileTypeCounts b)
}

kb :: Integer
kb = 1024

-- Pretty-print file size
prettyPrintSize :: Integer -> String
prettyPrintSize size
    | size < kb = show size ++ " bytes"
    | size < kb * kb = show (size `div` kb) ++ " KB " ++ prettyPrintSize (size `mod` kb)
    | size < kb * kb * kb = show (size `div` (kb * kb)) ++ " MB " ++ prettyPrintSize (size `mod` (kb * kb))
    | otherwise = show (size `div` (kb * kb * kb)) ++ " GB " ++ prettyPrintSize (size `mod` (kb * kb * kb))

-- Function type for processing a file
type FileProcessor = FilePath -> FileStats -> IO FileStats

-- Higher-order function that applies a given file processor during directory traversal
traverseDirectoryWithProcessor :: FileProcessor -> FilePath -> IO FileStats
traverseDirectoryWithProcessor processor dir = do
    contents <- listDirectory dir
    foldM processFile emptyStats contents
  where
    processFile accStats name = do
        let path = dir </> name
        isDir <- doesDirectoryExist path
        if isDir
            then do
                dirStats <- traverseDirectoryWithProcessor processor path
                return $ combineStats accStats dirStats
            else processor path accStats

-- Example file processor that just updates stats
simpleFileProcessor :: FileProcessor
simpleFileProcessor path stats = do
    (fileSize, extension) <- getFileInfo path
    return $ updateStats fileSize extension stats

-- Usage in main or other functions
analyzeDirectoryWithSimpleProcessor :: FilePath -> IO FileStats
analyzeDirectoryWithSimpleProcessor = traverseDirectoryWithProcessor simpleFileProcessor

