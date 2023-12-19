module Main (main) where

import Lib

main :: IO ()
main = do
    putStrLn "Enter a directory to analyze: "
    directory <- getLine
    stats <- analyzeDirectoryWithSimpleProcessor directory
    putStrLn $ "Analysing directory: " ++ directory
    putStrLn $ "Total size: " ++ prettyPrintSize (totalSize stats)
    putStrLn $ "Total files: " ++ show (totalFiles stats)
    putStrLn $ "File type counts: " ++ show (sortFileTypeCounts DESC (fileTypeCounts stats))
