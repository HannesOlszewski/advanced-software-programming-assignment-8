module Main (main) where

import Lib
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))
import Control.Exception (IOException, catch)

main :: IO ()
main = do
    catch (do
        args <- getArgs
        let directory = head args
        stats <- analyzeDirectoryWithSimpleProcessor directory
        putStrLn $ "Analysing directory: " ++ directory
        putStrLn $ "Total size: " ++ prettyPrintSize (totalSize stats)
        putStrLn $ "Total files: " ++ show (totalFiles stats)
        -- putStrLn $ "File type counts: " ++ show (sortFileTypeCounts DESC (fileTypeCounts stats))
        putStrLn $ "Top three file types: " ++ show (take 3 (sortFileTypeCounts DESC (fileTypeCounts stats)))
        )
        (\e -> do
            let err = show (e :: IOException)
            hPutStrLn stderr ("Error (in catch): " ++ err)
            exitWith (ExitFailure 1)
        )

