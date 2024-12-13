module Main where

import System.Directory (listDirectory)
import System.FilePath ((</>), takeFileName)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Configuration: adjust these as needed
mainScript :: FilePath
mainScript = "Main.hs"

examplesDir :: FilePath
examplesDir = "examples"

stdoutDir :: FilePath
stdoutDir = "examples_stdout"

-- Get all files in the examples directory except while.c that do not finish
getFiles :: IO [FilePath]
getFiles = do
    contents <- listDirectory examplesDir  -- Get all files in the current directory
    return $ filter (\f -> f /= "while.c" && not (f `elem` [".", ".."])) contents  

-- Run the Haskell script on a given input file
runScriptOnFile :: FilePath -> IO (ExitCode, String, String)
runScriptOnFile inputFile = 
    readProcessWithExitCode "cabal" ["run", mainScript, examplesDir </> inputFile] ""

-- Compare output with reference file
compareWithReferenceFile :: FilePath -> String -> IO Bool
compareWithReferenceFile inputFile stdout = do
    let refFilePath = stdoutDir </> takeFileName inputFile
    refContent <- TIO.readFile refFilePath
    let cleanedStdout = T.strip (T.pack stdout)
    let cleanedRefContent = T.strip refContent
    return $ cleanedStdout == cleanedRefContent

processFiles :: IO ()
processFiles = do
    files <- getFiles
    results <- mapM processFile files
    mapM_ printResult (zip files results)
  where
    processFile file = do
        (exitCode, stdout, stderr) <- runScriptOnFile file -- Run the script
        comparisonResult <- compareWithReferenceFile file stdout -- Compare with reference file
        comparisonResultStderr <- compareWithReferenceFile file stderr -- Compare with reference file
        return (exitCode, stdout, stderr, comparisonResult, comparisonResultStderr)
    
    printResult (file, (exitCode, stdout, stderr, comparisonResult,comparisonResultStderr)) = do
        when (comparisonResult) $ do -- Print if matching
            putStrLn $ "File: " ++ file
            putStrLn $ "Exit Code: " ++ show exitCode
            putStrLn $ "Matches Reference: " ++ show comparisonResult

        
        when (not comparisonResult && comparisonResultStderr) $ do -- Print if match in stderr
                putStrLn $ "File: " ++ file
                putStrLn $ "Exit Code: " ++ show exitCode
                putStrLn $ "Matches Reference: " ++ show comparisonResultStderr
                putStrLn "Match in Stderr:"
        when (not comparisonResult && not comparisonResultStderr) $ do -- Print the output if not matching
                putStrLn "Full Stdout:"
                putStrLn stdout
                putStrLn "Full Stderr:"
                putStrLn stderr
        putStrLn "---"

main :: IO ()
main = processFiles