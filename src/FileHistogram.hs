{-# LANGUAGE OverloadedStrings #-}

module FileHistogram 
    ( getFileSizes
    , createHistogram
    , formatFileSize
    , generateHistogram
    , fileHistogramCli
    ) where

import Graphics.Vega.VegaLite
import System.Directory
import System.FilePath
import System.Environment
import System.Process
import System.Exit
import Control.Monad
-- import Data.Aeson
import qualified Data.Text.Lazy as TL

-- | Create histogram with logarithmic scaling for both axes
createHistogram :: [Integer] -> VegaLite
createHistogram sizes =
    let fileSizeData = dataFromColumns []
            . dataColumn "size" (Numbers $ map fromInteger sizes)
            $ []

        enc = encoding
            . position X [ PName "size"
                        , PmType Quantitative
                        , PTitle "File Size (bytes)"
                        , PScale [SType ScLog]  -- Logarithmic scale
                        , PBin [MaxBins 25]
                        ]
            . position Y [ PAggregate Count
                        , PmType Quantitative
                        , PTitle "Number of Files"
                        , PScale [SType ScLog]  -- Log scale for counts
                        ]
            . color [MString "#4682B4"]

    in toVegaLite
        [ title "File Size Distribution (Log Scale)" []
        , width 700
        , height 450
        , fileSizeData
        , mark Bar [MTooltip TTEncoding]
        , enc []
        ]

-- | Get all file sizes recursively from a directory
getFileSizes :: FilePath -> IO [Integer]
getFileSizes path = do
    exists <- doesDirectoryExist path
    if exists
        then do
            contents <- listDirectory path
            sizes <- forM contents $ \item -> do
                let fullPath = path </> item
                isFile <- doesFileExist fullPath
                isDir <- doesDirectoryExist fullPath
                if isFile
                    then do
                        fileSize <- getFileSize fullPath
                        return [fileSize]
                    else if isDir
                        then getFileSizes fullPath
                        else return []
            return $ concat sizes
        else do
            putStrLn $ "Directory does not exist: " ++ path
            return []

-- | Format file size in human-readable format
formatFileSize :: Integer -> String
formatFileSize bytes
    | bytes < 1024 = show bytes ++ " B"
    | bytes < (1024 :: Integer)^(2 :: Integer) = show (bytes `div` 1024) ++ " KB"
    | bytes < (1024 :: Integer)^(3 :: Integer) = show (bytes `div` ((1024 :: Integer)^(2 :: Integer))) ++ " MB"
    | otherwise = show (bytes `div` ((1024 :: Integer)^(3 :: Integer))) ++ " GB"

-- | Main function to generate and save histogram
generateHistogram :: FilePath -> FilePath -> IO ()
generateHistogram inputPath outputPath = do
    putStrLn $ "Scanning files in: " ++ inputPath
    sizes <- getFileSizes inputPath
    
    if null sizes
        then putStrLn "No files found or directory doesn't exist"
        else do
            putStrLn $ "Found " ++ show (length sizes) ++ " files"
            putStrLn $ "Size range: " ++ formatFileSize (minimum sizes) ++ " - " ++ formatFileSize (maximum sizes)
            
            let histogram = createHistogram sizes
            
            -- Save to HTML file
            writeFile outputPath $ TL.unpack $ toHtml histogram
            putStrLn $ "Histogram saved to: " ++ outputPath
            
            -- Open the file with xdg-open
            putStrLn $ "Opening " ++ outputPath ++ " with xdg-open..."
            _ <- spawnProcess "xdg-open" [outputPath]
            return ()

-- | Parse command line arguments
parseArgs :: [String] -> IO (FilePath, FilePath)
parseArgs [] = do
    putStrLn "Usage: file-histogram <directory> [-o <output-file>]"
    putStrLn "  <directory>         Directory to analyze"
    putStrLn "  -o <output-file>    Output HTML file (default: file_size_histogram.html)"
    exitFailure
parseArgs [dir] = return (dir, "file_size_histogram.html")
parseArgs [dir, "-o", output] = return (dir, output)
parseArgs ("-o":output:dir:_) = return (dir, output)
parseArgs _ = do
    putStrLn "Invalid arguments. Usage: file-histogram <directory> [-o <output-file>]"
    exitFailure

-- | Command line interface
fileHistogramCli :: IO ()
fileHistogramCli = do
    args <- getArgs
    (inputPath, outputPath) <- parseArgs args
    generateHistogram inputPath outputPath
