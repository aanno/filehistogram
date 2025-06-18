{-# LANGUAGE OverloadedStrings #-}

module FileHistogram 
    ( getFileSizes
    , createHistogram
    , formatFileSize
    , generateHistogram
    , fileHistogramCli
    , main
    ) where

import qualified Graphics.Vega.VegaLite as VL
import Graphics.Vega.VegaLite (VegaLite)
import System.Directory
import System.FilePath
import System.Environment
import System.Process
import System.Exit
import Control.Monad
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- | Create histogram with logarithmic scaling for both axes
createHistogramNotWorking :: [Integer] -> VegaLite
createHistogramNotWorking sizes =
    let         -- Filter out zero-byte files for log scale
        nonZeroSizes = filter (> 0) sizes
        fileSizeData = VL.dataFromJson (toJSON $ map (\fileSize -> object ["size" .= fileSize]) nonZeroSizes) []

        enc = VL.encoding
            . VL.position VL.X [ VL.PName "size"
                        , VL.PmType VL.Quantitative
                        , VL.PTitle "File Size (bytes, log scale)"
                        , VL.PScale [VL.SType VL.ScLog]  -- Logarithmic scale
                        , VL.PBin [VL.MaxBins 25]
                        ]
            . VL.position VL.Y [ VL.PAggregate VL.Count
                        , VL.PmType VL.Quantitative
                        , VL.PTitle "Number of Files"
                        ]
            . VL.color [VL.MString "#4682B4"]

    in VL.toVegaLite
        [ VL.title (T.pack $ "File Size Distribution (" ++ show (length nonZeroSizes) ++ " files with size > 0)") []
        , VL.width 700
        , VL.height 450
        , fileSizeData
        , VL.mark VL.Bar [VL.MTooltip VL.TTEncoding]
        , enc []
        ]

-- | Create histogram with human-readable file sizes
createHistogram :: [Integer] -> VegaLite
createHistogram sizes =
    let fileSizeData = VL.dataFromColumns []
            . VL.dataColumn "size" (VL.Numbers $ map fromInteger sizes)
            $ []

        enc = VL.encoding
            . VL.position VL.X [ VL.PName "size"
                        , VL.PmType VL.Quantitative
                        , VL.PTitle "File Size (bytes)"
                        -- , VL.PScale [VL.SType VL.ScLog]  -- Logarithmic scale
                        , VL.PBin [VL.MaxBins 20]
                        ]
            . VL.position VL.Y [ VL.PAggregate VL.Count
                        , VL.PmType VL.Quantitative
                        -- , VL.PScale [VL.SType VL.ScLog]  -- Logarithmic scale
                        , VL.PTitle "Number of Files"
                        ]
            . VL.color [VL.MString "#4682B4"]

    in VL.toVegaLite
        [ VL.title "File Size Distribution" []
        , VL.width 600
        , VL.height 400
        , fileSizeData
        , VL.mark VL.Bar []
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
            writeFile outputPath $ TL.unpack $ VL.toHtml histogram
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

-- | Main entry point
main :: IO ()
main = fileHistogramCli
