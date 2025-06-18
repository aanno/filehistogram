{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module FileHistogram 
    ( createHistogram
    , formatFileSize
    , generateHistogram
    , fileHistogramCli
    , main
    ) where

import qualified Data.Text.Lazy as TL
import qualified Graphics.Vega.VegaLite as VL
import Graphics.Vega.VegaLite (VegaLite)
import System.Info (os)
import System.Environment
import System.Process
import System.Exit
import FileScanner
import Logging

-- | Create histogram with logarithmic transformation and proper file size formatting
createHistogram :: [Integer] -> VegaLite
createHistogram allSizes =
    -- Filter out zero-byte files and transform to log
    let sizes = filter (> 0) allSizes
        logSizes = map (\x -> logBase 10 (fromInteger x)) sizes
        fileSizeData = VL.dataFromColumns []
            . VL.dataColumn "log_size" (VL.Numbers logSizes)
            . VL.dataColumn "size" (VL.Numbers $ map fromInteger sizes)
            $ []

        enc = VL.encoding
            . VL.position VL.X [ VL.PName "log_size"
                        , VL.PmType VL.Quantitative
                        , VL.PTitle "File Size (log scale)"
                        , VL.PBin [VL.MaxBins 20]
                        -- Format the axis labels to show actual file sizes
                        , VL.PAxis [ VL.AxLabelExpr "pow(10, datum.value) < 1024 ? pow(10, datum.value) + ' B' : pow(10, datum.value) < 1048576 ? floor(pow(10, datum.value)/1024) + ' KB' : pow(10, datum.value) < 1073741824 ? floor(pow(10, datum.value)/1048576) + ' MB' : floor(pow(10, datum.value)/1073741824) + ' GB'" ]
                        ]
            . VL.position VL.Y [ VL.PAggregate VL.Count
                        , VL.PmType VL.Quantitative
                        , VL.PScale [VL.SType VL.ScLog]  -- Logarithmic scale on Y
                        , VL.PTitle "Number of Files (log scale)"
                        ]
            . VL.color [VL.MString "#4682B4"]

    in VL.toVegaLite
        [ VL.title "File Size Distribution (Log Scale)" []
        , VL.width 600
        , VL.height 400
        , fileSizeData
        , VL.mark VL.Bar []
        , enc []
        ]

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
    logInfo $ "Scanning files in: " ++ inputPath
    
    let scanOpts = defaultScanOptions 
            { followSymlinks = False
            , crossMountBoundaries = False
            }
    
    sizes <- getFileSizes scanOpts inputPath
    
    if null sizes
        then do
            logWarn "No files found or directory doesn't exist"
        else do
            logInfo $ "Found " ++ show (length sizes) ++ " files"
            putStrLn $ "Size range: " ++ formatFileSize (minimum sizes) ++ " - " ++ formatFileSize (maximum sizes)
            
            let histogram = createHistogram sizes
            
            -- Save to HTML file
            logInfo $ "Saving histogram to: " ++ outputPath
            writeFile outputPath $ TL.unpack $ VL.toHtml histogram
            putStrLn $ "Histogram saved to: " ++ outputPath
            
            -- Open the file with appropriate command for the platform
            let openCommand = getOpenCommand
            logInfo $ "Opening " ++ outputPath ++ " with " ++ openCommand
            _ <- spawnProcess openCommand [outputPath]
            return ()

-- | Get the appropriate command to open files based on the operating system
getOpenCommand :: String
getOpenCommand
    | "mingw" `elem` words os = "start"      -- Windows
    | "darwin" `elem` words os = "open"      -- macOS  
    | otherwise = "xdg-open"                 -- Linux/Unix

-- | Parse command line arguments
parseArgs :: [String] -> IO (FilePath, FilePath)
parseArgs [] = do
    logError "No arguments provided"
    putStrLn "Usage: file-histogram <directory> [-o <output-file>]"
    putStrLn "  <directory>         Directory to analyze"
    putStrLn "  -o <output-file>    Output HTML file (default: file_size_histogram.html)"
    exitFailure
parseArgs [dir] = do
    logDebug $ "Using default output file for directory: " ++ dir
    return (dir, "file_size_histogram.html")
parseArgs [dir, "-o", output] = do
    logDebug $ "Directory: " ++ dir ++ ", Output: " ++ output
    return (dir, output)
parseArgs ("-o":output:dir:_) = do
    logDebug $ "Directory: " ++ dir ++ ", Output: " ++ output
    return (dir, output)
parseArgs args = do
    logError $ "Invalid arguments: " ++ show args
    putStrLn "Invalid arguments. Usage: file-histogram <directory> [-o <output-file>]"
    exitFailure

-- | Command line interface
fileHistogramCli :: IO ()
fileHistogramCli = do
    args <- getArgs
    logDebug $ "Command line arguments: " ++ show args
    (inputPath, outputPath) <- parseArgs args
    logInfo $ "Input directory: " ++ inputPath
    logInfo $ "Output file: " ++ outputPath
    generateHistogram inputPath outputPath

-- | Main entry point
main :: IO ()
main = fileHistogramCli
