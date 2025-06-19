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
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold
import Control.Monad.IO.Class (MonadIO)
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
    
    -- Use the new streaming-based getFileSizes function
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

-- | Alternative version using the streaming interface directly
generateHistogramStreaming :: FilePath -> FilePath -> IO ()
generateHistogramStreaming inputPath outputPath = do
    logInfo $ "Scanning files in (streaming): " ++ inputPath
    
    let scanOpts = defaultScanOptions 
            { followSymlinks = False
            , crossMountBoundaries = False
            }
    
    -- Use the streaming interface to collect file sizes
    sizes <- S.fold Fold.toList $ getFileSizesStream scanOpts inputPath
    
    if null sizes
        then do
            logWarn "No files found or directory doesn't exist"
        else do
            logInfo $ "Found " ++ show (length sizes) ++ " files (via streaming)"
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

-- | Version that processes the stream incrementally (most memory efficient)
generateHistogramIncremental :: FilePath -> FilePath -> IO ()
generateHistogramIncremental inputPath outputPath = do
    logInfo $ "Scanning files incrementally in: " ++ inputPath
    
    let scanOpts = defaultScanOptions 
            { followSymlinks = False
            , crossMountBoundaries = False
            }
    
    -- Process the stream incrementally to get statistics
    (count, minSize, maxSize, allSizes) <- S.fold 
        (collectSizeStats <$> Fold.length <*> Fold.minimum <*> Fold.maximum <*> Fold.toList)
        (getFileSizesStream scanOpts inputPath)
    
    if count == 0
        then do
            logWarn "No files found or directory doesn't exist"
        else do
            logInfo $ "Found " ++ show count ++ " files (incremental processing)"
            case (minSize, maxSize) of
                (Just minS, Just maxS) -> 
                    putStrLn $ "Size range: " ++ formatFileSize minS ++ " - " ++ formatFileSize maxS
                _ -> 
                    putStrLn "Could not determine size range"
            
            let histogram = createHistogram allSizes
            
            -- Save to HTML file
            logInfo $ "Saving histogram to: " ++ outputPath
            writeFile outputPath $ TL.unpack $ VL.toHtml histogram
            putStrLn $ "Histogram saved to: " ++ outputPath
            
            -- Open the file with appropriate command for the platform
            let openCommand = getOpenCommand
            logInfo $ "Opening " ++ outputPath ++ " with " ++ openCommand
            _ <- spawnProcess openCommand [outputPath]
            return ()
  where
    collectSizeStats :: Int -> Maybe Integer -> Maybe Integer -> [Integer] -> (Int, Maybe Integer, Maybe Integer, [Integer])
    collectSizeStats c minS maxS sizes = (c, minS, maxS, sizes)

-- | Get the appropriate command to open files based on the operating system
getOpenCommand :: String
getOpenCommand
    | "mingw" `elem` words os = "start"      -- Windows
    | "darwin" `elem` words os = "open"      -- macOS  
    | otherwise = "xdg-open"                 -- Linux/Unix

-- | Parse command line arguments
parseArgs :: [String] -> IO (FilePath, FilePath, Bool)
parseArgs [] = do
    logError "No arguments provided"
    putStrLn "Usage: file-histogram <directory> [-o <output-file>] [--streaming]"
    putStrLn "  <directory>         Directory to analyze"
    putStrLn "  -o <output-file>    Output HTML file (default: file_size_histogram.html)"
    putStrLn "  --streaming         Use streaming processing (more memory efficient)"
    putStrLn "  --incremental       Use incremental processing (most memory efficient)"
    exitFailure
parseArgs [dir] = do
    logDebug $ "Using default output file for directory: " ++ dir
    return (dir, "file_size_histogram.html", False)
parseArgs [dir, "--streaming"] = do
    logDebug $ "Using streaming mode for directory: " ++ dir
    return (dir, "file_size_histogram.html", True)
parseArgs [dir, "--incremental"] = do
    logDebug $ "Using incremental mode for directory: " ++ dir
    return (dir, "file_size_histogram.html", True)
parseArgs [dir, "-o", output] = do
    logDebug $ "Directory: " ++ dir ++ ", Output: " ++ output
    return (dir, output, False)
parseArgs [dir, "-o", output, "--streaming"] = do
    logDebug $ "Directory: " ++ dir ++ ", Output: " ++ output ++ " (streaming)"
    return (dir, output, True)
parseArgs [dir, "-o", output, "--incremental"] = do
    logDebug $ "Directory: " ++ dir ++ ", Output: " ++ output ++ " (incremental)"
    return (dir, output, True)
parseArgs ("-o":output:dir:_) = do
    logDebug $ "Directory: " ++ dir ++ ", Output: " ++ output
    return (dir, output, False)
parseArgs args = do
    logError $ "Invalid arguments: " ++ show args
    putStrLn "Invalid arguments. Usage: file-histogram <directory> [-o <output-file>] [--streaming|--incremental]"
    exitFailure

-- | Command line interface
fileHistogramCli :: IO ()
fileHistogramCli = do
    args <- getArgs
    logDebug $ "Command line arguments: " ++ show args
    (inputPath, outputPath, useStreaming) <- parseArgs args
    logInfo $ "Input directory: " ++ inputPath
    logInfo $ "Output file: " ++ outputPath
    logInfo $ "Streaming mode: " ++ show useStreaming
    
    if useStreaming
        then do
            logInfo "Using streaming/incremental processing"
            if "--incremental" `elem` args
                then generateHistogramIncremental inputPath outputPath
                else generateHistogramStreaming inputPath outputPath
        else do
            logInfo "Using traditional processing"
            generateHistogram inputPath outputPath

-- | Main entry point
main :: IO ()
main = fileHistogramCli
