{-# LANGUAGE OverloadedStrings #-}

module FileHistogram 
    ( createHistogram
    , formatFileSize
    , generateHistogram
    , generateHistogramTraditional
    , generateHistogramStreaming
    , generateHistogramIncremental
    ) where

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Graphics.Vega.VegaLite as VL
import Graphics.Vega.VegaLite (VegaLite)
import System.Info (os)
import System.Process
import System.IO (hFlush, stdout)
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, modifyIORef')
import FileScanner
import Logging
import ProgressIndicator

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

    in if null sizes
       then -- Create empty histogram for zero files
            VL.toVegaLite
                [ VL.title (T.pack "File Size Distribution (No Files Found)") []
                , VL.width 600
                , VL.height 400
                , VL.dataFromColumns [] $ []
                , VL.mark VL.Bar []
                , enc []
                ]
       else -- Normal histogram
            VL.toVegaLite
                [ VL.title (T.pack $ "File Size Distribution (" ++ show (length sizes) ++ " files)") []
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

-- | Main function to generate and save histogram (defaults to incremental)
generateHistogram :: FilePath -> FilePath -> IO ()
generateHistogram inputPath outputPath = do
    let scanOpts = defaultScanOptions 
            { followSymlinks = False
            , crossMountBoundaries = False
            , concurrentWorkers = 16
            }
    progressConfig <- progressConfigWithOverride True
    generateHistogramIncremental scanOpts progressConfig inputPath outputPath

-- | OPTIMIZED: Single-pass incremental streaming with efficient fold
generateHistogramIncremental :: ScanOptions -> ProgressConfig -> FilePath -> FilePath -> IO ()
generateHistogramIncremental scanOpts progressConfig inputPath outputPath = do
    logInfo $ "Scanning files in: " ++ inputPath
    
    -- Show initial progress
    when (enableProgress progressConfig) $ do
        putStr "Scanning files... "
        hFlush stdout
    
    -- Efficient single-pass fold that collects sizes with progress
    let progressFold = if enableProgress progressConfig
            then Fold.foldlM' 
                (\(sizes, count) size -> do
                    let newCount = count + 1
                    -- Show progress every 1000 files
                    when (newCount `mod` 1000 == 0) $ do
                        putStr $ "\rScanning files... " ++ show newCount ++ " found"
                        hFlush stdout
                    return (size : sizes, newCount)
                ) 
                (return ([], 0 :: Int))
            else Fold.foldl' (\(sizes, count) size -> (size : sizes, count + 1)) ([], 0 :: Int)
    
    -- Process files using concurrent streaming
    (allSizes, count) <- S.fold progressFold (getFileSizesStream scanOpts inputPath)
    
    -- Clear progress line and show final count
    when (enableProgress progressConfig) $ do
        putStr $ "\rScanning files... " ++ show count ++ " found\n"
        hFlush stdout
    
    processAndSaveHistogram allSizes count outputPath

-- | Traditional version (kept for compatibility)
generateHistogramTraditional :: ScanOptions -> ProgressConfig -> FilePath -> FilePath -> IO ()
generateHistogramTraditional scanOpts _ inputPath outputPath = do
    logInfo $ "Scanning files in: " ++ inputPath
    
    putStrLn "Scanning files..."
    hFlush stdout
    sizes <- getFileSizes scanOpts inputPath
    
    processAndSaveHistogram sizes (length sizes) outputPath

-- | Streaming version with progress indicators
generateHistogramStreaming :: ScanOptions -> ProgressConfig -> FilePath -> FilePath -> IO ()
generateHistogramStreaming scanOpts progressConfig inputPath outputPath = do
    logInfo $ "Scanning files in (streaming): " ++ inputPath
    
    when (enableProgress progressConfig) $ do
        putStr "Streaming files... "
        hFlush stdout
    
    -- Use streaming interface with progress reporting
    countRef <- newIORef (0 :: Int)
    sizes <- S.fold (Fold.foldlM' (\acc size -> do
        modifyIORef' countRef (+1)
        count <- readIORef countRef
        -- Show progress every 1000 files
        when (enableProgress progressConfig && count `mod` 1000 == 0) $ do
            putStr $ "\rStreaming files... " ++ show count ++ " found"
            hFlush stdout
        return (size : acc)) (return [])) (getFileSizesStream scanOpts inputPath)
    
    count <- readIORef countRef
    
    -- Clear progress line and show final count
    when (enableProgress progressConfig) $ do
        putStr $ "\rStreaming files... " ++ show count ++ " found\n"
        hFlush stdout
    
    processAndSaveHistogram sizes count outputPath

-- | Common function to process sizes and save histogram
processAndSaveHistogram :: [Integer] -> Int -> FilePath -> IO ()
processAndSaveHistogram sizes count outputPath = do
    if count == 0
        then do
            logWarn "No files found or directory doesn't exist"
        else do
            logInfo $ "Found " ++ show count ++ " files"
            if not (null sizes)
                then do
                    let minSize = minimum sizes
                        maxSize = maximum sizes
                    putStrLn $ "Size range: " ++ formatFileSize minSize ++ " - " ++ formatFileSize maxSize
                else putStrLn "No valid file sizes found"
            
            putStrLn "Generating histogram..."
            logDebug $ "Creating histogram from " ++ show count ++ " file sizes"
            logDebug $ "Sample sizes: " ++ show (take 10 sizes)
            let histogram = createHistogram (reverse sizes)  -- Reverse to get original order
                htmlContent = VL.toHtml histogram
            
            logDebug $ "Generated HTML content length: " ++ show (TL.length htmlContent)
            
            -- Save to HTML file
            logInfo $ "Saving histogram to: " ++ outputPath
            writeFile outputPath $ TL.unpack htmlContent
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
