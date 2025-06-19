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
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
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

-- | Main function to generate and save histogram (DEFAULT: incremental streaming with progress)
generateHistogram :: FilePath -> FilePath -> IO ()
generateHistogram inputPath outputPath = generateHistogramIncremental inputPath outputPath

-- | NEW DEFAULT: Incremental streaming version with progress indicators
generateHistogramIncremental :: FilePath -> FilePath -> IO ()
generateHistogramIncremental inputPath outputPath = do
    logInfo $ "Scanning files in: " ++ inputPath
    
    let scanOpts = defaultScanOptions 
            { followSymlinks = False
            , crossMountBoundaries = False
            }
    
    -- Auto-detect if we should show progress (interactive terminal)
    progressConfig <- progressConfigWithOverride True  -- Will auto-detect terminal
    
    -- Use incremental streaming with progress indicators
    withProgress (progressConfig { progressPrefix = "Scanning files" }) Nothing $ \progressMVar -> do
        (count, minSize, maxSize, allSizes) <- S.fold 
            (collectHistogramStats <$> 
                Fold.foldlM' (\acc _ -> updateProgress progressMVar acc >> return (acc + 1)) (return 0) <*>
                Fold.minimum <*> 
                Fold.maximum <*> 
                Fold.toList)
            (getFileSizesStream scanOpts inputPath)
        
        liftIO $ do
            if count == 0
                then do
                    logWarn "No files found or directory doesn't exist"
                else do
                    logInfo $ "Found " ++ show count ++ " files"
                    case (minSize, maxSize) of
                        (Just minS, Just maxS) -> 
                            putStrLn $ "Size range: " ++ formatFileSize minS ++ " - " ++ formatFileSize maxS
                        _ -> 
                            putStrLn "Could not determine size range"
                    
                    when (enableProgress progressConfig) $ putStrLn "Generating histogram..."
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
    collectHistogramStats :: Int -> Maybe Integer -> Maybe Integer -> [Integer] -> (Int, Maybe Integer, Maybe Integer, [Integer])
    collectHistogramStats c minS maxS sizes = (c, minS, maxS, sizes)

-- | Traditional version (kept for compatibility)
generateHistogramTraditional :: FilePath -> FilePath -> IO ()
generateHistogramTraditional inputPath outputPath = do
    logInfo $ "Scanning files in: " ++ inputPath
    
    let scanOpts = defaultScanOptions 
            { followSymlinks = False
            , crossMountBoundaries = False
            }
    
    putStrLn "Scanning files..."
    sizes <- getFileSizes scanOpts inputPath
    
    if null sizes
        then do
            logWarn "No files found or directory doesn't exist"
        else do
            logInfo $ "Found " ++ show (length sizes) ++ " files"
            putStrLn $ "Size range: " ++ formatFileSize (minimum sizes) ++ " - " ++ formatFileSize (maximum sizes)
            
            putStrLn "Generating histogram..."
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

-- | Streaming version with progress indicators
generateHistogramStreaming :: FilePath -> FilePath -> IO ()
generateHistogramStreaming inputPath outputPath = do
    logInfo $ "Scanning files in (streaming): " ++ inputPath
    
    let scanOpts = defaultScanOptions 
            { followSymlinks = False
            , crossMountBoundaries = False
            }
    
    -- Auto-detect terminal for progress
    progressConfig <- progressConfigWithOverride True
    
    -- Use streaming interface with progress reporting
    sizes <- withProgress (progressConfig { progressPrefix = "Streaming files" }) Nothing $ \progressMVar -> do
        countRef <- liftIO $ newIORef 0
        S.fold (Fold.foldlM' (\acc size -> do
            c <- liftIO $ readIORef countRef
            updateProgress progressMVar c
            liftIO $ writeIORef countRef (c + 1)
            return (size : acc)) (return [])) (getFileSizesStream scanOpts inputPath)
    
    if null sizes
        then do
            logWarn "No files found or directory doesn't exist"
        else do
            logInfo $ "Found " ++ show (length sizes) ++ " files (via streaming)"
            putStrLn $ "Size range: " ++ formatFileSize (minimum sizes) ++ " - " ++ formatFileSize (maximum sizes)
            
            progressConfig <- progressConfigWithOverride True
            when (enableProgress progressConfig) $ putStrLn "Generating histogram..."
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
parseArgs :: [String] -> IO (FilePath, FilePath, ProcessingMode, Bool)
parseArgs [] = do
    logError "No arguments provided"
    putStrLn "Usage: file-histogram <directory> [-o <output-file>] [--traditional|--streaming] [--no-progress]"
    putStrLn "  <directory>         Directory to analyze"
    putStrLn "  -o <output-file>    Output HTML file (default: file_size_histogram.html)"
    putStrLn "  --traditional       Use traditional processing (collect all then process)"
    putStrLn "  --streaming         Use streaming processing"
    putStrLn "  --no-progress       Disable progress indicators"
    putStrLn ""
    putStrLn "Default: Incremental streaming with progress indicators (fastest and most memory efficient)"
    exitFailure
parseArgs args = parseArgsRec args "" "file_size_histogram.html" IncrementalMode True
  where
    parseArgsRec [] "" _ _ _ = do
        logError "No directory specified"
        exitFailure
    parseArgsRec [] dir output mode progress = return (dir, output, mode, progress)
    parseArgsRec ("--traditional":rest) dir output _ progress = 
        parseArgsRec rest dir output TraditionalMode progress
    parseArgsRec ("--streaming":rest) dir output _ progress = 
        parseArgsRec rest dir output StreamingMode progress
    parseArgsRec ("--incremental":rest) dir output _ progress = 
        parseArgsRec rest dir output IncrementalMode progress
    parseArgsRec ("--no-progress":rest) dir output mode _ = 
        parseArgsRec rest dir output mode False
    parseArgsRec ("-o":output:rest) dir _ mode progress = 
        parseArgsRec rest dir output mode progress
    parseArgsRec (dir:rest) "" output mode progress = 
        parseArgsRec rest dir output mode progress
    parseArgsRec (unknown:_) _ _ _ _ = do
        logError $ "Unknown argument: " ++ unknown
        exitFailure

-- | Processing modes
data ProcessingMode = TraditionalMode | StreamingMode | IncrementalMode
    deriving (Show, Eq)

-- | Command line interface
fileHistogramCli :: IO ()
fileHistogramCli = do
    args <- getArgs
    logDebug $ "Command line arguments: " ++ show args
    (inputPath, outputPath, mode, enableProgressIndicators) <- parseArgs args
    
    logInfo $ "Input directory: " ++ inputPath
    logInfo $ "Output file: " ++ outputPath
    logInfo $ "Processing mode: " ++ show mode
    logInfo $ "Progress indicators: " ++ show enableProgressIndicators
    
    -- Set global progress configuration based on user override and terminal detection
    progressConfig <- progressConfigWithOverride enableProgressIndicators
    
    case mode of
        TraditionalMode -> do
            logInfo "Using traditional processing"
            generateHistogramTraditional inputPath outputPath
        StreamingMode -> do
            logInfo "Using streaming processing"
            generateHistogramStreaming inputPath outputPath
        IncrementalMode -> do
            logInfo "Using incremental streaming processing (default)"
            generateHistogramIncremental inputPath outputPath

-- | Main entry point
main :: IO ()
main = fileHistogramCli
