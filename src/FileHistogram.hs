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
import qualified Data.Text as T
import qualified Graphics.Vega.VegaLite as VL
import Graphics.Vega.VegaLite (VegaLite)
import System.Info (os)
import System.Environment
import System.Process
import System.Exit
import System.IO (openFile, hClose, IOMode(WriteMode), stderr, hFlush, stdout)
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
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

-- | Main function to generate and save histogram (DEFAULT: incremental streaming with progress)
generateHistogram :: FilePath -> FilePath -> IO ()
generateHistogram inputPath outputPath = generateHistogramIncremental inputPath outputPath

-- | FIXED: Single-pass incremental streaming with progressive counting
generateHistogramIncremental :: FilePath -> FilePath -> IO ()
generateHistogramIncremental inputPath outputPath = do
    logInfo $ "Scanning files in: " ++ inputPath
    
    let scanOpts = defaultScanOptions 
            { followSymlinks = False
            , crossMountBoundaries = False
            }
    
    -- Auto-detect if we should show progress (interactive terminal)
    progressConfig <- progressConfigWithOverride True  -- Will auto-detect terminal
    
    -- SINGLE PASS: collect file sizes with progressive counting
    when (enableProgress progressConfig) $ do
        putStr "Scanning files... "
        hFlush stdout  -- FORCE IMMEDIATE OUTPUT
    
    -- Use a single stream pass with progressive counting (no pre-counting!)
    countRef <- newIORef 0
    allSizes <- S.fold (Fold.foldlM' (\acc size -> do
        count <- readIORef countRef
        let newCount = count + 1
        writeIORef countRef newCount
        -- Show progress every 1000 files
        when (enableProgress progressConfig && newCount `mod` 1000 == 0) $ do
            putStr $ "\rScanning files... " ++ show newCount ++ " found"
            hFlush stdout
        return (size : acc)) (return [])) (getFileSizesStream scanOpts inputPath)
    
    let count = length allSizes
    
    -- Clear progress line and show final count
    when (enableProgress progressConfig) $ do
        putStr $ "\rScanning files... " ++ show count ++ " found\n"
        hFlush stdout
    
    if count == 0
        then do
            logWarn "No files found or directory doesn't exist"
        else do
            logInfo $ "Found " ++ show count ++ " files"
            if not (null allSizes)
                then do
                    let minSize = minimum allSizes
                        maxSize = maximum allSizes
                    putStrLn $ "Size range: " ++ formatFileSize minSize ++ " - " ++ formatFileSize maxSize
                else putStrLn "No valid file sizes found"
            
            when (enableProgress progressConfig) $ putStrLn "Generating histogram..."
            logDebug $ "Creating histogram from " ++ show count ++ " file sizes"
            logDebug $ "Sample sizes: " ++ show (take 10 $ reverse allSizes)
            let histogram = createHistogram (reverse allSizes)  -- Reverse to get original order
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

-- | Traditional version (kept for compatibility)
generateHistogramTraditional :: FilePath -> FilePath -> IO ()
generateHistogramTraditional inputPath outputPath = do
    logInfo $ "Scanning files in: " ++ inputPath
    
    let scanOpts = defaultScanOptions 
            { followSymlinks = False
            , crossMountBoundaries = False
            }
    
    putStrLn "Scanning files..."
    hFlush stdout
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
    
    when (enableProgress progressConfig) $ do
        putStr "Streaming files... "
        hFlush stdout
    
    -- Use streaming interface with progress reporting
    countRef <- newIORef 0
    sizes <- S.fold (Fold.foldlM' (\acc size -> do
        count <- readIORef countRef
        let newCount = count + 1
        writeIORef countRef newCount
        -- Show progress every 1000 files
        when (enableProgress progressConfig && newCount `mod` 1000 == 0) $ do
            putStr $ "\rStreaming files... " ++ show newCount ++ " found"
            hFlush stdout
        return (size : acc)) (return [])) (getFileSizesStream scanOpts inputPath)
    
    let count = length sizes
    
    -- Clear progress line and show final count
    when (enableProgress progressConfig) $ do
        putStr $ "\rStreaming files... " ++ show count ++ " found\n"
        hFlush stdout
    
    if null sizes
        then do
            logWarn "No files found or directory doesn't exist"
        else do
            logInfo $ "Found " ++ show (length sizes) ++ " files (via streaming)"
            putStrLn $ "Size range: " ++ formatFileSize (minimum sizes) ++ " - " ++ formatFileSize (maximum sizes)
            
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
parseArgs :: [String] -> IO (FilePath, FilePath, ProcessingMode, Bool, LogLevel, Maybe FilePath)
parseArgs [] = do
    putStrLn "Usage: file-histogram <directory> [OPTIONS]"
    putStrLn "  <directory>                Directory to analyze"
    putStrLn ""
    putStrLn "Options:"
    putStrLn "  -o, --output <file>        Output HTML file (default: file_size_histogram.html)"
    putStrLn "  --traditional              Use traditional processing (collect all then process)"
    putStrLn "  --streaming                Use streaming processing"
    putStrLn "  --incremental              Use incremental processing (default)"
    putStrLn "  --no-progress              Disable progress indicators"
    putStrLn "  --log-level <level>        Set log level: DEBUG, INFO, WARN, ERROR (default: INFO)"
    putStrLn "  --log-file <file>          Write logs to file (default: stderr)"
    putStrLn "  --help                     Show this help"
    exitFailure
parseArgs args = parseArgsRec args "" "file_size_histogram.html" IncrementalMode True INFO Nothing
  where
    parseArgsRec [] "" _ _ _ _ _ = do
        putStrLn "Error: No directory specified"
        exitFailure
    parseArgsRec [] dir output mode progress logLevel logFile = 
        return (dir, output, mode, progress, logLevel, logFile)
    parseArgsRec ("--traditional":rest) dir output _ progress logLevel logFile = 
        parseArgsRec rest dir output TraditionalMode progress logLevel logFile
    parseArgsRec ("--streaming":rest) dir output _ progress logLevel logFile = 
        parseArgsRec rest dir output StreamingMode progress logLevel logFile
    parseArgsRec ("--incremental":rest) dir output _ progress logLevel logFile = 
        parseArgsRec rest dir output IncrementalMode progress logLevel logFile
    parseArgsRec ("--no-progress":rest) dir output mode _ logLevel logFile = 
        parseArgsRec rest dir output mode False logLevel logFile
    parseArgsRec ("--log-level":level:rest) dir output mode progress _ logFile = 
        case parseLogLevel level of
            Just lvl -> parseArgsRec rest dir output mode progress lvl logFile
            Nothing -> do
                putStrLn $ "Invalid log level: " ++ level ++ ". Use DEBUG, INFO, WARN, or ERROR"
                exitFailure
    parseArgsRec ("--log-file":file:rest) dir output mode progress logLevel _ = 
        parseArgsRec rest dir output mode progress logLevel (Just file)
    parseArgsRec ("-o":output:rest) dir _ mode progress logLevel logFile = 
        parseArgsRec rest dir output mode progress logLevel logFile
    parseArgsRec ("--output":output:rest) dir _ mode progress logLevel logFile = 
        parseArgsRec rest dir output mode progress logLevel logFile
    parseArgsRec ("--help":_) _ _ _ _ _ _ = do
        putStrLn "file-histogram - Generate file size histograms"
        putStrLn ""
        putStrLn "Usage: file-histogram <directory> [OPTIONS]"
        putStrLn ""
        putStrLn "Arguments:"
        putStrLn "  <directory>                Directory to analyze"
        putStrLn ""
        putStrLn "Options:"
        putStrLn "  -o, --output <file>        Output HTML file (default: file_size_histogram.html)"
        putStrLn "  --traditional              Use traditional processing (collect all then process)"
        putStrLn "  --streaming                Use streaming processing"
        putStrLn "  --incremental              Use incremental processing (default, fastest)"
        putStrLn "  --no-progress              Disable progress indicators"
        putStrLn "  --log-level <level>        Set log level: DEBUG, INFO, WARN, ERROR (default: INFO)"
        putStrLn "  --log-file <file>          Write logs to file (default: stderr)"
        putStrLn "  --help                     Show this help"
        putStrLn ""
        putStrLn "Examples:"
        putStrLn "  file-histogram /usr/lib64"
        putStrLn "  file-histogram /home/user --log-level DEBUG --log-file scan.log"
        putStrLn "  file-histogram /data --traditional --no-progress"
        exitSuccess
    parseArgsRec (dir:rest) "" output mode progress logLevel logFile = 
        parseArgsRec rest dir output mode progress logLevel logFile
    parseArgsRec (unknown:_) _ _ _ _ _ _ = do
        putStrLn $ "Unknown argument: " ++ unknown
        putStrLn "Use --help for usage information"
        exitFailure

-- | Parse log level from string
parseLogLevel :: String -> Maybe LogLevel
parseLogLevel "DEBUG" = Just DEBUG
parseLogLevel "INFO" = Just INFO
parseLogLevel "WARN" = Just WARN
parseLogLevel "ERROR" = Just ERROR
parseLogLevel _ = Nothing

-- | Processing modes
data ProcessingMode = TraditionalMode | StreamingMode | IncrementalMode
    deriving (Show, Eq)

-- | Command line interface with PROPER LOGGING SETUP
fileHistogramCli :: IO ()
fileHistogramCli = do
    args <- getArgs
    (inputPath, outputPath, mode, enableProgressIndicators, logLevel, logFile) <- parseArgs args
    
    -- SETUP LOGGING FIRST - before any log calls
    logHandle <- case logFile of
        Just file -> openFile file WriteMode
        Nothing -> return stderr
    
    let logConfig = defaultLogConfig 
            { minLogLevel = logLevel
            , logHandle = logHandle
            , enableConsole = False  -- Disable double console output
            }
    
    initLogging logConfig
    
    -- NOW we can use logging properly
    logInfo "=== file-histogram starting ==="
    logDebug $ "Command line arguments: " ++ show args
    logInfo $ "Input directory: " ++ inputPath
    logInfo $ "Output file: " ++ outputPath
    logInfo $ "Processing mode: " ++ show mode
    logInfo $ "Progress indicators: " ++ show enableProgressIndicators
    logInfo $ "Log level: " ++ show logLevel
    case logFile of
        Just file -> logInfo $ "Log file: " ++ file
        Nothing -> logInfo "Logging to stderr"
    
    -- Set global progress configuration
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
    
    logInfo "=== file-histogram completed successfully ==="
    
    -- Close log file if we opened one
    case logFile of
        Just _ -> hClose logHandle
        Nothing -> return ()

-- | Main entry point
main :: IO ()
main = fileHistogramCli
