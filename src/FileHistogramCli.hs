{-# LANGUAGE OverloadedStrings #-}

module FileHistogramCli
    ( fileHistogramCli
    , ProcessingMode(..)
    , CliConfig(..)
    , parseArgs
    ) where

import System.Environment
import System.Exit
import System.IO (openFile, hClose, IOMode(WriteMode), stderr)
import Control.Monad (when)
import Data.Maybe (isJust)
import FileHistogram
import FileScanner (defaultScanOptions, ScanOptions(..))
import Logging
import ProgressIndicator

-- | Processing modes
data ProcessingMode = TraditionalMode | StreamingMode | IncrementalMode
    deriving (Show, Eq)

-- | CLI Configuration
data CliConfig = CliConfig
    { inputPath :: FilePath
    , outputPath :: FilePath
    , processingMode :: ProcessingMode
    , enableProgressIndicators :: Bool
    , logLevel :: LogLevel
    , logFile :: Maybe FilePath
    , concurrentWorkers :: Int
    } deriving (Show)

-- | Default configuration
defaultCliConfig :: CliConfig
defaultCliConfig = CliConfig
    { inputPath = ""
    , outputPath = "file_size_histogram.html"
    , processingMode = IncrementalMode
    , enableProgressIndicators = True
    , logLevel = INFO
    , logFile = Nothing
    , concurrentWorkers = 16
    }

-- | Parse command line arguments
parseArgs :: [String] -> IO CliConfig
parseArgs [] = showUsageAndExit
parseArgs args = parseArgsRec args defaultCliConfig
  where
    parseArgsRec :: [String] -> CliConfig -> IO CliConfig
    parseArgsRec [] config
        | null (inputPath config) = do
            putStrLn "Error: No directory specified"
            exitFailure
        | otherwise = return config
    
    parseArgsRec ("--help":_) _ = showUsageAndExit
    parseArgsRec ("-h":_) _ = showUsageAndExit
    
    parseArgsRec ("--traditional":rest) config = 
        parseArgsRec rest (config { processingMode = TraditionalMode })
    parseArgsRec ("--streaming":rest) config = 
        parseArgsRec rest (config { processingMode = StreamingMode })
    parseArgsRec ("--incremental":rest) config = 
        parseArgsRec rest (config { processingMode = IncrementalMode })
    
    parseArgsRec ("--no-progress":rest) config = 
        parseArgsRec rest (config { enableProgressIndicators = False })
    
    parseArgsRec ("--log-level":level:rest) config = 
        case parseLogLevel level of
            Just lvl -> parseArgsRec rest (config { logLevel = lvl })
            Nothing -> do
                putStrLn $ "Invalid log level: " ++ level ++ ". Use DEBUG, INFO, WARN, or ERROR"
                exitFailure
    parseArgsRec ("--log-level":_) _ = do
        putStrLn "Error: --log-level requires an argument"
        exitFailure
    
    parseArgsRec ("--log-file":file:rest) config = 
        parseArgsRec rest (config { logFile = Just file })
    parseArgsRec ("--log-file":_) _ = do
        putStrLn "Error: --log-file requires an argument"
        exitFailure
    
    parseArgsRec ("-o":output:rest) config = 
        parseArgsRec rest (config { outputPath = output })
    parseArgsRec ("-o":_) _ = do
        putStrLn "Error: -o requires an argument"
        exitFailure
    
    parseArgsRec ("--output":output:rest) config = 
        parseArgsRec rest (config { outputPath = output })
    parseArgsRec ("--output":_) _ = do
        putStrLn "Error: --output requires an argument"
        exitFailure
    
    parseArgsRec ("--workers":n:rest) config = 
        case reads n of
            [(num, "")] | num > 0 -> parseArgsRec rest (config { concurrentWorkers = num })
            _ -> do
                putStrLn $ "Invalid number of workers: " ++ n
                exitFailure
    parseArgsRec ("--workers":_) _ = do
        putStrLn "Error: --workers requires a number"
        exitFailure
    
    parseArgsRec (dir:rest) config
        | null (inputPath config) = parseArgsRec rest (config { inputPath = dir })
        | otherwise = do
            putStrLn $ "Unknown argument: " ++ dir
            putStrLn "Use --help for usage information"
            exitFailure

-- | Show usage and exit
showUsageAndExit :: IO a
showUsageAndExit = do
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
    putStrLn "  --workers <n>              Number of concurrent workers (default: 16)"
    putStrLn "  --log-level <level>        Set log level: DEBUG, INFO, WARN, ERROR (default: INFO)"
    putStrLn "  --log-file <file>          Write logs to file (default: stderr)"
    putStrLn "  -h, --help                 Show this help"
    putStrLn ""
    putStrLn "Examples:"
    putStrLn "  file-histogram /usr/lib64"
    putStrLn "  file-histogram /home/user --log-level DEBUG --log-file scan.log"
    putStrLn "  file-histogram /data --traditional --no-progress"
    putStrLn "  file-histogram /home/user/large-dir --workers 32"
    exitSuccess

-- | Parse log level from string
parseLogLevel :: String -> Maybe LogLevel
parseLogLevel "DEBUG" = Just DEBUG
parseLogLevel "INFO" = Just INFO
parseLogLevel "WARN" = Just WARN
parseLogLevel "ERROR" = Just ERROR
parseLogLevel _ = Nothing

-- | Command line interface entry point
fileHistogramCli :: IO ()
fileHistogramCli = do
    args <- getArgs
    config <- parseArgs args
    
    -- Setup logging
    logHandle <- case logFile config of
        Just file -> openFile file WriteMode
        Nothing -> return stderr
    
    let logConfig = defaultLogConfig 
            { minLogLevel = logLevel config
            , logHandle = logHandle
            , enableConsole = False  -- Disable double console output
            }
    
    initLogging logConfig
    
    -- Log configuration
    logInfo "=== file-histogram starting ==="
    logDebug $ "Command line arguments: " ++ show args
    logInfo $ "Input directory: " ++ inputPath config
    logInfo $ "Output file: " ++ outputPath config
    logInfo $ "Processing mode: " ++ show (processingMode config)
    logInfo $ "Progress indicators: " ++ show (enableProgressIndicators config)
    logInfo $ "Log level: " ++ show (logLevel config)
    logInfo $ "Concurrent workers: " ++ show (concurrentWorkers config)
    case logFile config of
        Just file -> logInfo $ "Log file: " ++ file
        Nothing -> logInfo "Logging to stderr"
    
    -- Set scan options with concurrent workers
    let scanOpts = defaultScanOptions 
            { followSymlinks = False
            , crossMountBoundaries = False
            , concurrentWorkers = concurrentWorkers config
            }
    
    -- Set global progress configuration
    progressConfig <- progressConfigWithOverride (enableProgressIndicators config)
    
    -- Process based on mode
    case processingMode config of
        TraditionalMode -> do
            logInfo "Using traditional processing"
            generateHistogramTraditional scanOpts progressConfig (inputPath config) (outputPath config)
        StreamingMode -> do
            logInfo "Using streaming processing"
            generateHistogramStreaming scanOpts progressConfig (inputPath config) (outputPath config)
        IncrementalMode -> do
            logInfo "Using incremental streaming processing (default)"
            generateHistogramIncremental scanOpts progressConfig (inputPath config) (outputPath config)
    
    logInfo "=== file-histogram completed successfully ==="
    
    -- Close log file if we opened one
    when (isJust $ logFile config) $ hClose logHandle
