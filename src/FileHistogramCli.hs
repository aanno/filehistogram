{-# LANGUAGE OverloadedStrings #-}

module FileHistogramCli
    ( fileHistogramCli
    , ProcessingMode(..)
    , CliConfig(..)
    , parseArgs
    ) where

import System.Environment
import System.Exit
import System.IO (stderr, stdout, hClose)
import System.OsPath
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow)
import Control.Exception (try)
import Data.Maybe (isJust, fromJust)
import Data.Traversable (traverse)
import FileHistogram
import FileScanner (defaultScanOptions, ScanOptions(..))
import Logging (LogLevel(..), LogConfig(..), defaultLogConfig, initLogging, logDebug, logInfo, withLogging)
import ProgressIndicator

-- | Processing modes
data ProcessingMode = TraditionalMode | StreamingMode | IncrementalMode
    deriving (Show, Eq)

-- | CLI Configuration
data CliConfig = CliConfig
    { cliInputPath :: FilePath
    , cliOutputPath :: FilePath
    , cliProcessingMode :: ProcessingMode
    , cliEnableProgressIndicators :: Bool
    , cliLogLevel :: LogLevel
    , cliLogFile :: Maybe FilePath
    , cliConcurrentWorkers :: Int
    } deriving (Show)

-- | Default configuration
defaultCliConfig :: CliConfig
defaultCliConfig = CliConfig
    { cliInputPath = ""
    , cliOutputPath = "file_size_histogram.html"
    , cliProcessingMode = IncrementalMode
    , cliEnableProgressIndicators = True
    , cliLogLevel = INFO
    , cliLogFile = Nothing
    , cliConcurrentWorkers = 16
    }

-- | Parse command line arguments
parseArgs :: [String] -> IO CliConfig
parseArgs [] = showUsageAndExit
parseArgs args = parseArgsRec args defaultCliConfig
  where
    parseArgsRec :: [String] -> CliConfig -> IO CliConfig
    parseArgsRec [] config
        | null (cliInputPath config) = do
            putStrLn "Error: No directory specified"
            exitFailure
        | otherwise = return config
    
    parseArgsRec ("--help":_) _ = showUsageAndExit
    parseArgsRec ("-h":_) _ = showUsageAndExit
    
    parseArgsRec ("--traditional":rest) config = 
        parseArgsRec rest (config { cliProcessingMode = TraditionalMode })
    parseArgsRec ("--streaming":rest) config = 
        parseArgsRec rest (config { cliProcessingMode = StreamingMode })
    parseArgsRec ("--incremental":rest) config = 
        parseArgsRec rest (config { cliProcessingMode = IncrementalMode })
    
    parseArgsRec ("--no-progress":rest) config = 
        parseArgsRec rest (config { cliEnableProgressIndicators = False })
    
    parseArgsRec ("--log-level":level:rest) config = 
        case parseLogLevel level of
            Just lvl -> parseArgsRec rest (config { cliLogLevel = lvl })
            Nothing -> do
                putStrLn $ "Invalid log level: " ++ level ++ ". Use DEBUG, INFO, WARN, or ERROR"
                exitFailure
    parseArgsRec ("--log-level":_) _ = do
        putStrLn "Error: --log-level requires an argument"
        exitFailure
    
    parseArgsRec ("--log-file":file:rest) config = 
        parseArgsRec rest (config { cliLogFile = Just file })
    parseArgsRec ("--log-file":_) _ = do
        putStrLn "Error: --log-file requires an argument"
        exitFailure
    
    parseArgsRec ("-o":output:rest) config = 
        parseArgsRec rest (config { cliOutputPath = output })
    parseArgsRec ("-o":_) _ = do
        putStrLn "Error: -o requires an argument"
        exitFailure
    
    parseArgsRec ("--output":output:rest) config = 
        parseArgsRec rest (config { cliOutputPath = output })
    parseArgsRec ("--output":_) _ = do
        putStrLn "Error: --output requires an argument"
        exitFailure
    
    parseArgsRec ("--workers":n:rest) config = 
        case reads n of
            [(num, "")] | num > 0 -> parseArgsRec rest (config { cliConcurrentWorkers = num })
            _ -> do
                putStrLn $ "Invalid number of workers: " ++ n
                exitFailure
    parseArgsRec ("--workers":_) _ = do
        putStrLn "Error: --workers requires a number"
        exitFailure
    
    parseArgsRec (dir:rest) config
        | null (cliInputPath config) = parseArgsRec rest (config { cliInputPath = dir })
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
    -- traverse is `mapM` for `Maybe`, so it will only run if the value is `Just`
    maybeOsPath <- traverse encodeUtf $ cliLogFile config

    let logConfig = defaultLogConfig 
            { minLogLevel = cliLogLevel config
            , logFile = maybeOsPath
            , console = Just stdout
            }

    logHandle <- initLogging logConfig
    
    -- Log configuration
    logInfo "=== file-histogram starting ==="
    logDebug $ "Command line arguments: " ++ show args
    logInfo $ "Input directory: " ++ cliInputPath config
    logInfo $ "Output file: " ++ cliOutputPath config
    logInfo $ "Processing mode: " ++ show (cliProcessingMode config)
    logInfo $ "Progress indicators: " ++ show (cliEnableProgressIndicators config)
    logInfo $ "Log level: " ++ show (cliLogLevel config)
    logInfo $ "Concurrent workers: " ++ show (cliConcurrentWorkers config)
    case cliLogFile config of
        Just file -> logInfo $ "Log file: " ++ file
        Nothing -> logInfo "Logging to stderr"
    
    -- Set scan options with concurrent workers
    let scanOpts = defaultScanOptions 
            { followSymlinks = False
            , crossMountBoundaries = False
            , concurrentWorkers = cliConcurrentWorkers config
            }
    
    -- Set global progress configuration
    progressConfig <- progressConfigWithOverride (cliEnableProgressIndicators config)
    
    -- Process based on mode
    case cliProcessingMode config of
        TraditionalMode -> do
            logInfo "Using traditional processing"
            generateHistogramTraditional scanOpts progressConfig (cliInputPath config) (cliOutputPath config)
        StreamingMode -> do
            logInfo "Using streaming processing"
            generateHistogramStreaming scanOpts progressConfig (cliInputPath config) (cliOutputPath config)
        IncrementalMode -> do
            logInfo "Using incremental streaming processing (default)"
            generateHistogramIncremental scanOpts progressConfig (cliInputPath config) (cliOutputPath config)
    
    logInfo "=== file-histogram completed successfully ==="

    -- Close log file if we opened one
    -- Data.Foldable.for_ logHandle hClose
    when (isJust logHandle) $ hClose $ fromJust logHandle
