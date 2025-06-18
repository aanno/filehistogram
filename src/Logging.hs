{-# LANGUAGE OverloadedStrings #-}

module Logging
    ( LogLevel(..)
    , LogEntry(..)
    , LogConfig(..)
    , defaultLogConfig
    , initLogging
    , logMessage
    , logDebug
    , logInfo
    , logWarn
    , logError
    , logToStream
    , withLogging
    , setLogLevel
    , getLogLevel
    ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (when, forM_)
import Control.Concurrent.STM
import Control.Concurrent.Async
import System.IO (hPutStrLn, stderr, Handle)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale, UTCTime)
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold
import Streamly.Data.Stream (Stream)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- | Log levels in order of severity
data LogLevel = DEBUG | INFO | WARN | ERROR deriving (Show, Eq, Ord)

-- | Structured log entry
data LogEntry = LogEntry
    { logLevel :: LogLevel
    , logTimestamp :: UTCTime
    , logMessageString :: String
    , logThreadId :: String
    } deriving (Show, Eq)

-- | Logging configuration
data LogConfig = LogConfig
    { minLogLevel :: LogLevel
    , logHandle :: Handle
    , enableAsync :: Bool
    , bufferSize :: Int
    } deriving (Eq)

-- | Default logging configuration
defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
    { minLogLevel = INFO
    , logHandle = stderr
    , enableAsync = True
    , bufferSize = 100
    }

-- Global logging state
{-# NOINLINE globalLogQueue #-}
globalLogQueue :: TBQueue LogEntry
globalLogQueue = unsafePerformIO $ newTBQueueIO 1000

{-# NOINLINE globalLogConfig #-}
globalLogConfig :: IORef LogConfig
globalLogConfig = unsafePerformIO $ newIORef defaultLogConfig

{-# NOINLINE globalLogWorker #-}
globalLogWorker :: IORef (Maybe (Async ()))
globalLogWorker = unsafePerformIO $ newIORef Nothing

-- | Set the minimum log level
setLogLevel :: MonadIO m => LogLevel -> m ()
setLogLevel level = liftIO $ do
    config <- readIORef globalLogConfig
    writeIORef globalLogConfig (config { minLogLevel = level })

-- | Get the current log level
getLogLevel :: MonadIO m => m LogLevel
getLogLevel = liftIO $ do
    config <- readIORef globalLogConfig
    return $ minLogLevel config

-- | Initialize the logging system
initLogging :: MonadIO m => LogConfig -> m ()
initLogging config = liftIO $ do
    writeIORef globalLogConfig config
    
    when (enableAsync config) $ do
        -- Stop existing worker if any
        maybeWorker <- readIORef globalLogWorker
        forM_ maybeWorker cancel
        
        -- Start new async log worker
        worker <- async $ runLogWorker config
        writeIORef globalLogWorker (Just worker)

-- | Run the log processing worker
runLogWorker :: LogConfig -> IO ()
runLogWorker config = do
    S.fold (logFold config) logEntryStream
  where
    -- Stream of log entries from the queue
    logEntryStream :: Stream IO LogEntry
    logEntryStream = S.repeatM $ atomically $ readTBQueue globalLogQueue
    
    -- Fold that processes log entries
    logFold :: LogConfig -> Fold.Fold IO LogEntry ()
    logFold cfg = Fold.drainMapM (writeLogEntry cfg)

-- | Write a single log entry to the configured handle
writeLogEntry :: LogConfig -> LogEntry -> IO ()
writeLogEntry config entry = do
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (logTimestamp entry)
        formatted = "[" ++ show (logLevel entry) ++ "] " ++ timeStr ++ " - " ++ logMessageString entry
    hPutStrLn (logHandle config) formatted

-- | Main logging function
logMessage :: MonadIO m => LogLevel -> String -> m ()
logMessage level msg = liftIO $ do
    config <- readIORef globalLogConfig
    when (level >= minLogLevel config) $ do
        timestamp <- getCurrentTime
        let entry = LogEntry level timestamp msg "main"
        
        if enableAsync config
            then do
                -- Try to write to queue, drop if full (non-blocking)
                result <- atomically $ do
                    full <- isFullTBQueue globalLogQueue
                    if full
                        then return False
                        else do
                            writeTBQueue globalLogQueue entry
                            return True
                when (not result) $
                    hPutStrLn stderr "Warning: Log queue full, dropping message"
            else writeLogEntry config entry

-- | Convenience logging functions
logDebug, logInfo, logWarn, logError :: MonadIO m => String -> m ()
logDebug = logMessage DEBUG
logInfo = logMessage INFO
logWarn = logMessage WARN
logError = logMessage ERROR

-- | Create a stream of log entries (for advanced usage)
logToStream :: MonadIO m => LogLevel -> Stream m String -> Stream m LogEntry
logToStream level = S.mapM $ \msg -> liftIO $ do
    timestamp <- getCurrentTime
    return $ LogEntry level timestamp msg "stream"

-- | Run an action with logging, cleaning up afterwards
withLogging :: MonadIO m => LogConfig -> m a -> m a
withLogging config action = do
    initLogging config
    result <- action
    liftIO $ do
        -- Stop the log worker
        maybeWorker <- readIORef globalLogWorker
        forM_ maybeWorker cancel
        writeIORef globalLogWorker Nothing
    return result
