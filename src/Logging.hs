module Logging
    ( LogLevel(..)
    , logMessage
    , logDebug
    , logInfo
    , logWarn
    , logError
    , setLogLevel
    , getLogLevel
    ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import System.IO (hPutStrLn, stderr)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

-- Simple logging without external dependencies
data LogLevel = DEBUG | INFO | WARN | ERROR deriving (Show, Eq, Ord)

-- Global log level (starts at INFO, so DEBUG is filtered out)
{-# NOINLINE globalLogLevel #-}
globalLogLevel :: IORef LogLevel
globalLogLevel = unsafePerformIO (newIORef INFO)

-- | Set the minimum log level
setLogLevel :: MonadIO m => LogLevel -> m ()
setLogLevel level = liftIO $ writeIORef globalLogLevel level

-- | Get the current log level
getLogLevel :: MonadIO m => m LogLevel
getLogLevel = liftIO $ readIORef globalLogLevel

logMessage :: MonadIO m => LogLevel -> String -> m ()
logMessage level msg = do
    currentLevel <- getLogLevel
    when (level >= currentLevel) $ liftIO $ do
        timestamp <- getCurrentTime
        let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
        hPutStrLn stderr $ "[" ++ show level ++ "] " ++ timeStr ++ " - " ++ msg
  where
    when True action = action
    when False _ = return ()

logDebug, logInfo, logWarn, logError :: MonadIO m => String -> m ()
logDebug = logMessage DEBUG
logInfo = logMessage INFO
logWarn = logMessage WARN
logError = logMessage ERROR
