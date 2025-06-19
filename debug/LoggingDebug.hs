{-# LANGUAGE OverloadedStrings #-}

module LoggingDebug where

import System.IO (withFile, IOMode(..), stderr, hPutStrLn)
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace (trace, traceIO)
import Data.IORef (readIORef)
-- Helper imports
import Control.Concurrent (threadDelay)
import Data.Time (getCurrentTime)

import Logging

-- | Debug version of logMessage that shows execution path
debugLogMessage :: LogLevel -> String -> IO ()
debugLogMessage level msg = do
    traceIO $ "=== debugLogMessage called with level: " ++ show level
    
    config <- readIORef globalLogConfig
    traceIO $ "=== Current config: enableConsole=" ++ show (enableConsole config) ++ 
              ", enableAsync=" ++ show (enableAsync config) ++
              ", minLogLevel=" ++ show (minLogLevel config)
    
    -- Call the actual logMessage function
    logMessage level msg
    traceIO $ "=== logMessage completed"

-- | Test 1: Basic configuration test
testBasicConfig :: IO ()
testBasicConfig = do
    putStrLn "\n=== Test 1: Basic Configuration ==="
    
    -- Test with file-only config
    withSystemTempFile "debug.log" $ \path handle -> do
        let config = LogConfig
                { minLogLevel = INFO
                , logHandle = handle
                , enableAsync = False
                , bufferSize = 1000
                , enableConsole = False  -- Should prevent console output
                }
        
        putStrLn $ "Setting config with enableConsole = " ++ show (enableConsole config)
        withLogging config $ do
            putStrLn "About to call debugLogMessage..."
            liftIO $ debugLogMessage INFO "Test message 1"
            putStrLn "debugLogMessage completed"

-- | Test 2: Async vs Sync behavior
testAsyncVsSync :: IO ()
testAsyncVsSync = do
    putStrLn "\n=== Test 2: Async vs Sync ==="
    
    withSystemTempFile "debug-sync.log" $ \syncPath syncHandle -> do
        withSystemTempFile "debug-async.log" $ \asyncPath asyncHandle -> do
            
            putStrLn "Testing SYNC logging..."
            let syncConfig = LogConfig
                    { minLogLevel = INFO
                    , logHandle = syncHandle  
                    , enableAsync = False
                    , bufferSize = 1000
                    , enableConsole = False
                    }
            withLogging syncConfig $ do
                liftIO $ debugLogMessage INFO "Sync test message"
            
            putStrLn "Testing ASYNC logging..."
            let asyncConfig = LogConfig
                    { minLogLevel = INFO
                    , logHandle = asyncHandle
                    , enableAsync = True
                    , bufferSize = 1000
                    , enableConsole = False
                    }
            withLogging asyncConfig $ do
                liftIO $ debugLogMessage INFO "Async test message"
                liftIO $ threadDelay 100000  -- Wait for async processing

-- | Test 3: Global state investigation
testGlobalState :: IO ()
testGlobalState = do
    putStrLn "\n=== Test 3: Global State Investigation ==="
    
    -- Check initial global state
    initialConfig <- readIORef globalLogConfig
    putStrLn $ "Initial global config: enableConsole=" ++ show (enableConsole initialConfig)
    
    withSystemTempFile "debug-global.log" $ \path handle -> do
        let newConfig = LogConfig
                { minLogLevel = INFO
                , logHandle = handle
                , enableAsync = False  
                , bufferSize = 1000
                , enableConsole = False  -- This should be respected
                }
        
        putStrLn "Before withLogging..."
        withLogging newConfig $ do
            configDuringTest <- liftIO $ readIORef globalLogConfig
            liftIO $ putStrLn $ "Config during test: enableConsole=" ++ show (enableConsole configDuringTest)
            
            liftIO $ debugLogMessage INFO "Global state test message"
        
        configAfterTest <- readIORef globalLogConfig  
        putStrLn $ "Config after test: enableConsole=" ++ show (enableConsole configAfterTest)

-- | Test 4: Direct writeLogEntry test
testDirectWriteLogEntry :: IO ()
testDirectWriteLogEntry = do
    putStrLn "\n=== Test 4: Direct writeLogEntry Test ==="
    
    withSystemTempFile "debug-direct.log" $ \path handle -> do
        let config = LogConfig
                { minLogLevel = INFO
                , logHandle = handle
                , enableAsync = False
                , bufferSize = 1000
                , enableConsole = False  -- Should NOT write to console
                }
        
        timestamp <- getCurrentTime
        let entry = LogEntry INFO timestamp "Direct test message" "debug"
        
        putStrLn "Calling writeLogEntry directly..."
        writeLogEntry config entry
        putStrLn "writeLogEntry completed"

-- | Test 5: Check if there are other logging systems
testAlternativeLoggingSources :: IO ()
testAlternativeLoggingSources = do
    putStrLn "\n=== Test 5: Alternative Logging Sources ==="
    
    -- Test if the messages might be coming from FileScanner
    putStrLn "Testing if FileScanner produces console output..."
    
    -- This will help us see if the console messages are coming from
    -- FileScanner logging calls rather than our benchmark logging
    return ()

-- | Run all debug tests
runAllDebugTests :: IO ()
runAllDebugTests = do
    putStrLn "Starting systematic logging debug tests..."
    
    testBasicConfig
    testAsyncVsSync  
    testGlobalState
    testDirectWriteLogEntry
    testAlternativeLoggingSources
    
    putStrLn "\nDebug tests completed!"
