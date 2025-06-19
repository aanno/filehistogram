module LoggingBench where

import Gauge
import Control.Monad (replicateM_, forM_, replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Async
import System.IO (withFile, IOMode(..), hClose, stderr)
import System.IO.Temp (withSystemTempFile)

import Logging

-- | Benchmark configuration
data BenchConfig = BenchConfig
    { numMessages :: Int
    , numThreads :: Int
    , messageSize :: Int
    } deriving (Show)

-- | Different benchmark scenarios
lightLoad, mediumLoad, heavyLoad :: BenchConfig
lightLoad = BenchConfig 1000 1 50
mediumLoad = BenchConfig 10000 4 100
heavyLoad = BenchConfig 100000 16 200

-- | Generate a test message of specified size
generateMessage :: Int -> String
generateMessage size = take size $ cycle "This is a test log message with some content. "

-- | Main benchmark suite
main :: IO ()
main = do
    -- Set global config to disable console but allow INFO level logging
    let globalQuietConfig = LogConfig 
            { minLogLevel = DEBUG  -- Allow all log levels for benchmarks
            , logHandle = stderr   -- Default handle (will be overridden by withLogging)
            , enableAsync = False
            , bufferSize = 1000
            , enableConsole = False  -- CRITICAL: Disable console globally
            }
    initLogging globalQuietConfig
    
    withSystemTempFile "bench.log" $ \path1 handle1 -> do
        hClose handle1
        withSystemTempFile "bench-concurrent.log" $ \path2 handle2 -> do
            hClose handle2
            withSystemTempFile "bench-levels.log" $ \path3 handle3 -> do
                hClose handle3
                
                let mediumConfig = mediumLoad
                    msg = generateMessage (messageSize mediumConfig)
                    largeMsg = generateMessage 1000
                
                defaultMain
                    [ bgroup "Sync vs Async Performance"
                        [ bench "sync logging" $ nfIO $ do
                            withFile path1 WriteMode $ \h -> do
                                let syncConfig = LogConfig 
                                        { enableAsync = False
                                        , logHandle = h
                                        , minLogLevel = INFO
                                        , bufferSize = 1000
                                        , enableConsole = False
                                        }
                                withLogging syncConfig $ do
                                    replicateM_ 1000 $ logInfo msg
                        
                        , bench "async logging" $ nfIO $ do
                            withFile path1 WriteMode $ \h -> do
                                let asyncConfig = LogConfig 
                                        { enableAsync = True
                                        , logHandle = h
                                        , minLogLevel = INFO
                                        , bufferSize = 10000
                                        , enableConsole = False
                                        }
                                withLogging asyncConfig $ do
                                    replicateM_ 1000 $ logInfo msg
                        ]
                    
                    , bgroup "Concurrent Logging"
                        [ bench "single thread" $ nfIO $ do
                            withFile path2 WriteMode $ \h -> do
                                let logConfig = LogConfig 
                                        { enableAsync = False
                                        , logHandle = h
                                        , minLogLevel = INFO
                                        , bufferSize = 1000
                                        , enableConsole = False
                                        }
                                withLogging logConfig $ do
                                    replicateM_ 2000 $ logInfo msg
                        
                        , bench "multiple threads" $ nfIO $ do
                            withFile path2 WriteMode $ \h -> do
                                let logConfig = LogConfig 
                                        { enableAsync = False
                                        , logHandle = h
                                        , minLogLevel = INFO
                                        , bufferSize = 1000
                                        , enableConsole = False
                                        }
                                withLogging logConfig $ do
                                    let msgsPerThread = 200
                                    asyncs <- replicateM 2 $ async $ do
                                        replicateM_ msgsPerThread $ logInfo msg
                                    mapM_ wait asyncs
                        ]
                    
                    , bgroup "Log Level Filtering"
                        [ bench "all levels (DEBUG)" $ nfIO $ do
                            withFile path3 WriteMode $ \h -> do
                                let logConfig = LogConfig 
                                        { enableAsync = False
                                        , logHandle = h
                                        , minLogLevel = DEBUG
                                        , bufferSize = 1000
                                        , enableConsole = False
                                        }
                                withLogging logConfig $ do
                                    forM_ ([1..2000] :: [Int]) $ \i -> do
                                        case i `mod` 4 of
                                            0 -> logDebug msg
                                            1 -> logInfo msg
                                            2 -> logWarn msg
                                            _ -> logError msg
                        
                        , bench "filtered (ERROR only)" $ nfIO $ do
                            withFile path3 WriteMode $ \h -> do
                                let logConfig = LogConfig 
                                        { enableAsync = False
                                        , logHandle = h
                                        , minLogLevel = ERROR
                                        , bufferSize = 1000
                                        , enableConsole = False
                                        }
                                withLogging logConfig $ do
                                    forM_ ([1..2000] :: [Int]) $ \i -> do
                                        case i `mod` 4 of
                                            0 -> logDebug msg
                                            1 -> logInfo msg
                                            2 -> logWarn msg
                                            _ -> logError msg
                        ]
                    
                    , bgroup "Memory Usage"
                        [ bench "many small messages" $ nfIO $ do
                            withFile path3 WriteMode $ \h -> do
                                let logConfig = LogConfig 
                                        { enableAsync = False
                                        , logHandle = h
                                        , minLogLevel = INFO
                                        , bufferSize = 1000
                                        , enableConsole = False
                                        }
                                withLogging logConfig $ do
                                    replicateM_ 5000 $ logInfo msg
                        
                        , bench "few large messages" $ nfIO $ do
                            withFile path3 WriteMode $ \h -> do
                                let logConfig = LogConfig 
                                        { enableAsync = False
                                        , logHandle = h
                                        , minLogLevel = INFO
                                        , bufferSize = 1000
                                        , enableConsole = False
                                        }
                                withLogging logConfig $ do
                                    replicateM_ 500 $ logInfo largeMsg
                        ]
                    ]
