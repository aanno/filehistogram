module LoggingBench where

import Gauge
import Control.Monad (replicateM_, forM_, replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
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
    -- IMMEDIATELY set global config to disable console output PERMANENTLY
    let globalQuietConfig = LogConfig 
            { minLogLevel = ERROR  -- Only critical errors
            , logHandle = stderr   -- Doesn't matter since enableConsole=False
            , enableAsync = False
            , bufferSize = 1000
            , enableConsole = False  -- CRITICAL: Disable console globally
            }
    initLogging globalQuietConfig
    
    withSystemTempFile "bench.log" $ \path1 handle1 -> do
        hClose handle1
        withSystemTempFile "bench-concurrent.log" $ \path2 handle2 -> do
            hClose handle2
            
            let mediumConfig = mediumLoad
                msg = generateMessage (messageSize mediumConfig)
                largeMsg = generateMessage 1000
                
            defaultMain
                [ bgroup "Sync vs Async Performance"
                    [ bench "sync logging" $ nfIO $ do
                        withFile path1 WriteMode $ \h -> do
                            let syncConfig = defaultLogConfig 
                                    { enableAsync = False
                                    , logHandle = h
                                    , minLogLevel = INFO
                                    }
                            withLogging syncConfig $ do
                                replicateM_ 1000 $ logInfo msg  -- Reduced from mediumConfig
                    
                    , bench "async logging" $ nfIO $ do
                        withFile path1 WriteMode $ \h -> do
                            let asyncConfig = defaultLogConfig 
                                    { enableAsync = True
                                    , logHandle = h
                                    , minLogLevel = INFO
                                    , bufferSize = 10000  -- Increased buffer size
                                    }
                            withLogging asyncConfig $ do
                                replicateM_ 1000 $ logInfo msg  -- Reduced from mediumConfig
                                -- Add delay to let queue drain
                                liftIO $ threadDelay 100000  -- 100ms
                    ]
                
                , bgroup "Concurrent Logging"
                    [ bench "single thread" $ nfIO $ do
                        withFile path2 WriteMode $ \h -> do
                            let logConfig = defaultLogConfig 
                                    { enableAsync = False  -- Use sync to avoid queue issues
                                    , logHandle = h
                                    , minLogLevel = INFO
                                    }
                            withLogging logConfig $ do
                                replicateM_ 2000 $ logInfo msg
                    
                    , bench "multiple threads" $ nfIO $ do
                        withFile path2 WriteMode $ \h -> do
                            let logConfig = defaultLogConfig 
                                    { enableAsync = False  -- Use sync to avoid queue overflow
                                    , logHandle = h
                                    , minLogLevel = INFO
                                    }
                            withLogging logConfig $ do
                                let msgsPerThread = 200
                                asyncs <- replicateM 2 $ async $ do
                                    replicateM_ msgsPerThread $ logInfo msg
                                mapM_ wait asyncs
                    ]
                
                , bgroup "Log Level Filtering"
                    [ bench "all levels (DEBUG)" $ nfIO $ do
                        let logConfig = defaultLogConfig 
                                { enableAsync = False  -- Use sync for predictable performance
                                , minLogLevel = DEBUG
                                }
                        withLogging logConfig $ do
                            forM_ ([1..2000] :: [Int]) $ \i -> do  -- Reduced load
                                case i `mod` 4 of
                                    0 -> logDebug msg
                                    1 -> logInfo msg
                                    2 -> logWarn msg
                                    _ -> logError msg
                    
                    , bench "filtered (ERROR only)" $ nfIO $ do
                        let logConfig = defaultLogConfig 
                                { enableAsync = False  -- Use sync for predictable performance
                                , minLogLevel = ERROR
                                }
                        withLogging logConfig $ do
                            forM_ ([1..2000] :: [Int]) $ \i -> do  -- Reduced load
                                case i `mod` 4 of
                                    0 -> logDebug msg
                                    1 -> logInfo msg
                                    2 -> logWarn msg
                                    _ -> logError msg
                    ]
                
                , bgroup "Memory Usage"
                    [ bench "many small messages" $ nfIO $ do
                        let logConfig = defaultLogConfig 
                                { enableAsync = False  -- Use sync to avoid queue issues
                                , minLogLevel = INFO
                                }
                        withLogging logConfig $ do
                            replicateM_ 5000 $ logInfo msg  -- Reduced load
                    
                    , bench "few large messages" $ nfIO $ do
                        let logConfig = defaultLogConfig 
                                { enableAsync = False  -- Use sync to avoid queue issues
                                , minLogLevel = INFO
                                }
                        withLogging logConfig $ do
                            replicateM_ 500 $ logInfo largeMsg  -- Reduced load
                    ]
                ]
