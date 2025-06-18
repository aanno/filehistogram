{-# LANGUAGE OverloadedStrings #-}

module LoggingBench where

import Gauge
import Control.Monad (replicateM_, forM_, replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import System.IO (withFile, IOMode(..), hClose)
import System.IO.Temp (withSystemTempFile)
import qualified Streamly.Data.Stream.Prelude as S

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

-- | Benchmark sync vs async logging
benchmarkSyncVsAsync :: IO ()
benchmarkSyncVsAsync = do
    withSystemTempFile "bench.log" $ \path handle -> do
        hClose handle  -- Close so we can reopen in different modes
        
        let config = mediumLoad
            msg = generateMessage (messageSize config)
            
        defaultMain
            [ bgroup "Logging Performance"
                [ bench "sync logging" $ nfIO $ do
                    withFile path WriteMode $ \h -> do
                        let syncConfig = defaultLogConfig 
                                { enableAsync = False
                                , logHandle = h
                                , minLogLevel = INFO
                                }
                        withLogging syncConfig $ do
                            replicateM_ (numMessages config) $ logInfo msg
                
                , bench "async logging" $ nfIO $ do
                    withFile path WriteMode $ \h -> do
                        let asyncConfig = defaultLogConfig 
                                { enableAsync = True
                                , logHandle = h
                                , minLogLevel = INFO
                                }
                        withLogging asyncConfig $ do
                            replicateM_ (numMessages config) $ logInfo msg
                ]
            ]

-- | Benchmark concurrent logging from multiple threads
benchmarkConcurrency :: IO ()
benchmarkConcurrency = do
    withSystemTempFile "bench-concurrent.log" $ \path handle -> do
        hClose handle
        
        let config = heavyLoad
            msg = generateMessage (messageSize config)
            msgsPerThread = numMessages config `div` numThreads config
            
        defaultMain
            [ bgroup "Concurrent Logging"
                [ bench "single thread" $ nfIO $ do
                    withFile path WriteMode $ \h -> do
                        let logConfig = defaultLogConfig 
                                { enableAsync = True
                                , logHandle = h
                                , minLogLevel = INFO
                                }
                        withLogging logConfig $ do
                            replicateM_ (numMessages config) $ logInfo msg
                
                , bench "multiple threads" $ nfIO $ do
                    withFile path WriteMode $ \h -> do
                        let logConfig = defaultLogConfig 
                                { enableAsync = True
                                , logHandle = h
                                , minLogLevel = INFO
                                }
                        withLogging logConfig $ do
                            asyncs <- replicateM (numThreads config) $ async $ do
                                replicateM_ msgsPerThread $ logInfo msg
                            mapM_ wait asyncs
                ]
            ]

-- | Benchmark different log levels
benchmarkLogLevels :: IO ()
benchmarkLogLevels = do
    withSystemTempFile "bench-levels.log" $ \path handle -> do
        hClose handle
        
        let config = mediumLoad
            msg = generateMessage (messageSize config)
            
        defaultMain
            [ bgroup "Log Level Filtering"
                [ bench "all levels (DEBUG)" $ nfIO $ do
                    withFile path WriteMode $ \h -> do
                        let logConfig = defaultLogConfig 
                                { enableAsync = True
                                , logHandle = h
                                , minLogLevel = DEBUG
                                }
                        withLogging logConfig $ do
                            forM_ [1..numMessages config] $ \i -> do
                                case i `mod` 4 of
                                    0 -> logDebug msg
                                    1 -> logInfo msg
                                    2 -> logWarn msg
                                    _ -> logError msg
                
                , bench "filtered (ERROR only)" $ nfIO $ do
                    withFile path WriteMode $ \h -> do
                        let logConfig = defaultLogConfig 
                                { enableAsync = True
                                , logHandle = h
                                , minLogLevel = ERROR
                                }
                        withLogging logConfig $ do
                            forM_ [1..numMessages config] $ \i -> do
                                case i `mod` 4 of
                                    0 -> logDebug msg
                                    1 -> logInfo msg
                                    2 -> logWarn msg
                                    _ -> logError msg
                ]
            ]

-- | Benchmark stream-based logging
benchmarkStreamLogging :: IO ()
benchmarkStreamLogging = do
    withSystemTempFile "bench-stream.log" $ \path handle -> do
        hClose handle
        
        let config = mediumLoad
            msg = generateMessage (messageSize config)
            
        defaultMain
            [ bgroup "Stream vs Individual Logging"
                [ bench "individual log calls" $ nfIO $ do
                    withFile path WriteMode $ \h -> do
                        let logConfig = defaultLogConfig 
                                { enableAsync = True
                                , logHandle = h
                                , minLogLevel = INFO
                                }
                        withLogging logConfig $ do
                            replicateM_ (numMessages config) $ logInfo msg
                
                , bench "stream logging" $ nfIO $ do
                    withFile path WriteMode $ \h -> do
                        let logConfig = defaultLogConfig 
                                { enableAsync = True
                                , logHandle = h
                                , minLogLevel = INFO
                                }
                        withLogging logConfig $ do
                            let msgStream = S.replicate (numMessages config) msg
                                logEntries = logToStream INFO msgStream
                            _ <- S.toList logEntries  -- Just consume the stream
                            return ()
                ]
            ]

-- | Benchmark queue pressure under load
benchmarkQueuePressure :: IO ()
benchmarkQueuePressure = do
    withSystemTempFile "bench-pressure.log" $ \path handle -> do
        hClose handle
        
        let msg = generateMessage 1000  -- Large messages
            
        defaultMain
            [ bgroup "Queue Pressure"
                [ bench "fast producer" $ nfIO $ do
                    withFile path WriteMode $ \h -> do
                        let logConfig = defaultLogConfig 
                                { enableAsync = True
                                , logHandle = h
                                , minLogLevel = INFO
                                }
                        withLogging logConfig $ do
                            -- Rapidly produce many large messages
                            replicateM_ 50000 $ logInfo msg
                
                , bench "slow consumer simulation" $ nfIO $ do
                    withFile path WriteMode $ \h -> do
                        let logConfig = defaultLogConfig 
                                { enableAsync = True
                                , logHandle = h
                                , minLogLevel = INFO
                                }
                        withLogging logConfig $ do
                            -- Simulate slower consumer by adding delays
                            forM_ ([1..1000] :: [Int]) $ \i -> do
                                logInfo msg
                                when (i `mod` 100 == 0) $ 
                                    liftIO $ threadDelay 1000  -- 1ms delay every 100 messages
                ]
            ]
  where
    when True action = action
    when False _ = return ()

-- | Memory usage benchmark
benchmarkMemoryUsage :: IO ()
benchmarkMemoryUsage = do
    let msg = generateMessage 100
        
    defaultMain
        [ bgroup "Memory Usage"
            [ bench "many small messages" $ nfIO $ do
                let logConfig = defaultLogConfig 
                        { enableAsync = True
                        , minLogLevel = INFO
                        }
                withLogging logConfig $ do
                    replicateM_ 100000 $ logInfo msg
            
            , bench "few large messages" $ nfIO $ do
                let largeMsg = generateMessage 10000
                    logConfig = defaultLogConfig 
                        { enableAsync = True
                        , minLogLevel = INFO
                        }
                withLogging logConfig $ do
                    replicateM_ 1000 $ logInfo largeMsg
            ]
        ]

-- | Comprehensive benchmark suite
main :: IO ()
main = do
    putStrLn "Running comprehensive logging benchmarks..."
    putStrLn "This will test sync vs async, concurrency, filtering, and queue pressure."
    
    putStrLn "\n=== Sync vs Async Performance ==="
    benchmarkSyncVsAsync
    
    putStrLn "\n=== Concurrency Benefits ==="
    benchmarkConcurrency
    
    putStrLn "\n=== Log Level Filtering ==="
    benchmarkLogLevels
    
    putStrLn "\n=== Stream vs Individual Logging ==="
    benchmarkStreamLogging
    
    putStrLn "\n=== Queue Pressure Testing ==="
    benchmarkQueuePressure
    
    putStrLn "\n=== Memory Usage Patterns ==="
    benchmarkMemoryUsage
