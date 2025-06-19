{-# LANGUAGE OverloadedStrings #-}

-- | Complete benchmark suite for filehistogram
module Main where

import System.Environment (getArgs, withArgs)
import System.IO (stderr)
import Logging

-- Import our benchmark modules  
import qualified LoggingBench as LB
import qualified FileScannerBench as FSB
import qualified FileHistogramBench as FHB
import qualified Data.Text.Array as FHB

main :: IO ()
main = do
    -- ALWAYS disable console logging FIRST, before anything else
    let globalQuietConfig = LogConfig 
            { minLogLevel = DEBUG  -- Allow all log levels for benchmarks
            , logHandle = stderr   -- Default handle (will be overridden)
            , enableAsync = False
            , bufferSize = 1000
            , enableConsole = False  -- CRITICAL: Disable console globally
            }
    initLogging globalQuietConfig
    
    args <- getArgs
    case args of
        ["logging"] -> do
            putStrLn "Running logging benchmarks only..."
            -- Clear args so LoggingBench.main doesn't see "logging"
            withArgs [] LB.main
            
        ["scanner"] -> do
            putStrLn "Running file scanner benchmarks only..."
            -- Clear args so FileScannerBench doesn't see "scanner"  
            withArgs [] FSB.runFileScannerBenchmarks
            
        ["histogram"] -> do
            putStrLn "Running file scanner benchmarks only..."
            -- Clear args so FileScannerBench doesn't see "scanner"  
            withArgs [] FHB.runFileHistogramBenchmarks

        [] -> do
            putStrLn "Running logging benchmarks (default)..."
            putStrLn $ "\n" ++ replicate 60 '='
            putStrLn "LOGGING BENCHMARKS"
            putStrLn $ replicate 60 '='
            withArgs [] LB.main
            
        _ -> do
            -- Pass through any other arguments to LoggingBench for standard gauge options
            putStrLn "Running logging benchmarks with gauge arguments..."
            LB.main
