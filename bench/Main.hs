{-# LANGUAGE OverloadedStrings #-}

-- | Complete benchmark suite for filehistogram
module Main where

import System.Environment (getArgs, withArgs)

-- Import our benchmark modules  
import qualified LoggingBench as LB
import qualified FileScannerBench as FSB

main :: IO ()
main = do
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
