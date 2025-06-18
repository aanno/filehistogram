{-# LANGUAGE OverloadedStrings #-}

-- | Complete benchmark suite for filehistogram
module Main where

import System.Environment (getArgs)

-- Import our benchmark modules  
import qualified LoggingBench as LB
import qualified FileScannerBench as FSB

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["logging"] -> do
            putStrLn "Running logging benchmarks only..."
            LB.main
            
        ["scanner"] -> do
            putStrLn "Running file scanner benchmarks only..."
            FSB.runFileScannerBenchmarks
            
        ["all"] -> do
            putStrLn "Running complete benchmark suite..."
            putStrLn $ "\n" ++ replicate 60 '='
            putStrLn "LOGGING BENCHMARKS"
            putStrLn $ replicate 60 '='
            LB.main
            
            putStrLn $ "\n" ++ replicate 60 '='  
            putStrLn "FILE SCANNER BENCHMARKS"
            putStrLn $ replicate 60 '='
            FSB.runFileScannerBenchmarks
            
        _ -> do
            putStrLn "Usage: benchmark [logging|scanner|all]"
            putStrLn ""
            putStrLn "  logging  - Run only logging performance tests"
            putStrLn "  scanner  - Run only file scanner performance tests"  
            putStrLn "  all      - Run complete benchmark suite"
            putStrLn ""
            putStrLn "Examples:"
            putStrLn "  stack bench --benchmark-arguments=\"logging\""
            putStrLn "  stack bench --benchmark-arguments=\"all\""
