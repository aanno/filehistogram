{-# LANGUAGE OverloadedStrings #-}

module FileScannerBench where

import Gauge
import Control.Monad (forM_)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold

import FileScanner
import Logging

-- | Create a test directory structure for benchmarking
createTestDirectoryStructure :: FilePath -> Int -> Int -> Int -> IO ()
createTestDirectoryStructure basePath numDirs filesPerDir sizeBytes = do
    forM_ [1..numDirs] $ \dirNum -> do
        let dirPath = basePath </> ("dir" ++ show dirNum)
        createDirectoryIfMissing True dirPath
        
        forM_ [1..filesPerDir] $ \fileNum -> do
            let testFilePath = dirPath </> ("file" ++ show fileNum ++ ".txt")
                content = take sizeBytes $ cycle "test data content for benchmarking purposes "
            writeFile testFilePath content

-- | Benchmark configurations for different scenarios
data ScanBenchConfig = ScanBenchConfig
    { numDirectories :: Int
    , filesPerDirectory :: Int
    , fileSizeBytes :: Int
    , maxDepthLimit :: Maybe Int
    } deriving (Show)

smallStructure, mediumStructure, largeStructure :: ScanBenchConfig
smallStructure = ScanBenchConfig 10 50 1024 Nothing
mediumStructure = ScanBenchConfig 50 100 4096 Nothing
largeStructure = ScanBenchConfig 100 500 8192 (Just 5)

-- | Benchmark streaming vs collecting
benchmarkStreamingVsCollecting :: IO ()
benchmarkStreamingVsCollecting = do
    withSystemTempDirectory "filescanner-bench" $ \tempDir -> do
        let config = mediumStructure
        
        -- Create test structure
        createTestDirectoryStructure tempDir 
            (numDirectories config) 
            (filesPerDirectory config) 
            (fileSizeBytes config)
        
        let scanOpts = defaultScanOptions
        
        defaultMain
            [ bgroup "Streaming vs Collecting"
                [ bench "collect all files" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = WARN }
                    files <- scanFiles scanOpts tempDir
                    return $ length files
                
                , bench "stream processing" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = WARN }
                    count <- S.fold Fold.length $ scanFilesStream scanOpts tempDir
                    return count
                
                , bench "stream with early termination" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = WARN }
                    -- Take only first 100 files
                    count <- S.fold Fold.length $ S.take 100 $ scanFilesStream scanOpts tempDir
                    return count
                ]
            ]

-- | Benchmark different scan options
benchmarkScanOptions :: IO ()
benchmarkScanOptions = do
    withSystemTempDirectory "filescanner-options" $ \tempDir -> do
        let config = mediumStructure
        
        -- Create test structure with some symlinks
        createTestDirectoryStructure tempDir 
            (numDirectories config) 
            (filesPerDirectory config) 
            (fileSizeBytes config)
        
        defaultMain
            [ bgroup "Scan Options Performance"
                [ bench "default options" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR }
                    files <- scanFiles defaultScanOptions tempDir
                    return $ length files
                
                , bench "with depth limit 3" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR }
                    let opts = defaultScanOptions { maxDepth = Just 3 }
                    files <- scanFiles opts tempDir
                    return $ length files
                
                , bench "follow symlinks" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR }
                    let opts = defaultScanOptions { followSymlinks = True }
                    files <- scanFiles opts tempDir
                    return $ length files
                ]
            ]

-- | Benchmark memory usage patterns
benchmarkMemoryPatterns :: IO ()
benchmarkMemoryPatterns = do
    withSystemTempDirectory "filescanner-memory" $ \tempDir -> do
        -- Create a moderate directory structure (not too large)
        createTestDirectoryStructure tempDir 20 100 1024  -- Reduced size
        
        defaultMain
            [ bgroup "Memory Usage Patterns"
                [ bench "collect all (high memory)" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    files <- scanFiles defaultScanOptions tempDir
                    return $ length files
                
                , bench "stream fold count (low memory)" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    count <- S.fold Fold.length $ scanFilesStream defaultScanOptions tempDir
                    return count
                
                , bench "stream fold sizes (low memory)" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    totalSize <- S.fold (Fold.foldl' (+) 0) $ 
                        S.mapM (return . fileSize) $ scanFilesStream defaultScanOptions tempDir
                    return totalSize
                
                , bench "stream with limit (constant memory)" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    count <- S.fold Fold.length $ S.take 500 $ scanFilesStream defaultScanOptions tempDir
                    return count
                ]
            ]

-- | Benchmark concurrent file processing simulation
benchmarkConcurrentProcessing :: IO ()
benchmarkConcurrentProcessing = do
    withSystemTempDirectory "filescanner-concurrent" $ \tempDir -> do
        let config = mediumStructure
        
        createTestDirectoryStructure tempDir 
            (numDirectories config) 
            (filesPerDirectory config) 
            (fileSizeBytes config)
        
        defaultMain
            [ bgroup "Concurrent Processing Simulation"
                [ bench "sequential processing" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR }
                    files <- scanFiles defaultScanOptions tempDir
                    -- Simulate processing each file
                    total <- return $ sum $ map fileSize files
                    return total
                
                , bench "stream with parallel map simulation" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR }
                    -- Simulate concurrent processing using stream
                    total <- S.fold (Fold.foldl' (+) 0) $ 
                        S.mapM (\info -> do
                            -- Simulate some processing work
                            let size = fileSize info
                            return size) $ 
                        scanFilesStream defaultScanOptions tempDir
                    return total
                ]
            ]

-- | Benchmark different directory structures
benchmarkDirectoryStructures :: IO ()
benchmarkDirectoryStructures = do
    withSystemTempDirectory "filescanner-structures" $ \tempDir -> do
        -- Create different directory structures
        let flatDir = tempDir </> "flat"
            deepDir = tempDir </> "deep"
            wideDir = tempDir </> "wide"
        
        -- Flat structure: few dirs, many files per dir
        createTestDirectoryStructure flatDir 5 2000 1024
        
        -- Deep structure: many nested dirs, few files per dir  
        createDeepStructure deepDir 10 5 1024
        
        -- Wide structure: many dirs, moderate files per dir
        createTestDirectoryStructure wideDir 100 100 1024
        
        defaultMain
            [ bgroup "Directory Structure Types"
                [ bench "flat structure" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR }
                    count <- S.fold Fold.length $ scanFilesStream defaultScanOptions flatDir
                    return count
                
                , bench "deep structure" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR }
                    count <- S.fold Fold.length $ scanFilesStream defaultScanOptions deepDir
                    return count
                
                , bench "wide structure" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR }
                    count <- S.fold Fold.length $ scanFilesStream defaultScanOptions wideDir
                    return count
                ]
            ]

-- | Create a deep directory structure for testing
createDeepStructure :: FilePath -> Int -> Int -> Int -> IO ()
createDeepStructure basePath depth filesPerLevel sizeBytes = go basePath 0
  where
    go currentPath currentDepth
        | currentDepth >= depth = return ()
        | otherwise = do
            createDirectoryIfMissing True currentPath
            
            -- Create files at this level
            forM_ [1..filesPerLevel] $ \fileNum -> do
                let testFilePath = currentPath </> ("file" ++ show fileNum ++ ".txt")
                    content = take sizeBytes $ cycle "deep structure test data "
                writeFile testFilePath content
            
            -- Create next level
            let nextPath = currentPath </> ("level" ++ show (currentDepth + 1))
            go nextPath (currentDepth + 1)

-- | Run all FileScanner benchmarks
runFileScannerBenchmarks :: IO ()
runFileScannerBenchmarks = do
    putStrLn "Running FileScanner performance benchmarks..."
    
    putStrLn "\n=== Streaming vs Collecting ==="
    benchmarkStreamingVsCollecting
    
    putStrLn "\n=== Scan Options Impact ==="
    benchmarkScanOptions
    
    putStrLn "\n=== Memory Usage Patterns ==="
    benchmarkMemoryPatterns
    
    putStrLn "\n=== Concurrent Processing Simulation ==="
    benchmarkConcurrentProcessing
    
    putStrLn "\n=== Directory Structure Types ==="
    benchmarkDirectoryStructures
