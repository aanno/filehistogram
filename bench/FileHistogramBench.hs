{-# LANGUAGE OverloadedStrings #-}

module FileHistogramBench where

import Gauge
import Control.Monad (forM_)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeExtension)
import qualified Data.Text.Lazy as TL
import qualified Graphics.Vega.VegaLite as VL
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold
import qualified Data.Map.Strict as Map
import Data.List (sort)

import FileScanner
import FileHistogram
import Logging

-- | Configuration for benchmark test data
data HistogramBenchConfig = HistogramBenchConfig
    { numDirectories :: Int
    , filesPerDirectory :: Int
    , fileExtensions :: [String]  -- Different file types to create
    , fileSizeRange :: (Int, Int)  -- Min, max file sizes
    , enableSubdirs :: Bool
    } deriving (Show)

-- Different benchmark scenarios
smallDataset, mediumDataset, largeDataset, diverseDataset :: HistogramBenchConfig
smallDataset = HistogramBenchConfig 5 20 [".txt", ".log"] (100, 1000) False
mediumDataset = HistogramBenchConfig 20 100 [".txt", ".log", ".json", ".csv"] (500, 10000) True
largeDataset = HistogramBenchConfig 50 500 [".txt", ".log", ".json", ".csv", ".xml"] (1000, 100000) True
diverseDataset = HistogramBenchConfig 30 200 [".txt", ".log", ".json", ".csv", ".xml", ".md", ".yml", ".conf", ".data", ".tmp"] (10, 1000000) True

-- | Create test files with diverse sizes and extensions
createHistogramTestStructure :: FilePath -> HistogramBenchConfig -> IO ()
createHistogramTestStructure basePath config = do
    let extensions = fileExtensions config
        (minSize, maxSize) = fileSizeRange config
    
    forM_ [1..numDirectories config] $ \dirNum -> do
        let dirPath = basePath </> ("dir" ++ show dirNum)
        createDirectoryIfMissing True dirPath
        
        -- Optionally create subdirectories
        when (enableSubdirs config) $ do
            let subDir = dirPath </> ("subdir" ++ show dirNum)
            createDirectoryIfMissing True subDir
            createFilesInDir subDir (filesPerDirectory config `div` 3) extensions minSize maxSize
        
        createFilesInDir dirPath (filesPerDirectory config) extensions minSize maxSize

-- | Create files in a directory with varying sizes and extensions
createFilesInDir :: FilePath -> Int -> [String] -> Int -> Int -> IO ()
createFilesInDir dirPath numFiles extensions minSize maxSize = do
    forM_ [1..numFiles] $ \fileNum -> do
        let ext = extensions !! (fileNum `mod` length extensions)
            fileName = "file" ++ show fileNum ++ ext
            filePath = dirPath </> fileName
            -- Create files with logarithmically distributed sizes for realistic testing
            sizeMultiplier = fromIntegral fileNum / fromIntegral numFiles
            fileSize = round $ fromIntegral minSize * exp (sizeMultiplier * log (fromIntegral maxSize / fromIntegral minSize))
            content = take fileSize $ cycle ("test content for " ++ ext ++ " file type ")
        writeFile filePath content

-- | Benchmark histogram generation methods
benchmarkHistogramGeneration :: IO ()
benchmarkHistogramGeneration = do
    withSystemTempDirectory "histogram-generation" $ \tempDir -> do
        let config = mediumDataset
        
        createHistogramTestStructure tempDir config
        
        let scanOpts = defaultScanOptions
        
        defaultMain
            [ bgroup "Histogram Generation Methods"
                [ bench "traditional processing" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    sizes <- getFileSizes scanOpts tempDir
                    let histogram = createHistogram sizes
                    return $ VL.toHtml histogram  -- Force evaluation
                
                , bench "streaming processing" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    sizes <- S.fold Fold.toList $ getFileSizesStream scanOpts tempDir
                    let histogram = createHistogram sizes
                    return $ VL.toHtml histogram
                
                , bench "incremental stream processing" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    (count, minSize, maxSize, sizes) <- S.fold 
                        (collectHistogramStats <$> Fold.length <*> Fold.minimum <*> Fold.maximum <*> Fold.toList)
                        (getFileSizesStream scanOpts tempDir)
                    let histogram = createHistogram sizes
                    return $ VL.toHtml histogram
                ]
            ]
  where
    collectHistogramStats :: Int -> Maybe Integer -> Maybe Integer -> [Integer] -> (Int, Maybe Integer, Maybe Integer, [Integer])
    collectHistogramStats c minS maxS sizes = (c, minS, maxS, sizes)

-- | Benchmark memory usage patterns for different dataset sizes
benchmarkMemoryUsage :: IO ()
benchmarkMemoryUsage = do
    withSystemTempDirectory "histogram-memory" $ \tempDir -> do
        -- Create different sized datasets
        let smallDir = tempDir </> "small"
            mediumDir = tempDir </> "medium"
            largeDir = tempDir </> "large"
        
        createHistogramTestStructure smallDir smallDataset
        createHistogramTestStructure mediumDir mediumDataset  
        createHistogramTestStructure largeDir largeDataset
        
        defaultMain
            [ bgroup "Memory Usage by Dataset Size"
                [ bench "small dataset (collect all)" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    sizes <- getFileSizes defaultScanOptions smallDir
                    return $ length sizes
                
                , bench "small dataset (streaming)" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    count <- S.fold Fold.length $ getFileSizesStream defaultScanOptions smallDir
                    return count
                
                , bench "medium dataset (collect all)" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    sizes <- getFileSizes defaultScanOptions mediumDir
                    return $ length sizes
                
                , bench "medium dataset (streaming)" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    count <- S.fold Fold.length $ getFileSizesStream defaultScanOptions mediumDir
                    return count
                
                , bench "large dataset (streaming only)" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    count <- S.fold Fold.length $ getFileSizesStream defaultScanOptions largeDir
                    return count
                ]
            ]

-- | Benchmark histogram creation with different data characteristics
benchmarkHistogramCharacteristics :: IO ()
benchmarkHistogramCharacteristics = do
    withSystemTempDirectory "histogram-characteristics" $ \tempDir -> do
        -- Create datasets with different characteristics
        let uniformDir = tempDir </> "uniform"
            skewedDir = tempDir </> "skewed"  
            diverseDir = tempDir </> "diverse"
        
        -- Uniform sizes (all files roughly same size)
        createUniformSizeStructure uniformDir 50 100 5000
        
        -- Skewed sizes (few large files, many small files)
        createSkewedSizeStructure skewedDir 50 100
        
        -- Diverse file types and sizes
        createHistogramTestStructure diverseDir diverseDataset
        
        defaultMain
            [ bgroup "Histogram Data Characteristics"
                [ bench "uniform file sizes" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    sizes <- getFileSizes defaultScanOptions uniformDir
                    let histogram = createHistogram sizes
                    return $ TL.length $ VL.toHtml histogram
                
                , bench "skewed file sizes" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    sizes <- getFileSizes defaultScanOptions skewedDir
                    let histogram = createHistogram sizes
                    return $ TL.length $ VL.toHtml histogram
                
                , bench "diverse file types and sizes" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    sizes <- getFileSizes defaultScanOptions diverseDir
                    let histogram = createHistogram sizes
                    return $ TL.length $ VL.toHtml histogram
                ]
            ]

-- | Create uniform-sized files for testing
createUniformSizeStructure :: FilePath -> Int -> Int -> Int -> IO ()
createUniformSizeStructure basePath numDirs filesPerDir uniformSize = do
    forM_ [1..numDirs] $ \dirNum -> do
        let dirPath = basePath </> ("dir" ++ show dirNum)
        createDirectoryIfMissing True dirPath
        
        forM_ [1..filesPerDir] $ \fileNum -> do
            let filePath = dirPath </> ("file" ++ show fileNum ++ ".txt")
                content = take uniformSize $ cycle "uniform size test data "
            writeFile filePath content

-- | Create skewed file size distribution (realistic scenario)
createSkewedSizeStructure :: FilePath -> Int -> Int -> IO ()
createSkewedSizeStructure basePath numDirs filesPerDir = do
    forM_ [1..numDirs] $ \dirNum -> do
        let dirPath = basePath </> ("dir" ++ show dirNum)
        createDirectoryIfMissing True dirPath
        
        forM_ [1..filesPerDir] $ \fileNum -> do
            let filePath = dirPath </> ("file" ++ show fileNum ++ ".txt")
                -- Create skewed distribution: 80% small files, 15% medium, 5% large
                fileSize = if fileNum `mod` 20 < 16
                          then 100 + (fileNum `mod` 900)        -- Small files: 100-1000 bytes
                          else if fileNum `mod` 20 < 19
                               then 10000 + (fileNum `mod` 90000)  -- Medium files: 10KB-100KB
                               else 1000000 + (fileNum `mod` 9000000) -- Large files: 1MB-10MB
                content = take fileSize $ cycle "skewed distribution test data "
            writeFile filePath content

-- | Benchmark histogram processing pipeline performance
benchmarkHistogramPipeline :: IO ()
benchmarkHistogramPipeline = do
    withSystemTempDirectory "histogram-pipeline" $ \tempDir -> do
        let config = mediumDataset
        createHistogramTestStructure tempDir config
        
        defaultMain
            [ bgroup "Histogram Processing Pipeline"
                [ bench "scan + collect + histogram" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    sizes <- getFileSizes defaultScanOptions tempDir
                    let histogram = createHistogram sizes
                        htmlOutput = VL.toHtml histogram
                    return $ TL.length htmlOutput
                
                , bench "scan + stream + histogram" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    sizes <- S.fold Fold.toList $ getFileSizesStream defaultScanOptions tempDir
                    let histogram = createHistogram sizes
                        htmlOutput = VL.toHtml histogram
                    return $ TL.length htmlOutput
                
                , bench "scan only (baseline)" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    count <- S.fold Fold.length $ getFileSizesStream defaultScanOptions tempDir
                    return count
                
                , bench "histogram creation only" $ nfIO $ do
                    -- Pre-generate test data
                    let testSizes = [1..10000] ++ [10000,20000..100000] ++ [100000,200000..1000000]
                    let histogram = createHistogram testSizes
                        htmlOutput = VL.toHtml histogram
                    return $ TL.length htmlOutput
                ]
            ]

-- | Benchmark different scan options impact on histogram generation
benchmarkScanOptionsImpact :: IO ()
benchmarkScanOptionsImpact = do
    withSystemTempDirectory "histogram-scan-options" $ \tempDir -> do
        let config = mediumDataset
        createHistogramTestStructure tempDir config
        
        defaultMain
            [ bgroup "Scan Options Impact on Histogram"
                [ bench "default scan options" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    count <- S.fold Fold.length $ getFileSizesStream defaultScanOptions tempDir
                    return count
                
                , bench "with depth limit 2" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    let opts = defaultScanOptions { maxDepth = Just 2 }
                    count <- S.fold Fold.length $ getFileSizesStream opts tempDir
                    return count
                
                , bench "follow symlinks enabled" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    let opts = defaultScanOptions { followSymlinks = True }
                    count <- S.fold Fold.length $ getFileSizesStream opts tempDir
                    return count
                
                , bench "cross mount boundaries" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    let opts = defaultScanOptions { crossMountBoundaries = True }
                    count <- S.fold Fold.length $ getFileSizesStream opts tempDir
                    return count
                ]
            ]

-- | Benchmark early termination scenarios (useful for large datasets)
benchmarkEarlyTermination :: IO ()
benchmarkEarlyTermination = do
    withSystemTempDirectory "histogram-early-term" $ \tempDir -> do
        let config = largeDataset
        createHistogramTestStructure tempDir config
        
        defaultMain
            [ bgroup "Early Termination Scenarios"
                [ bench "process all files" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    count <- S.fold Fold.length $ getFileSizesStream defaultScanOptions tempDir
                    return count
                
                , bench "first 1000 files only" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    sizes <- S.fold Fold.toList $ S.take 1000 $ getFileSizesStream defaultScanOptions tempDir
                    let histogram = createHistogram sizes
                    return $ TL.length $ VL.toHtml histogram
                
                , bench "first 100 files only" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    sizes <- S.fold Fold.toList $ S.take 100 $ getFileSizesStream defaultScanOptions tempDir
                    let histogram = createHistogram sizes
                    return $ TL.length $ VL.toHtml histogram
                
                , bench "statistical sampling (every 10th file)" $ nfIO $ do
                    initLogging defaultLogConfig { minLogLevel = ERROR, enableConsole = False }
                    sizes <- S.fold Fold.toList $ 
                        S.mapMaybe (\(i, size) -> if i `mod` 10 == 0 then Just size else Nothing) $
                        S.indexed $ getFileSizesStream defaultScanOptions tempDir
                    let histogram = createHistogram sizes
                    return $ TL.length $ VL.toHtml histogram
                ]
            ]

-- | Benchmark file size formatting performance
benchmarkFileSizeFormatting :: IO ()
benchmarkFileSizeFormatting = do
    let testSizes = [1, 1024, 1048576, 1073741824, 1099511627776] -- B, KB, MB, GB, TB
        largeSizeList = concat $ replicate 10000 testSizes
    
    defaultMain
        [ bgroup "File Size Formatting"
            [ bench "format small sizes" $ nf (map formatFileSize) testSizes
            , bench "format large list of sizes" $ nf (map formatFileSize) largeSizeList
            , bench "format with show (baseline)" $ nf (map show) largeSizeList
            ]
        ]

-- | Helper function to enable conditional compilation
when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()

-- | Run all FileHistogram benchmarks
runFileHistogramBenchmarks :: IO ()
runFileHistogramBenchmarks = do
    putStrLn "Running FileHistogram performance benchmarks..."
    
    putStrLn "\n=== Histogram Generation Methods ==="
    benchmarkHistogramGeneration
    
    putStrLn "\n=== Memory Usage Patterns ==="
    benchmarkMemoryUsage
    
    putStrLn "\n=== Data Characteristics Impact ==="
    benchmarkHistogramCharacteristics
    
    putStrLn "\n=== Processing Pipeline Performance ==="
    benchmarkHistogramPipeline
    
    putStrLn "\n=== Scan Options Impact ==="
    benchmarkScanOptionsImpact
    
    putStrLn "\n=== Early Termination Scenarios ==="
    benchmarkEarlyTermination
    
    putStrLn "\n=== File Size Formatting Performance ==="
    benchmarkFileSizeFormatting
    
    putStrLn "\nFileHistogram benchmarks completed!"

-- | Main function for standalone execution
main :: IO ()
main = runFileHistogramBenchmarks
