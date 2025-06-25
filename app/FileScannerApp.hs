{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE OverloadedStrings #-}

module FileScannerApp (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.OsPath
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold
import Text.Printf (printf)

import FileScanner
import qualified CannonizedDirectoryCache as CDC
import qualified MountBoundary as MB

-- | Print file information
printFileInfo :: FileInfo -> IO ()
printFileInfo fileInfo = do
    let path = filePath fileInfo
        size = fileSize fileInfo
    pathText <- osPathsToTextSafe path
    T.putStrLn $ T.concat [pathText, " (", T.pack (show size), " bytes)"]

-- | Print cache statistics
printCacheStats :: ScanCaches -> IO ()
printCacheStats caches = do
    canonCacheSize <- CDC.cacheSize (canonCache caches)
    canonStats <- CDC.cacheStats (canonCache caches)
    
    putStrLn $ "\n=== Cache Statistics ==="
    putStrLn $ "Canonicalization cache entries: " ++ show canonCacheSize
    putStrLn $ "Cache hits: " ++ show (CDC.cacheHits canonStats)
    putStrLn $ "Cache misses: " ++ show (CDC.cacheMisses canonStats)
    putStrLn $ "Hit ratio: " ++ printf "%.2f%%" ((hitRatio canonStats * 100) :: Double)
  where
    hitRatio stats = 
        let hits = fromIntegral (CDC.cacheHits stats) :: Double
            total = fromIntegral (CDC.totalLookups stats) :: Double
        in if total > 0 then hits / total else 0.0

-- | Streaming scan with progress reporting
streamingScanWithProgress :: ScanOptions -> ScanCaches -> OsPath -> IO ()
streamingScanWithProgress opts caches startPath = do
    pathStr <- decodeFS startPath
    putStrLn $ "Starting streaming scan of: " ++ pathStr
    putStrLn "Files found:"
    putStrLn "============="
    
    -- Count and process files as a stream
    (fileCount, totalSize) <- S.fold countAndSizeFold $ 
        scanFilesStream opts caches startPath
    
    putStrLn $ "\n=== Scan Results ==="
    putStrLn $ "Total files: " ++ show fileCount
    putStrLn $ "Total size: " ++ formatBytes totalSize
    
    printCacheStats caches
  where
    -- Custom fold to count files and sum sizes while printing each file
    countAndSizeFold = Fold.foldlM' 
        (\(count, size) fileInfo -> do
            printFileInfo fileInfo
            return (count + 1, size + fileSize fileInfo))
        (return (0, 0))

-- | Batch scan (collect all results first)
{-# DEPRECATED batchScan "as scanFiles is deprecated" #-}
batchScan :: ScanOptions -> ScanCaches -> OsPath -> IO ()
batchScan opts caches startPath = do
    pathStr <- decodeFS startPath
    putStrLn $ "Starting batch scan of: " ++ pathStr
    
    files <- scanFiles opts caches startPath
    
    putStrLn $ "\n=== Files Found ==="
    mapM_ printFileInfo files
    
    let totalSize = sum $ map fileSize files
    putStrLn $ "\n=== Scan Results ==="
    putStrLn $ "Total files: " ++ show (length files)
    putStrLn $ "Total size: " ++ formatBytes totalSize
    
    printCacheStats caches

-- | Format bytes in human-readable format
formatBytes :: Integer -> String
formatBytes bytes
    | bytes < 1024 = show bytes ++ " B"
    | bytes < 1048576 = printf "%.1f KB" (fromIntegral bytes / 1024.0 :: Double)
    | bytes < 1073741824 = printf "%.1f MB" (fromIntegral bytes / 1048576.0 :: Double)
    | otherwise = printf "%.1f GB" (fromIntegral bytes / 1073741824.0 :: Double)

-- | Example scan options for different scenarios
fastScanOptions :: ScanOptions
fastScanOptions = defaultScanOptions
    { maxDepth = Just 5  -- Limit depth for faster scanning
    , concurrentWorkers = 32
    }

deepScanOptions :: ScanOptions
deepScanOptions = defaultScanOptions
    { followSymlinks = True
    , crossMountBoundaries = True  -- Cross filesystem boundaries
    , maxDepth = Nothing  -- No depth limit
    }

safeScanOptions :: ScanOptions
safeScanOptions = defaultScanOptions  -- Use safe defaults

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Usage: file-scanner <path> [--streaming|--batch] [--fast|--deep|--safe]"
            putStrLn ""
            putStrLn "Options:"
            putStrLn "  --streaming  Use streaming scan (memory efficient, shows progress)"
            putStrLn "  --batch      Use batch scan (collect all results first)"
            putStrLn "  --fast       Fast scan (limited depth, more concurrent)"
            putStrLn "  --deep       Deep scan (follow symlinks, cross mount boundaries)"
            putStrLn "  --safe       Safe scan (default options)"
        
        (pathArg:flags) -> do
            -- Initialize caches once
            caches <- newScanCaches
            
            -- Convert path to OsPath
            startPath <- encodeFS pathArg
            
            -- Parse options from command line flags
            let useStreaming = "--streaming" `elem` flags
                useBatch = "--batch" `elem` flags
                
                scanMode = if useStreaming then "streaming"
                          else if useBatch then "batch"
                          else "streaming"  -- Default to streaming
                
                opts = if "--fast" `elem` flags then fastScanOptions
                      else if "--deep" `elem` flags then deepScanOptions
                      else safeScanOptions
            
            putStrLn $ "Scan mode: " ++ scanMode
            putStrLn $ "Options: " ++ show opts
            putStrLn ""
            
            -- Perform the scan
            case scanMode of
                "streaming" -> streamingScanWithProgress opts caches startPath
                "batch" -> batchScan opts caches startPath
                _ -> streamingScanWithProgress opts caches startPath
