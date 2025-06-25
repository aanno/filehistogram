{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileScanner 
    ( ScanOptions(..)
    , ScanCaches(..)
    , defaultScanOptions
    , newScanCaches
    , scanFilesStream
    , scanFiles
    , getFileSizes
    , getFileSizesStream
    , FileInfo(..)
    , osPathsToTextSafe
    , osPathToTextSafe
    ) where

import Control.Exception (try, IOException, SomeException (SomeException))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (when)
import Control.Concurrent.STM
import Control.Concurrent.Async (mapConcurrently)
import System.OsPath
import System.OsPath.Types (OsPath)
import System.Directory
import System.FilePath (isAbsolute)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as Stream
import Logging
import qualified MountBoundary as MB
import qualified CannonizedDirectoryCache as CDC
import CannonizedDirectoryCache (cacheStats)
import qualified VisitedPathTrie as VPT
import Data.Either (fromRight)

-- | Configuration for file scanning
data ScanOptions = ScanOptions
    { followSymlinks :: Bool
    , crossMountBoundaries :: Bool
    , maxDepth :: Maybe Int
    , concurrentWorkers :: Int  -- Control concurrency level
    } deriving (Show, Eq)

-- | Caches for optimized scanning
data ScanCaches = ScanCaches
    { mountCache :: MB.MountCache
    , canonCache :: CDC.CannonizedDirectoryCache
    }

-- | Default scan options (safe defaults)
defaultScanOptions :: ScanOptions
defaultScanOptions = ScanOptions
    { followSymlinks = False
    , crossMountBoundaries = False
    , maxDepth = Nothing
    , concurrentWorkers = 16  -- Higher for I/O bound work
    }

-- | Create new scan caches
newScanCaches :: MonadIO m => m ScanCaches
newScanCaches = liftIO $ do
    mountCache <- MB.newMountCache
    canonCache <- CDC.newCache
    -- Pre-populate mount cache
    MB.refreshMountCache mountCache
    return $ ScanCaches mountCache canonCache

logScanCachesInfo :: ScanCaches -> IO ()
logScanCachesInfo caches = do
    mountCount <- MB.mountCount (mountCache caches)
    canonSize <- CDC.cacheSize (canonCache caches)
    cacheStats <- CDC.cacheStats (canonCache caches)
    logInfo $ "Mount cache entries: " ++ show mountCount
    logInfo $ "Canonicalization cache size: " ++ show canonSize
    logInfo $ "Canonicalization cache stats: " ++ show cacheStats

-- | Information about a scanned file
data FileInfo = FileInfo
    { filePath :: [OsPath]
    , fileSize :: Integer
    } deriving (Show, Eq)

-- | Log visited trie statistics
logVisitedTrieStats :: TVar VPT.VisitedTrie -> IO ()
logVisitedTrieStats visitedTVar = do
    trie <- readTVarIO visitedTVar
    let (total, notVis, visiting, complete) = VPT.stats trie
    logDebug $ "VisitedTrie stats - Total nodes: " ++ show total 
            ++ ", Not visited: " ++ show notVis
            ++ ", Visiting: " ++ show visiting
            ++ ", Complete: " ++ show complete
            ++ ", Size: " ++ show (VPT.size trie)

osPathToTextSafe :: OsPath -> IO Text
osPathToTextSafe osPath = do
  result <- try (decodeFS osPath)
  case result of
    Left (_ :: SomeException) -> return (T.pack "<invalid path>")
    Right filePath -> return (T.pack filePath)

osPathsToTextSafe :: [OsPath] -> IO Text
osPathsToTextSafe osPaths = do
  textParts <- mapM convertOne osPaths
  return (T.concat textParts)
  where
    convertOne :: OsPath -> IO Text
    convertOne osPath = do
      result <- try (decodeFS osPath)
      case result of
        Left (_ :: SomeException) -> return (T.pack "<invalid>")
        Right filePath -> return (T.pack filePath)

-- | Scan files and return a stream of file information
{-# SCC scanFilesStream #-}
scanFilesStream :: MonadIO m => ScanOptions -> ScanCaches -> OsPath -> Stream m FileInfo
scanFilesStream opts caches path = 
    S.concatEffect $ do
        logInfo $ "Starting streaming file scan of: " ++ show path
        
        -- Canonicalize the path using cache
        canonResult <- liftIO $ try (CDC.getCanonicalized (canonCache caches) path)
        case canonResult of
            Left ex -> do
                logError $ "Cannot canonicalize path " ++ show path ++ ": " ++ show (ex :: IOException)
                return S.nil
            Right canonPath -> do
                canonPathStr <- liftIO $ decodeFS canonPath
                logDebug $ "Canonicalized path: " ++ canonPathStr
                
                -- Check if it's a directory (convert to FilePath for directory operations)
                isDir <- liftIO $ doesDirectoryExist canonPathStr
                if isDir
                    then do
                        -- Create thread-safe visited trie using STM
                        visitedTVar <- liftIO $ newTVarIO VPT.empty
                        
                        -- Mark the root as being visited
                        let canonComponents = splitPath canonPath
                        liftIO $ atomically $ VPT.markVisitedSTM canonComponents visitedTVar
                        
                        -- Start directory scan with concurrent processing
                        return $ scanDirectoryConcurrent opts caches visitedTVar canonPath 0
                    else do
                        -- It's a single file
                        maybeInfo <- getFileInfoSafe canonPath
                        case maybeInfo of
                            Just info -> return $ S.fromPure info
                            Nothing -> return S.nil

-- | Scan a directory concurrently, returning a stream of files
scanDirectoryConcurrent :: MonadIO m => ScanOptions -> ScanCaches -> TVar VPT.VisitedTrie -> OsPath -> Int -> Stream m FileInfo
scanDirectoryConcurrent opts caches visitedTVar dirPath depth =
    -- Check depth limit
    if maybe False (< depth) (maxDepth opts) || depth > 50
        then S.nil
        else
            -- List directory and process items concurrently
            S.concatEffect $ do
                dirPathStr <- liftIO $ decodeFS dirPath
                logDebug $ "Scanning directory (depth " ++ show depth ++ "): " ++ dirPathStr
                contentsResult <- liftIO $ try (listDirectory dirPathStr)
                case contentsResult of
                    Left ex -> do
                        logWarn $ "Cannot read directory " ++ dirPathStr ++ ": " ++ show (ex :: IOException)
                        -- Mark this directory as completely visited (failed)
                        let dirComponents = splitPath dirPath
                        liftIO $ atomically $ VPT.markCompleteAndCleanup dirComponents visitedTVar
                        return S.nil
                    Right contents -> do
                        -- Log cache info and visited trie stats periodically
                        liftIO $ do
                            logScanCachesInfo caches
                            logVisitedTrieStats visitedTVar
                        
                        when (length contents > 10000) $ 
                            logWarn $ "Large directory with " ++ show (length contents) ++ " items: " ++ dirPathStr
                        
                        -- Convert String paths to OsPath
                        osContents <- liftIO $ mapM encodeFS contents
                        
                        -- Process items
                        -- Note: Marking as complete happens after each subdirectory is processed
                        -- in processSubdirectory, not here at the parent level
                        return $ processItemsConcurrently opts caches visitedTVar dirPath depth osContents

-- | Process directory items with concurrency control
processItemsConcurrently :: MonadIO m => ScanOptions -> ScanCaches -> TVar VPT.VisitedTrie -> OsPath -> Int -> [OsPath] -> Stream m FileInfo
processItemsConcurrently opts caches visitedTVar dirPath depth items = do
    -- First, process all items to get file info
    let itemStreams = S.fromList items
            & S.concatMapM (processDirectoryItem opts caches visitedTVar dirPath depth)
    
    -- Stream all file infos
    itemStreams
    
    -- Note: We cannot easily mark directories as complete in a streaming context
    -- because we don't know when all items have been processed.
    -- Instead, we rely on the "Visiting" state to prevent re-scanning
    -- and accept the slightly higher memory usage.

-- | Process a single directory item
{-# SCC processDirectoryItem #-}
processDirectoryItem :: MonadIO m => ScanOptions -> ScanCaches -> TVar VPT.VisitedTrie -> OsPath -> Int -> OsPath -> m (Stream m FileInfo)
processDirectoryItem opts caches visitedTVar dirPath depth item = do
    let fullPath = dirPath </> item
    
    -- Convert to FilePath for file operations
    fullPathStr <- liftIO $ decodeFS fullPath
    
    -- Check if it's a symbolic link first
    isLink <- liftIO $ pathIsSymbolicLink fullPathStr
    
    if isLink && not (followSymlinks opts)
        then return S.nil
        else do
            -- Check file type
            isFile <- liftIO $ doesFileExist fullPathStr
            if isFile
                then do
                    -- It's a file - get its info
                    maybeInfo <- getFileInfoSafe fullPath
                    case maybeInfo of
                        Just info -> return $ S.fromPure info
                        Nothing -> return S.nil
                else do
                    -- Check if it's a directory
                    isDir <- liftIO $ doesDirectoryExist fullPathStr
                    if isDir
                        then processSubdirectory opts caches visitedTVar dirPath depth fullPath
                        else return S.nil

-- | Process a subdirectory
processSubdirectory :: MonadIO m => ScanOptions -> ScanCaches -> TVar VPT.VisitedTrie -> OsPath -> Int -> OsPath -> m (Stream m FileInfo)
processSubdirectory opts caches visitedTVar parentPath depth fullPath = do
    -- Canonicalize the directory path using cache
    canonResult <- liftIO $ try (CDC.getCanonicalized (canonCache caches) fullPath)
    case canonResult of
        Left ex -> do
            logWarn $ "Cannot canonicalize directory " ++ show fullPath ++ ": " ++ show (ex :: IOException)
            return S.nil
        Right canonPath -> do
            let canonComponents = splitPath canonPath
            
            -- Check filesystem boundaries using the mount cache
            shouldCross <- if crossMountBoundaries opts
                then return True
                else do
                    -- Check if crossing filesystem boundary
                    crossesBoundary <- liftIO $ MB.isFileSystemBoundary (mountCache caches) parentPath canonPath
                    return (not crossesBoundary)
            
            if not shouldCross
                then do
                    logInfo $ "Skipping filesystem boundary: " ++ show canonPath
                    return S.nil
                else do
                    -- Check if already visited using the trie (thread-safe)
                    alreadyVisited <- liftIO $ atomically $ VPT.checkAndMarkVisited canonComponents visitedTVar
                    
                    if alreadyVisited
                        then do
                            logDebug $ "Already visited (or has visited prefix), skipping: " ++ show canonPath
                            return S.nil
                        else
                            -- Recursively scan the subdirectory
                            return $ scanDirectoryConcurrent opts caches visitedTVar canonPath (depth + 1)

-- | Get file information safely
{-# SCC getFileInfoSafe #-}
getFileInfoSafe :: MonadIO m => OsPath -> m (Maybe FileInfo)
getFileInfoSafe filePath = do
    -- Convert to FilePath for file operations in filepath 1.4
    filePathStr <- liftIO $ {-# SCC decodeFS_filePath #-} decodeFS filePath
    result <- liftIO $ try ({-# SCC getFileSize #-} getFileSize filePathStr)
    case result of
        Left ex -> do
            logDebug $ "Cannot get size of file " ++ filePathStr ++ ": " ++ show (ex :: IOException)
            return Nothing
        Right size -> do
            -- filePathText <- liftIO $ T.pack <$> decodeFS filePath
            logDebug $ "File: " ++ filePathStr ++ " -> " ++ show size ++ " bytes"
            return $ Just $ FileInfo (splitPath filePath) size

-- | Get file sizes as a stream
getFileSizesStream :: MonadIO m => ScanOptions -> ScanCaches -> OsPath -> Stream m Integer
getFileSizesStream opts caches path = 
    S.mapM (return . fileSize) $ scanFilesStream opts caches path

-- | Scan files and collect all results (non-streaming version)
{-# DEPRECATED scanFiles "Use scanFilesStream instead" #-}
scanFiles :: MonadIO m => ScanOptions -> ScanCaches -> OsPath -> m [FileInfo]
scanFiles opts caches path = do
    files <- S.fold Fold.toList $ scanFilesStream opts caches path
    logInfo $ "Scan completed. Found " ++ show (length files) ++ " files"
    return files

-- | Get file sizes (non-streaming version)
{-# DEPRECATED getFileSizes "Use getFileSizesStream instead" #-}
getFileSizes :: MonadIO m => ScanOptions -> ScanCaches -> OsPath -> m [Integer]
getFileSizes opts caches path = do
    sizes <- S.fold Fold.toList $ getFileSizesStream opts caches path
    logInfo $ "Collected " ++ show (length sizes) ++ " file sizes"
    return sizes

-- | Convenience function to scan with String path (converts to OsPath)
{-# DEPRECATED scanFilesFromString "Use scanFilesStreamFromString instead" #-}
scanFilesFromString :: MonadIO m => ScanOptions -> ScanCaches -> FilePath -> m [FileInfo]
scanFilesFromString opts caches pathStr = do
    osPath <- liftIO $ encodeFS pathStr
    scanFiles opts caches osPath

-- | Convenience function to scan stream with String path
scanFilesStreamFromString :: MonadIO m => ScanOptions -> ScanCaches -> FilePath -> Stream m FileInfo
scanFilesStreamFromString opts caches pathStr = 
    S.concatEffect $ do
        osPath <- liftIO $ encodeFS pathStr
        return $ scanFilesStream opts caches osPath
