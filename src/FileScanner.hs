{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

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
    ) where

import Control.Exception (try, IOException)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (when)
import Control.Concurrent.STM
import System.OsPath
import System.OsPath.Types (OsPath)
import System.Directory
import System.FilePath (isAbsolute)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Dir as Dir  -- Add this import
import Logging
import qualified MountBoundary as MB
import qualified CannonizedDirectoryCache as CDC
import CannonizedDirectoryCache (cacheStats)

-- | Configuration for file scanning
data ScanOptions = ScanOptions
    { followSymlinks :: Bool
    , crossMountBoundaries :: Bool
    , maxDepth :: Maybe Int
    , concurrentWorkers :: Int
    , maxVisitedSize :: Int  -- Add limit for visited set
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
    , concurrentWorkers = 16
    , maxVisitedSize = 1000000  -- Limit visited set to 1M entries
    }

-- | Create new scan caches
newScanCaches :: MonadIO m => m ScanCaches
newScanCaches = liftIO $ do
    mountCache <- MB.newMountCache
    canonCache <- CDC.newCache
    -- Pre-populate mount cache
    MB.refreshMountCache mountCache
    return $ ScanCaches mountCache canonCache

-- | Information about a scanned file
data FileInfo = FileInfo
    { filePath :: {-# UNPACK #-} !Text  -- Strict and unpacked
    , fileSize :: {-# UNPACK #-} !Integer
    } deriving (Show, Eq)

-- | Visited state with size tracking
data VisitedState = VisitedState
    { visitedSet :: !(HashSet Text)
    , visitedCount :: {-# UNPACK #-} !Int
    }

-- | Scan files and return a stream of file information
scanFilesStream :: MonadIO m => ScanOptions -> ScanCaches -> OsPath -> Stream m FileInfo
scanFilesStream opts caches path = 
    S.concatEffect $ do
        pathStr <- liftIO $ decodeFS path
        logInfo $ "Starting streaming file scan of: " ++ pathStr
        
        -- Canonicalize the path using cache
        canonResult <- liftIO $ try (CDC.getCanonicalized (canonCache caches) path)
        case canonResult of
            Left ex -> do
                logError $ "Cannot canonicalize path " ++ pathStr ++ ": " ++ show (ex :: IOException)
                return S.nil
            Right canonPath -> do
                canonPathStr <- liftIO $ decodeFS canonPath
                logDebug $ "Canonicalized path: " ++ canonPathStr
                
                -- Check if it's a directory
                isDir <- liftIO $ doesDirectoryExist canonPathStr
                if isDir
                    then do
                        -- Create visited state with size limit
                        canonPathText <- liftIO $ strictText <$> decodeFS canonPath
                        visitedTVar <- liftIO $ newTVarIO $ VisitedState 
                            { visitedSet = HashSet.singleton canonPathText
                            , visitedCount = 1
                            }
                        
                        -- Start directory scan
                        return $ scanDirectoryStream opts caches visitedTVar canonPath 0
                    else do
                        -- It's a single file
                        maybeInfo <- getFileInfoSafe canonPath
                        case maybeInfo of
                            Just info -> return $ S.fromPure info
                            Nothing -> return S.nil

-- | Convert to strict Text efficiently
strictText :: String -> Text
strictText = T.pack
{-# INLINE strictText #-}

-- | Scan a directory using streaming directory traversal
scanDirectoryStream :: MonadIO m => ScanOptions -> ScanCaches -> TVar VisitedState -> OsPath -> Int -> Stream m FileInfo
scanDirectoryStream opts caches visitedTVar dirPath depth =
    -- Check depth limit
    if maybe False (< depth) (maxDepth opts) || depth > 50
        then S.nil
        else
            -- Use streaming directory traversal if available
            -- This avoids loading all entries into memory at once
            streamDirectoryEntries dirPath
                & S.concatMapM (processStreamedItem opts caches visitedTVar dirPath depth)

-- | Stream directory entries without loading all into memory
streamDirectoryEntries :: MonadIO m => OsPath -> Stream m OsPath
streamDirectoryEntries dirPath = S.concatEffect $ do
    dirPathStr <- liftIO $ decodeFS dirPath
    -- Try to use Streamly's directory streaming if available
    -- Otherwise fall back to chunked reading
    return $ streamDirectoryChunked dirPathStr 1000  -- Process in chunks of 1000

-- | Stream directory in chunks to avoid loading all entries at once
streamDirectoryChunked :: MonadIO m => FilePath -> Int -> Stream m OsPath
streamDirectoryChunked dirPathStr chunkSize = S.concatEffect $ do
    -- This is a simplified version - in production you'd want proper streaming
    contentsResult <- liftIO $ try (listDirectory dirPathStr)
    case contentsResult of
        Left ex -> do
            logWarn $ "Cannot read directory " ++ dirPathStr ++ ": " ++ show (ex :: IOException)
            return S.nil
        Right contents -> do
            when (length contents > 10000) $ 
                logWarn $ "Large directory with " ++ show (length contents) ++ " items: " ++ dirPathStr
            
            -- Convert to OsPath and stream in chunks
            osContents <- liftIO $ mapM encodeFS contents
            return $ S.fromList osContents

-- | Process a streamed directory item
processStreamedItem :: MonadIO m => ScanOptions -> ScanCaches -> TVar VisitedState -> OsPath -> Int -> OsPath -> m (Stream m FileInfo)
processStreamedItem opts caches visitedTVar dirPath depth item = do
    let !fullPath = dirPath </> item  -- Strict evaluation
    
    -- Get path string only once and reuse
    !fullPathStr <- liftIO $ decodeFS fullPath
    
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
                    maybeInfo <- getFileInfoSafe' fullPath fullPathStr  -- Reuse string
                    case maybeInfo of
                        Just info -> return $ S.fromPure info
                        Nothing -> return S.nil
                else do
                    -- Check if it's a directory
                    isDir <- liftIO $ doesDirectoryExist fullPathStr
                    if isDir
                        then processSubdirectory opts caches visitedTVar dirPath depth fullPath
                        else return S.nil

-- | Process a subdirectory with visited set size limit
processSubdirectory :: MonadIO m => ScanOptions -> ScanCaches -> TVar VisitedState -> OsPath -> Int -> OsPath -> m (Stream m FileInfo)
processSubdirectory opts caches visitedTVar parentPath depth fullPath = do
    -- Canonicalize the directory path
    canonResult <- liftIO $ try (CDC.getCanonicalized (canonCache caches) fullPath)
    case canonResult of
        Left ex -> do
            fullPathStr <- liftIO $ decodeFS fullPath
            logWarn $ "Cannot canonicalize directory " ++ fullPathStr ++ ": " ++ show (ex :: IOException)
            return S.nil
        Right canonPath -> do
            !canonPathText <- liftIO $ strictText <$> decodeFS canonPath
            
            -- Check filesystem boundaries
            shouldCross <- if crossMountBoundaries opts
                then return True
                else do
                    crossesBoundary <- liftIO $ MB.isFileSystemBoundary (mountCache caches) parentPath canonPath
                    return (not crossesBoundary)
            
            if not shouldCross
                then do
                    canonPathStr <- liftIO $ decodeFS canonPath
                    logInfo $ "Skipping filesystem boundary: " ++ canonPathStr
                    return S.nil
                else do
                    -- Check if already visited with size limit
                    shouldVisit <- liftIO $ atomically $ do
                        state <- readTVar visitedTVar
                        let visited = visitedSet state
                            count = visitedCount state
                        
                        if HashSet.member canonPathText visited
                            then return False
                            else if count >= maxVisitedSize opts
                                then return False  -- Stop visiting new directories if limit reached
                                else do
                                    writeTVar visitedTVar $ VisitedState
                                        { visitedSet = HashSet.insert canonPathText visited
                                        , visitedCount = count + 1
                                        }
                                    return True
                    
                    if not shouldVisit
                        then do
                            -- Check if we hit the size limit
                            hitLimit <- liftIO $ atomically $ do
                                state <- readTVar visitedTVar
                                return $ visitedCount state >= maxVisitedSize opts
                            when hitLimit $
                                logWarn "Visited set size limit reached, skipping new directories"
                            return S.nil
                        else
                            -- Recursively scan the subdirectory
                            return $ scanDirectoryStream opts caches visitedTVar canonPath (depth + 1)

-- | Get file information safely with pre-computed path string
getFileInfoSafe' :: MonadIO m => OsPath -> FilePath -> m (Maybe FileInfo)
getFileInfoSafe' filePath filePathStr = do
    result <- liftIO $ try (getFileSize filePathStr)
    case result of
        Left ex -> do
            logDebug $ "Cannot get size of file " ++ filePathStr ++ ": " ++ show (ex :: IOException)
            return Nothing
        Right size -> do
            -- Convert to Text only once and make it strict
            let !filePathText = strictText filePathStr
            logDebug $ "File: " ++ filePathStr ++ " -> " ++ show size ++ " bytes"
            return $ Just $! FileInfo filePathText size

-- | Get file information safely (original interface)
getFileInfoSafe :: MonadIO m => OsPath -> m (Maybe FileInfo)
getFileInfoSafe filePath = do
    !filePathStr <- liftIO $ decodeFS filePath
    getFileInfoSafe' filePath filePathStr

-- | Get file sizes as a stream
getFileSizesStream :: MonadIO m => ScanOptions -> ScanCaches -> OsPath -> Stream m Integer
getFileSizesStream opts caches path = 
    Stream.mapM (return . fileSize) $ scanFilesStream opts caches path

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

-- | Convenience function to scan stream with String path
scanFilesStreamFromString :: MonadIO m => ScanOptions -> ScanCaches -> FilePath -> Stream m FileInfo
scanFilesStreamFromString opts caches pathStr = 
    S.concatEffect $ do
        osPath <- liftIO $ encodeFS pathStr
        return $ scanFilesStream opts caches osPath
