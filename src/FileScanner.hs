{-# LANGUAGE OverloadedStrings #-}

module FileScanner 
    ( ScanOptions(..)
    , defaultScanOptions
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
import System.Directory
import System.FilePath
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold
import Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as Stream
import Logging
import MountBoundary (shouldCrossFilesystemBoundary)

-- | Configuration for file scanning
data ScanOptions = ScanOptions
    { followSymlinks :: Bool
    , crossMountBoundaries :: Bool
    , maxDepth :: Maybe Int
    , concurrentWorkers :: Int  -- Control concurrency level
    } deriving (Show, Eq)

-- | Default scan options (safe defaults)
defaultScanOptions :: ScanOptions
defaultScanOptions = ScanOptions
    { followSymlinks = False
    , crossMountBoundaries = False
    , maxDepth = Nothing
    , concurrentWorkers = 16  -- Higher for I/O bound work
    }

-- | Information about a scanned file
data FileInfo = FileInfo
    { filePath :: Text
    , fileSize :: Integer
    } deriving (Show, Eq)

-- | Scan files and return a stream of file information
scanFilesStream :: MonadIO m => ScanOptions -> FilePath -> Stream m FileInfo
scanFilesStream opts path = 
    S.concatEffect $ do
        logInfo $ "Starting streaming file scan of: " ++ path
        
        -- Canonicalize the path first
        canonResult <- liftIO $ try (canonicalizePath path)
        case canonResult of
            Left ex -> do
                logError $ "Cannot canonicalize path " ++ path ++ ": " ++ show (ex :: IOException)
                return S.nil
            Right canonPath -> do
                logDebug $ "Canonicalized path: " ++ canonPath
                
                -- Check if it's a directory
                isDir <- liftIO $ doesDirectoryExist canonPath
                if isDir
                    then do
                        -- Create thread-safe visited set using STM
                        visitedTVar <- liftIO $ newTVarIO (HashSet.singleton (T.pack canonPath))
                        
                        -- Start directory scan with concurrent processing
                        return $ scanDirectoryConcurrent opts visitedTVar canonPath 0
                    else do
                        -- It's a single file
                        maybeInfo <- getFileInfoSafe canonPath
                        case maybeInfo of
                            Just info -> return $ S.fromPure info
                            Nothing -> return S.nil

-- | Scan a directory concurrently, returning a stream of files
scanDirectoryConcurrent :: MonadIO m => ScanOptions -> TVar (HashSet Text) -> FilePath -> Int -> Stream m FileInfo
scanDirectoryConcurrent opts visitedTVar dirPath depth =
    -- Check depth limit
    if maybe False (< depth) (maxDepth opts) || depth > 50
        then S.nil
        else
            -- List directory and process items concurrently
            S.concatEffect $ do
                logDebug $ "Scanning directory (depth " ++ show depth ++ "): " ++ dirPath
                contentsResult <- liftIO $ try (listDirectory dirPath)
                case contentsResult of
                    Left ex -> do
                        logWarn $ "Cannot read directory " ++ dirPath ++ ": " ++ show (ex :: IOException)
                        return S.nil
                    Right contents -> do
                        when (length contents > 10000) $ 
                            logWarn $ "Large directory with " ++ show (length contents) ++ " items: " ++ dirPath
                        
                        -- Process items as a concurrent stream
                        return $ S.fromList contents
                            & Stream.parMapM (Stream.eager True) (concurrentWorkers opts) 
                                (processDirectoryItem opts visitedTVar dirPath depth)
                            & Stream.concatMap id

-- | Process a single directory item
processDirectoryItem :: MonadIO m => ScanOptions -> TVar (HashSet Text) -> FilePath -> Int -> String -> m (Stream m FileInfo)
processDirectoryItem opts visitedTVar dirPath depth item = do
    let fullPath = dirPath </> item
    
    -- Check if it's a symbolic link first
    isLink <- liftIO $ pathIsSymbolicLink fullPath
    
    if isLink && not (followSymlinks opts)
        then return S.nil
        else do
            -- Check file type
            isFile <- liftIO $ doesFileExist fullPath
            if isFile
                then do
                    -- It's a file - get its info
                    maybeInfo <- getFileInfoSafe fullPath
                    case maybeInfo of
                        Just info -> return $ S.fromPure info
                        Nothing -> return S.nil
                else do
                    -- Check if it's a directory
                    isDir <- liftIO $ doesDirectoryExist fullPath
                    if isDir
                        then processSubdirectory opts visitedTVar dirPath depth fullPath
                        else return S.nil

-- | Process a subdirectory
processSubdirectory :: MonadIO m => ScanOptions -> TVar (HashSet Text) -> FilePath -> Int -> FilePath -> m (Stream m FileInfo)
processSubdirectory opts visitedTVar parentPath depth fullPath = do
    -- Canonicalize the directory path
    canonResult <- liftIO $ try (canonicalizePath fullPath)
    case canonResult of
        Left ex -> do
            logWarn $ "Cannot canonicalize directory " ++ fullPath ++ ": " ++ show (ex :: IOException)
            return S.nil
        Right canonPath -> do
            let canonPathText = T.pack canonPath
            
            -- Check filesystem boundaries
            shouldCross <- shouldCrossFilesystemBoundary
                           (crossMountBoundaries opts)
                           canonPath
                           parentPath
            
            if not shouldCross
                then do
                    logInfo $ "Skipping filesystem boundary: " ++ canonPath
                    return S.nil
                else do
                    -- Check if already visited (thread-safe)
                    alreadyVisited <- liftIO $ atomically $ do
                        visited <- readTVar visitedTVar
                        if HashSet.member canonPathText visited
                            then return True
                            else do
                                writeTVar visitedTVar (HashSet.insert canonPathText visited)
                                return False
                    
                    if alreadyVisited
                        then do
                            logDebug $ "Already visited, skipping: " ++ canonPath
                            return S.nil
                        else
                            -- Recursively scan the subdirectory
                            return $ scanDirectoryConcurrent opts visitedTVar canonPath (depth + 1)

-- | Get file information safely
getFileInfoSafe :: MonadIO m => FilePath -> m (Maybe FileInfo)
getFileInfoSafe filePath = do
    result <- liftIO $ try (getFileSize filePath)
    case result of
        Left ex -> do
            logDebug $ "Cannot get size of file " ++ filePath ++ ": " ++ show (ex :: IOException)
            return Nothing
        Right size -> do
            logDebug $ "File: " ++ filePath ++ " -> " ++ show size ++ " bytes"
            return $ Just $ FileInfo (T.pack filePath) size

-- | Get file sizes as a stream
getFileSizesStream :: MonadIO m => ScanOptions -> FilePath -> Stream m Integer
getFileSizesStream opts path = 
    S.map fileSize $ scanFilesStream opts path

-- | Scan files and collect all results (non-streaming version)
scanFiles :: MonadIO m => ScanOptions -> FilePath -> m [FileInfo]
scanFiles opts path = do
    files <- S.fold Fold.toList $ scanFilesStream opts path
    logInfo $ "Scan completed. Found " ++ show (length files) ++ " files"
    return files

-- | Get file sizes (non-streaming version)
getFileSizes :: MonadIO m => ScanOptions -> FilePath -> m [Integer]
getFileSizes opts path = do
    sizes <- S.fold Fold.toList $ getFileSizesStream opts path
    logInfo $ "Collected " ++ show (length sizes) ++ " file sizes"
    return sizes
