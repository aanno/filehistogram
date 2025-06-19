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
import System.Directory
import System.FilePath
import qualified Data.Set as Set
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold
import Streamly.Data.Stream (Stream)
import Logging
import MountBoundary (shouldCrossFilesystemBoundary)

-- | Configuration for file scanning
data ScanOptions = ScanOptions
    { followSymlinks :: Bool
    , crossMountBoundaries :: Bool
    , maxDepth :: Maybe Int
    } deriving (Show, Eq)

-- | Default scan options (safe defaults)
defaultScanOptions :: ScanOptions
defaultScanOptions = ScanOptions
    { followSymlinks = False
    , crossMountBoundaries = False
    , maxDepth = Nothing
    }

-- | Information about a scanned file
data FileInfo = FileInfo
    { filePath :: FilePath
    , fileSize :: Integer
    } deriving (Show, Eq)

-- | Work item for tail-recursive processing
data WorkItem = WorkItem
    { workPath :: FilePath
    , workVisited :: Set.Set FilePath
    , workDepth :: Int
    } deriving (Show)

-- | Scan files and return a stream of file information (TAIL RECURSIVE)
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
                    then return $ scanWorkQueue opts [WorkItem canonPath Set.empty 0]
                    else do
                        logError $ "Path is not a directory: " ++ canonPath
                        return S.nil

-- | Process work queue tail-recursively using Streamly streams
scanWorkQueue :: MonadIO m => ScanOptions -> [WorkItem] -> Stream m FileInfo
scanWorkQueue _opts [] = S.nil
scanWorkQueue opts (work:restWork) = 
    S.concatEffect $ do
        let WorkItem currentPath visited depth = work
        
        -- Check depth limit first
        case maxDepth opts of
            Just maxD | depth > maxD -> do
                logDebug $ "Max depth " ++ show maxD ++ " reached, skipping: " ++ currentPath
                return $ scanWorkQueue opts restWork
            _ -> do
                logDebug $ "Scanning directory (depth " ++ show depth ++ "): " ++ currentPath
                
                -- Path should already be canonicalized, but check for cycles
                if Set.member currentPath visited
                    then do
                        logWarn $ "Cycle detected (already visited), skipping: " ++ currentPath
                        return $ scanWorkQueue opts restWork
                    else do
                        let newVisited = Set.insert currentPath visited
                        -- Add depth limit check for very deep structures
                        if depth > 50  -- Emergency depth limit to prevent runaway recursion
                            then do
                                logWarn $ "Emergency depth limit (50) reached, skipping: " ++ currentPath
                                return $ scanWorkQueue opts restWork
                            else processDirectory opts currentPath newVisited depth restWork

-- | Process a directory and return combined stream
processDirectory :: MonadIO m => ScanOptions -> FilePath -> Set.Set FilePath -> Int -> [WorkItem] -> m (Stream m FileInfo)
processDirectory opts dirPath visited depth restWork = do
    contentsResult <- liftIO $ try (listDirectory dirPath)
    case contentsResult of
        Left ex -> do
            logWarn $ "Cannot read directory " ++ dirPath ++ ": " ++ show (ex :: IOException)
            return $ scanWorkQueue opts restWork
        Right contents -> do
            -- Limit number of items per directory to prevent memory issues
            let limitedContents = if length contents > 10000 
                                 then take 10000 contents
                                 else contents
            
            -- Log warning if we had to limit
            when (length contents > 10000) $ 
                logWarn $ "Directory has " ++ show (length contents) ++ " items, limiting to first 10000: " ++ dirPath
            
            -- Process all items and collect results
            (fileInfos, newWorkItems) <- processAllItems opts dirPath visited depth limitedContents
            
            -- Combine file results with continued processing
            let fileStream = S.fromList fileInfos
                remainingWork = newWorkItems ++ restWork
                continuationStream = scanWorkQueue opts remainingWork
            
            return $ S.append fileStream continuationStream

-- | Process all items in a directory (non-streaming, but efficient)
processAllItems :: MonadIO m => ScanOptions -> FilePath -> Set.Set FilePath -> Int -> [String] -> m ([FileInfo], [WorkItem])
processAllItems opts dirPath visited depth items = 
    processItemsAcc opts dirPath visited depth items [] []

-- | Accumulator-based processing (tail recursive) - FIXED CYCLE DETECTION
processItemsAcc :: MonadIO m => ScanOptions -> FilePath -> Set.Set FilePath -> Int -> [String] -> [FileInfo] -> [WorkItem] -> m ([FileInfo], [WorkItem])
processItemsAcc _opts _dirPath _visited _depth [] fileAcc workAcc = 
    return (reverse fileAcc, reverse workAcc)

processItemsAcc opts dirPath visited depth (item:remainingItems) fileAcc workAcc = do
    let fullPath = dirPath </> item
    
    -- Check if it's a symbolic link
    isLink <- liftIO $ pathIsSymbolicLink fullPath
    
    if isLink && not (followSymlinks opts)
        then do
            logDebug $ "Skipping symbolic link: " ++ fullPath
            processItemsAcc opts dirPath visited depth remainingItems fileAcc workAcc
        else do
            -- Check if it's a file or directory
            isFile <- liftIO $ doesFileExist fullPath
            isDir <- liftIO $ doesDirectoryExist fullPath
            
            if isFile
                then do
                    -- Process file
                    maybeFileInfo <- getFileInfoSafe fullPath
                    case maybeFileInfo of
                        Just fileInfo -> 
                            processItemsAcc opts dirPath visited depth remainingItems (fileInfo:fileAcc) workAcc
                        Nothing -> 
                            processItemsAcc opts dirPath visited depth remainingItems fileAcc workAcc
                else if isDir
                    then do
                        -- FIXED: Canonicalize the directory path before checking boundaries and cycles
                        canonResult <- liftIO $ try (canonicalizePath fullPath)
                        case canonResult of
                            Left ex -> do
                                logWarn $ "Cannot canonicalize directory " ++ fullPath ++ ": " ++ show (ex :: IOException)
                                processItemsAcc opts dirPath visited depth remainingItems fileAcc workAcc
                            Right canonPath -> do
                                -- Check if we've already visited this canonical path
                                if Set.member canonPath visited
                                    then do
                                        logWarn $ "Cycle detected (canonical path already visited), skipping: " ++ canonPath
                                        processItemsAcc opts dirPath visited depth remainingItems fileAcc workAcc
                                    else do
                                        -- Check filesystem boundaries using canonical path
                                        shouldCross <- shouldCrossFilesystemBoundary
                                                       (crossMountBoundaries opts)
                                                       canonPath
                                                       dirPath
                                        if shouldCross
                                            then do
                                                -- Add directory to work queue with canonical path
                                                let newWork = WorkItem canonPath visited (depth + 1)
                                                processItemsAcc opts dirPath visited depth remainingItems fileAcc (newWork:workAcc)
                                            else do
                                                logInfo $ "Skipping filesystem boundary: " ++ canonPath
                                                processItemsAcc opts dirPath visited depth remainingItems fileAcc workAcc
                    else do
                        logDebug $ "Skipping special file: " ++ fullPath
                        processItemsAcc opts dirPath visited depth remainingItems fileAcc workAcc

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
            return $ Just $ FileInfo filePath size

-- | NEW: Streaming version that returns Stream of Integers (for FileHistogram)
getFileSizesStream :: MonadIO m => ScanOptions -> FilePath -> Stream m Integer
getFileSizesStream opts path = 
    S.mapM (return . fileSize) $ scanFilesStream opts path

-- | Scan files and collect all results (non-streaming version for backward compatibility)
scanFiles :: MonadIO m => ScanOptions -> FilePath -> m [FileInfo]
scanFiles opts path = do
    files <- S.fold Fold.toList $ scanFilesStream opts path
    logInfo $ "Scan completed. Found " ++ show (length files) ++ " files"
    return files

-- | UPDATED: Now uses streaming version internally
getFileSizes :: MonadIO m => ScanOptions -> FilePath -> m [Integer]
getFileSizes opts path = do
    sizes <- S.fold Fold.toList $ getFileSizesStream opts path
    logInfo $ "Collected " ++ show (length sizes) ++ " file sizes"
    return sizes
