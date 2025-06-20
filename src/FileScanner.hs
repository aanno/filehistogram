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
import Control.Monad (when, foldM)
import System.Directory
import System.FilePath
import Data.Text (Text)
import qualified Data.Text as T
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
    { followSymlinks = False  -- This is the key - don't follow symlinks!
    , crossMountBoundaries = False
    , maxDepth = Nothing  -- No artificial limit needed if we don't follow symlinks
    }

-- | Information about a scanned file
data FileInfo = FileInfo
    { filePath :: Text
    , fileSize :: Integer
    } deriving (Show, Eq)

-- | Work item for tail-recursive processing
data WorkItem = WorkItem
    { workPath :: Text
    , workVisited :: Set.Set Text
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
                    then return $ scanWorkQueue opts [WorkItem (T.pack canonPath) Set.empty 0]
                    else do
                        logError $ "Path is not a directory: " ++ canonPath
                        return S.nil

-- | Process work queue tail-recursively using Streamly streams
scanWorkQueue :: MonadIO m => ScanOptions -> [WorkItem] -> Stream m FileInfo
scanWorkQueue _opts [] = S.nil
scanWorkQueue opts (work:restWork) = 
    S.concatEffect $ do
        let WorkItem currentPathText visited depth = work
            currentPath = T.unpack currentPathText
        
        -- Check depth limit if specified
        case maxDepth opts of
            Just maxD | depth > maxD -> do
                logDebug $ "Max depth " ++ show maxD ++ " reached, skipping: " ++ currentPath
                return $ scanWorkQueue opts restWork
            _ -> do
                -- Add emergency depth limit to prevent runaway recursion
                if depth > 50  -- Emergency depth limit to prevent runaway recursion
                    then do
                        logWarn $ "Emergency depth limit (50) reached, skipping: " ++ currentPath
                        return $ scanWorkQueue opts restWork
                    else do
                        logDebug $ "Scanning directory (depth " ++ show depth ++ "): " ++ currentPath
                        
                        -- Check for cycles using the visited set
                        if Set.member currentPathText visited
                            then do
                                logWarn $ "Cycle detected (already visited), skipping: " ++ currentPath
                                return $ scanWorkQueue opts restWork
                            else do
                                let newVisited = Set.insert currentPathText visited
                                processDirectory opts currentPath newVisited depth restWork

-- | Process a directory and return combined stream
processDirectory :: MonadIO m => ScanOptions -> FilePath -> Set.Set Text -> Int -> [WorkItem] -> m (Stream m FileInfo)
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
            
            -- Convert to Set for efficient operations
            let contentsSet = Set.fromList (map T.pack limitedContents)
            
            -- Process all items and collect results
            (fileInfos, newWorkItems) <- processAllItems opts dirPath visited depth contentsSet
            
            -- Combine file results with continued processing
            let fileStream = S.fromList fileInfos
                remainingWork = newWorkItems ++ restWork
                continuationStream = scanWorkQueue opts remainingWork
            
            return $ S.append fileStream continuationStream

-- | Process all items in a directory (non-streaming, but efficient)
processAllItems :: MonadIO m => ScanOptions -> FilePath -> Set.Set Text -> Int -> Set.Set Text -> m ([FileInfo], [WorkItem])
processAllItems opts dirPath visited depth itemsSet = 
    processItemsAcc opts dirPath visited depth (Set.toList itemsSet) [] []

-- | FIXED: Accumulator-based processing with proper visited set threading
processItemsAcc :: MonadIO m => ScanOptions -> FilePath -> Set.Set Text -> Int -> [Text] -> [FileInfo] -> [WorkItem] -> m ([FileInfo], [WorkItem])
processItemsAcc _opts _dirPath _visited _depth [] fileAcc workAcc = 
    return (reverse fileAcc, reverse workAcc)

processItemsAcc opts dirPath visited depth (item:remainingItems) fileAcc workAcc = do
    let itemStr = T.unpack item
        fullPath = dirPath </> itemStr
    
    -- Check if it's a symbolic link FIRST - this is the key fix!
    isLink <- liftIO $ pathIsSymbolicLink fullPath
    
    if isLink && not (followSymlinks opts)
        then do
            logDebug $ "Skipping symbolic link: " ++ fullPath
            processItemsAcc opts dirPath visited depth remainingItems fileAcc workAcc
        else do
            -- Check if it's a file or directory (but NOT if it's a symlink we don't want to follow)
            isFile <- liftIO $ if isLink then return False else doesFileExist fullPath
            isDir <- liftIO $ if isLink then return False else doesDirectoryExist fullPath
            
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
                        -- Canonicalize the directory path before checking boundaries and cycles
                        canonResult <- liftIO $ try (canonicalizePath fullPath)
                        case canonResult of
                            Left ex -> do
                                logWarn $ "Cannot canonicalize directory " ++ fullPath ++ ": " ++ show (ex :: IOException)
                                processItemsAcc opts dirPath visited depth remainingItems fileAcc workAcc
                            Right canonPath -> do
                                -- Check filesystem boundaries using canonical path
                                shouldCross <- shouldCrossFilesystemBoundary
                                               (crossMountBoundaries opts)
                                               canonPath
                                               dirPath
                                if shouldCross
                                    then do
                                        -- FIXED: Check if we've already visited this canonical path
                                        let canonPathText = T.pack canonPath
                                        if Set.member canonPathText visited
                                            then do
                                                logWarn $ "Cycle detected: already visited " ++ canonPath
                                                processItemsAcc opts dirPath visited depth remainingItems fileAcc workAcc
                                            else do
                                                -- Add to visited set and create work item
                                                let newVisited = Set.insert canonPathText visited
                                                    newWork = WorkItem canonPathText newVisited (depth + 1)
                                                -- FIXED: Thread the updated visited set through remaining processing
                                                processItemsAcc opts dirPath newVisited depth remainingItems fileAcc (newWork:workAcc)
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
            return $ Just $ FileInfo (T.pack filePath) size

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
