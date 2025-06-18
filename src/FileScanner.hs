{-# LANGUAGE OverloadedStrings #-}

module FileScanner 
    ( ScanOptions(..)
    , defaultScanOptions
    , scanFiles
    , getFileSizes
    , FileInfo(..)
    , LogLevel(..)
    , logMessage
    ) where

import Control.Exception (try, IOException)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.Directory
import System.FilePath
import qualified Data.Set as Set

import Logging

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

-- | Scan files and return file information
scanFiles :: MonadIO m => ScanOptions -> FilePath -> m [FileInfo]
scanFiles opts path = do
    logInfo $ "Starting file scan of: " ++ path
    
    -- Canonicalize the path first
    canonResult <- liftIO $ try (canonicalizePath path)
    case canonResult of
        Left ex -> do
            logError $ "Cannot canonicalize path " ++ path ++ ": " ++ show (ex :: IOException)
            return []
        Right canonPath -> do
            logDebug $ "Canonicalized path: " ++ canonPath
            
            -- Check if it's a directory
            isDir <- liftIO $ doesDirectoryExist canonPath
            if isDir
                then do
                    files <- scanFilesRecursive opts canonPath Set.empty 0
                    logInfo $ "Scan completed. Found " ++ show (length files) ++ " files"
                    return files
                else do
                    logError $ "Path is not a directory: " ++ canonPath
                    return []

-- | Recursive file scanning with cycle detection
scanFilesRecursive :: MonadIO m => ScanOptions -> FilePath -> Set.Set FilePath -> Int -> m [FileInfo]
scanFilesRecursive opts path visited depth = do
    -- Check depth limit
    case maxDepth opts of
        Just maxD | depth > maxD -> do
            logDebug $ "Max depth reached, skipping: " ++ path
            return []
        _ -> do
            logDebug $ "Scanning directory: " ++ path
            
            -- Check for cycles (important when following symlinks)
            canonPath <- liftIO $ try (canonicalizePath path)
            case canonPath of
                Left ex -> do
                    logWarn $ "Cannot canonicalize " ++ path ++ ": " ++ show (ex :: IOException)
                    return []
                Right canon -> do
                    if Set.member canon visited
                        then do
                            logWarn $ "Cycle detected, skipping: " ++ canon
                            return []
                        else do
                            let newVisited = Set.insert canon visited
                            scanDirectory opts canon newVisited depth

-- | Scan a single directory
scanDirectory :: MonadIO m => ScanOptions -> FilePath -> Set.Set FilePath -> Int -> m [FileInfo]
scanDirectory opts path visited depth = do
    contentsResult <- liftIO $ try (listDirectory path)
    case contentsResult of
        Left ex -> do
            logWarn $ "Cannot read directory " ++ path ++ ": " ++ show (ex :: IOException)
            return []
        Right contents -> do
            files <- liftIO $ forM contents $ \item -> do
                let fullPath = path </> item
                processItem opts fullPath visited depth
            return $ concat files

-- | Process a single file or directory item
processItem :: MonadIO m => ScanOptions -> FilePath -> Set.Set FilePath -> Int -> m [FileInfo]
processItem opts fullPath visited depth = do
    -- Check if it's a symbolic link
    isLink <- liftIO $ pathIsSymbolicLink fullPath
    
    if isLink && not (followSymlinks opts)
        then do
            logDebug $ "Skipping symbolic link: " ++ fullPath
            return []
        else do
            -- Check if it's a file or directory
            isFile <- liftIO $ doesFileExist fullPath
            isDir <- liftIO $ doesDirectoryExist fullPath
            
            if isFile
                then getFileInfo fullPath
                else if isDir
                    then do
                        shouldCross <- if crossMountBoundaries opts
                            then return True
                            else checkSameFileSystem fullPath (takeDirectory fullPath)
                        
                        if shouldCross
                            then scanFilesRecursive opts fullPath visited (depth + 1)
                            else do
                                logWarn $ "Skipping potential mount boundary: " ++ fullPath
                                return []
                    else do
                        logDebug $ "Skipping special file: " ++ fullPath
                        return []

-- | Get file information safely
getFileInfo :: MonadIO m => FilePath -> m [FileInfo]
getFileInfo path = do
    result <- liftIO $ try (getFileSize path)
    case result of
        Left ex -> do
            logWarn $ "Cannot get size of file " ++ path ++ ": " ++ show (ex :: IOException)
            return []
        Right size -> do
            logDebug $ "File: " ++ path ++ " -> " ++ show size ++ " bytes"
            return [FileInfo path size]

-- | Simple cross-platform mount boundary check
checkSameFileSystem :: MonadIO m => FilePath -> FilePath -> m Bool
checkSameFileSystem path1 path2 = do
    -- On Windows, check if drive letters are the same
    -- On Unix-like, this is a simplified check
    let drive1 = take 1 $ dropWhile (== pathSeparator) path1
        drive2 = take 1 $ dropWhile (== pathSeparator) path2
    
    if drive1 /= drive2
        then do
            logDebug $ "Different drives detected: " ++ drive1 ++ " vs " ++ drive2
            return False
        else return True

-- | Convenience function that just returns file sizes (for backward compatibility)
getFileSizes :: MonadIO m => ScanOptions -> FilePath -> m [Integer]
getFileSizes opts path = do
    files <- scanFiles opts path
    return $ map fileSize files
