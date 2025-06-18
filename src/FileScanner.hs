{-# LANGUAGE OverloadedStrings #-}

module FileScanner 
    ( ScanOptions(..)
    , defaultScanOptions
    , scanFilesStream
    , scanFiles
    , getFileSizes
    , FileInfo(..)
    ) where

import Control.Exception (try, IOException)
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.Directory
import System.FilePath
import qualified Data.Set as Set
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold
import Streamly.Data.Stream (Stream)
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
                    then return $ scanFilesStreamRecursive opts canonPath Set.empty 0
                    else do
                        logError $ "Path is not a directory: " ++ canonPath
                        return S.nil

-- | Recursive file scanning with streaming
scanFilesStreamRecursive :: MonadIO m => ScanOptions -> FilePath -> Set.Set FilePath -> Int -> Stream m FileInfo
scanFilesStreamRecursive opts path visited depth =
    S.concatEffect $ do
        -- Check depth limit
        case maxDepth opts of
            Just maxD | depth > maxD -> do
                logDebug $ "Max depth reached, skipping: " ++ path
                return S.nil
            _ -> do
                logDebug $ "Scanning directory: " ++ path
                
                -- Check for cycles (important when following symlinks)
                canonPath <- liftIO $ try (canonicalizePath path)
                case canonPath of
                    Left ex -> do
                        logWarn $ "Cannot canonicalize " ++ path ++ ": " ++ show (ex :: IOException)
                        return S.nil
                    Right canon -> do
                        if Set.member canon visited
                            then do
                                logWarn $ "Cycle detected, skipping: " ++ canon
                                return S.nil
                            else do
                                let newVisited = Set.insert canon visited
                                return $ scanDirectoryStream opts canon newVisited depth

-- | Scan a single directory with streaming
scanDirectoryStream :: MonadIO m => ScanOptions -> FilePath -> Set.Set FilePath -> Int -> Stream m FileInfo
scanDirectoryStream opts path visited depth =
    S.concatEffect $ do
        contentsResult <- liftIO $ try (listDirectory path)
        case contentsResult of
            Left ex -> do
                logWarn $ "Cannot read directory " ++ path ++ ": " ++ show (ex :: IOException)
                return S.nil
            Right contents -> do
                return $ S.concatMap (\item -> 
                    let fullPath = path </> item
                    in processItemStream opts fullPath visited depth) 
                    (S.fromList contents)

-- | Process a single file or directory item (streaming version)
processItemStream :: MonadIO m => ScanOptions -> FilePath -> Set.Set FilePath -> Int -> Stream m FileInfo
processItemStream opts fullPath visited depth =
    S.concatEffect $ do
        -- Check if it's a symbolic link
        isLink <- liftIO $ pathIsSymbolicLink fullPath
        
        if isLink && not (followSymlinks opts)
            then do
                logDebug $ "Skipping symbolic link: " ++ fullPath
                return S.nil
            else do
                -- Check if it's a file or directory
                isFile <- liftIO $ doesFileExist fullPath
                isDir <- liftIO $ doesDirectoryExist fullPath
                
                if isFile
                    then return $ getFileInfoStream fullPath
                    else if isDir
                        then do
                            shouldCross <- if crossMountBoundaries opts
                                then return True
                                else checkSameFileSystem fullPath (takeDirectory fullPath)
                            
                            if shouldCross
                                then return $ scanFilesStreamRecursive opts fullPath visited (depth + 1)
                                else do
                                    logWarn $ "Skipping potential mount boundary: " ++ fullPath
                                    return S.nil
                        else do
                            logDebug $ "Skipping special file: " ++ fullPath
                            return S.nil

-- | Get file information safely (streaming version)
getFileInfoStream :: MonadIO m => FilePath -> Stream m FileInfo
getFileInfoStream path =
    S.concatEffect $ do
        result <- liftIO $ try (getFileSize path)
        case result of
            Left ex -> do
                logWarn $ "Cannot get size of file " ++ path ++ ": " ++ show (ex :: IOException)
                return S.nil
            Right size -> do
                logDebug $ "File: " ++ path ++ " -> " ++ show size ++ " bytes"
                return $ S.fromPure $ FileInfo path size

-- | Simple cross-platform mount boundary check
checkSameFileSystem :: MonadIO m => FilePath -> FilePath -> m Bool
checkSameFileSystem path1 path2 = do
    let drive1 = take 1 $ dropWhile (== pathSeparator) path1
        drive2 = take 1 $ dropWhile (== pathSeparator) path2
    
    if drive1 /= drive2
        then do
            logDebug $ "Different drives detected: " ++ drive1 ++ " vs " ++ drive2
            return False
        else return True

-- | Scan files and collect all results (non-streaming version for backward compatibility)
scanFiles :: MonadIO m => ScanOptions -> FilePath -> m [FileInfo]
scanFiles opts path = do
    files <- S.fold Fold.toList $ scanFilesStream opts path
    logInfo $ "Scan completed. Found " ++ show (length files) ++ " files"
    return files

-- | Convenience function that just returns file sizes (for backward compatibility)
getFileSizes :: MonadIO m => ScanOptions -> FilePath -> m [Integer]
getFileSizes opts path = do
    files <- scanFiles opts path
    return $ map fileSize files
