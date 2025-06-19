{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MountBoundary 
    ( DeviceInfo(..)
    , getDeviceInfo
    , isSameFilesystem
    , isFileSystemBoundary
    , shouldCrossFilesystemBoundary
    ) where

import Control.Exception (try, IOException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (when)
import System.Directory (canonicalizePath)
import System.FilePath (normalise)
import qualified Data.Text as T
import Data.Text (Text)
import System.Info (os)
import Data.List (isPrefixOf, sortOn)
import Logging

#ifdef mingw32_HOST_OS
import System.Win32.File (getVolumeNameForVolumeMountPoint)
#else
import System.Posix.Files (getFileStatus, deviceID)
#endif

-- | Device information for filesystem boundary detection
data DeviceInfo = DeviceInfo
    { deviceId :: !Text           -- Device ID (Unix) or Volume GUID (Windows)
    , mountPoint :: !FilePath     -- Mount point path
    } deriving (Show, Eq)

-- | Get device information for a file path
getDeviceInfo :: MonadIO m => FilePath -> m (Maybe DeviceInfo)
getDeviceInfo path = liftIO $ do
    logDebug $ "Getting device info for: " ++ path
    result <- try $ do
        canonPath <- canonicalizePath path
#ifdef mingw32_HOST_OS
        getWindowsDeviceInfo canonPath
#else
        getUnixDeviceInfo canonPath
#endif
    case result of
        Left (_ :: IOException) -> do
            logWarn $ "Cannot get device info for " ++ path
            return Nothing
        Right info -> do
            logDebug $ "Device info for " ++ path ++ ": " ++ show info
            return (Just info)

#ifdef mingw32_HOST_OS
-- | Windows-specific device info extraction
getWindowsDeviceInfo :: FilePath -> IO DeviceInfo
getWindowsDeviceInfo path = do
    let volume = take 3 path ++ "\\"
    volumeGuid <- getVolumeNameForVolumeMountPoint volume
    return DeviceInfo
        { deviceId = T.pack volumeGuid
        , mountPoint = volume
        }
#else
-- | Unix-specific device info extraction  
getUnixDeviceInfo :: FilePath -> IO DeviceInfo
getUnixDeviceInfo path = do
    stat <- getFileStatus path
    let devId = deviceID stat
    return DeviceInfo
        { deviceId = T.pack $ show devId
        , mountPoint = "/"  -- Simplified for now
        }
#endif

-- | Check if two paths are on the same filesystem
isSameFilesystem :: MonadIO m => FilePath -> FilePath -> m Bool
isSameFilesystem path1 path2 = do
    logDebug $ "Checking if same filesystem: " ++ path1 ++ " vs " ++ path2
    dev1 <- getDeviceInfo path1
    dev2 <- getDeviceInfo path2
    case (dev1, dev2) of
        (Just d1, Just d2) -> do
            let same = deviceId d1 == deviceId d2
            logDebug $ "Device comparison: " ++ show (deviceId d1) ++ " vs " ++ show (deviceId d2) ++ " = " ++ show same
            return same
        _ -> do
            logWarn $ "Cannot determine filesystem boundary between " ++ path1 ++ " and " ++ path2 ++ ", assuming same"
            return True  -- If we can't determine, assume same filesystem (safer)

-- | Check for mount boundaries (including bind mounts on Linux)
checkMountBoundary :: MonadIO m => FilePath -> FilePath -> m Bool
checkMountBoundary path1 path2
    | "linux" `T.isInfixOf` T.pack os = liftIO $ do
        logDebug $ "Checking mount boundary between " ++ path1 ++ " and " ++ path2
        result <- try $ readFile "/proc/self/mountinfo"
        case result of
            Left (_ :: IOException) -> do
                logWarn "Failed to read /proc/self/mountinfo"
                return False
            Right content -> do
                let mountPoints = extractMountPoints content
                    mount1 = findMountPoint (normalise path1) mountPoints
                    mount2 = findMountPoint (normalise path2) mountPoints
                logDebug $ "Mount points: " ++ mount1 ++ " vs " ++ mount2
                let boundary = mount1 /= mount2
                when boundary $ logInfo $ "Mount boundary detected: " ++ mount1 ++ " vs " ++ mount2
                return boundary
    | otherwise = return False
  where
    extractMountPoints content = 
        let points = [ words line !! 4 | line <- lines content, length (words line) >= 5 ]
        in take 50 points  -- Limit to prevent performance issues
    
    findMountPoint path mountPoints =
        let matches = filter (`isPrefixOf` path) (map normalise mountPoints)
            sorted = sortOn (negate . length) matches  -- Longest first
        in case sorted of
            [] -> "/"
            (first:_) -> first

-- | Comprehensive filesystem boundary detection
isFileSystemBoundary :: MonadIO m => FilePath -> FilePath -> m Bool
isFileSystemBoundary path1 path2 = do
    -- First check device boundary (most reliable)
    sameDev <- isSameFilesystem path1 path2
    if not sameDev
        then return True  -- Different devices = boundary
        else checkMountBoundary path1 path2  -- Check mount boundaries

-- | Main API: Should we cross this filesystem boundary?
shouldCrossFilesystemBoundary :: MonadIO m => Bool -> FilePath -> FilePath -> m Bool
shouldCrossFilesystemBoundary crossMountBoundaries path1 path2 = do
    logDebug $ "shouldCrossFilesystemBoundary " ++ show crossMountBoundaries ++ " " ++ path1 ++ " -> " ++ path2
    if crossMountBoundaries
        then do
            logDebug "User wants to cross all boundaries"
            return True  -- User explicitly wants to cross boundaries
        else do
            -- First check device boundary (most reliable)
            sameDev <- isSameFilesystem path1 path2
            if not sameDev
                then do
                    logInfo $ "Different filesystems detected, skipping: " ++ path2
                    return False  -- Different devices = don't cross
                else do
                    -- Check mount boundaries (bind mounts, etc.)
                    boundary <- checkMountBoundary path1 path2
                    if boundary
                        then do
                            logInfo $ "Mount boundary detected, skipping: " ++ path2
                            return False
                        else return True
