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
import System.Directory (canonicalizePath)
import System.FilePath (normalise)
import qualified Data.Text as T
import Data.Text (Text)
import System.Info (os)
import Data.List (isPrefixOf, sortOn)

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
    result <- try $ do
        canonPath <- canonicalizePath path
#ifdef mingw32_HOST_OS
        getWindowsDeviceInfo canonPath
#else
        getUnixDeviceInfo canonPath
#endif
    case result of
        Left (_ :: IOException) -> return Nothing
        Right info -> return (Just info)

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
    dev1 <- getDeviceInfo path1
    dev2 <- getDeviceInfo path2
    case (dev1, dev2) of
        (Just d1, Just d2) -> return $ deviceId d1 == deviceId d2
        _ -> return True  -- If we can't determine, assume same filesystem (safer)

-- | Check for mount boundaries (including bind mounts on Linux)
checkMountBoundary :: MonadIO m => FilePath -> FilePath -> m Bool
checkMountBoundary path1 path2
    | "linux" `T.isInfixOf` T.pack os = liftIO $ do
        result <- try $ readFile "/proc/self/mountinfo"
        case result of
            Left (_ :: IOException) -> return False
            Right content -> do
                let mountPoints = extractMountPoints content
                    mount1 = findMountPoint (normalise path1) mountPoints
                    mount2 = findMountPoint (normalise path2) mountPoints
                return $ mount1 /= mount2
    | otherwise = return False
  where
    extractMountPoints content = 
        [ words line !! 4 | line <- lines content, length (words line) >= 5 ]
    
    findMountPoint path mountPoints =
        let matches = filter (`isPrefixOf` path) (map normalise mountPoints)
            sorted = sortOn (negate . length) matches  -- Longest first
        in if null sorted then "/" else head sorted

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
shouldCrossFilesystemBoundary crossMountBoundaries path1 path2 =
    if crossMountBoundaries
        then return True  -- User explicitly wants to cross boundaries
        else do
            boundary <- isFileSystemBoundary path1 path2
            return $ not boundary  -- Cross only if NOT a boundary
