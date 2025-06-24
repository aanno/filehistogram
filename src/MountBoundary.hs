{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MountBoundary
  ( MountCache
  , newMountCache
  , refreshMountCache
  , isFileSystemBoundary
  , getAllMountPoints
  , getMountPointFor
  , clearMountCache
  , mountCount
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.List (find, sortOn, isPrefixOf)
import qualified Data.Set as Set
import Data.Set (Set)
import System.OsPath
import System.OsPath.Types (OsPath)
import System.Directory (doesPathExist)
import Control.Exception (try, SomeException, handle)

#ifdef mingw32_HOST_OS
import qualified System.Win32.File as Win32
import qualified System.Win32.Types as Win32
#else
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
-- System.MountPoints temporarily disabled due to API compatibility issues
-- import qualified System.MountPoints as MP
#endif

-- Cache for mount points - stores normalized mount paths and their metadata
data MountInfo = MountInfo
  { mountPath :: !OsPath
  , mountDevice :: !String  -- Device identifier for uniqueness check
  } deriving (Eq, Show)

instance Ord MountInfo where
    compare m1 m2 = compare (mountPath m1) (mountPath m2)

newtype MountCache = MountCache (IORef (Set MountInfo))

-- Create a new mount cache
newMountCache :: MonadIO m => m MountCache
newMountCache = liftIO $ MountCache <$> newIORef Set.empty

mountCount :: MonadIO m => MountCache -> m Int
mountCount (MountCache cacheRef) = liftIO $ do
    cache <- readIORef cacheRef
    return (Set.size cache)

-- Refresh the mount cache by reading current mount points
refreshMountCache :: MonadIO m => MountCache -> m ()
refreshMountCache (MountCache cacheRef) = liftIO $ do
    mountInfos <- getAllMountInfos
    writeIORef cacheRef (Set.fromList mountInfos)

-- Clear the mount cache
clearMountCache :: MonadIO m => MountCache -> m ()
clearMountCache (MountCache cacheRef) = liftIO $ writeIORef cacheRef Set.empty

-- Get all mount points for the current platform
getAllMountPoints :: MonadIO m => m [OsPath]
getAllMountPoints = liftIO $ do
    mountInfos <- getAllMountInfos
    return (map mountPath mountInfos)

-- Internal function to get mount info with device information
getAllMountInfos :: IO [MountInfo]
getAllMountInfos = handle handleError getMountInfosPlatform
  where
    handleError (_ :: SomeException) = do
        -- Fallback: return at least root directory
        rootPath <- encodeFS "/"
        return [MountInfo rootPath "unknown"]

-- Platform-specific mount point detection
getMountInfosPlatform :: IO [MountInfo]
getMountInfosPlatform = do
#ifdef mingw32_HOST_OS
    getWindowsMountInfos
#else
    getUnixMountInfos
#endif

#ifdef mingw32_HOST_OS
-- Windows implementation using GetLogicalDrives and volume APIs
getWindowsMountInfos :: IO [MountInfo]
getWindowsMountInfos = do
    drives <- Win32.getLogicalDrives
    let driveLetters = [c : ":\\" | c <- ['A'..'Z'], testBit drives (fromEnum c - fromEnum 'A')]
    validDrives <- filterM isDriveValid driveLetters
    mountInfos <- mapM createMountInfo validDrives
    return mountInfos
  where
    testBit :: Win32.DWORD -> Int -> Bool
    testBit w i = (w `div` (2^i)) `mod` 2 == 1
    
    isDriveValid :: FilePath -> IO Bool
    isDriveValid drive = handle (\(_ :: SomeException) -> return False) $ do
        driveType <- Win32.getDriveType (Just drive)
        return $ driveType /= Win32.dRIVE_UNKNOWN && driveType /= Win32.dRIVE_NO_ROOT_DIR
    
    createMountInfo :: FilePath -> IO MountInfo
    createMountInfo drive = do
        osPath <- encodeFS drive
        -- Use drive letter as device identifier on Windows
        return $ MountInfo osPath drive

    filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
    filterM p = foldr (\x -> liftM2 (\flg -> if flg then (x:) else id) (p x)) (return [])
    
    liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
    liftM2 f ma mb = do { a <- ma; b <- mb; return (f a b) }

#else
-- Unix-like systems (Linux, macOS, BSD) using fallback implementation
getUnixMountInfos :: IO [MountInfo]
getUnixMountInfos = do
    -- Skip System.MountPoints for now due to API uncertainty
    -- Use direct fallback implementation which is reliable
    getFallbackMountInfos

-- Fallback implementation for when System.MountPoints fails
getFallbackMountInfos :: IO [MountInfo]
getFallbackMountInfos = do
    result <- tryReadMountsFile
    case result of
        Just mountInfos -> return mountInfos
        Nothing -> do
            -- Ultimate fallback to root filesystem
            rootPath <- encodeFS "/"
            return [MountInfo rootPath "/"]
  where
    tryReadMountsFile :: IO (Maybe [MountInfo])
    tryReadMountsFile = do
        -- Try different mount info sources in order of preference
        linuxResult <- tryLinuxMounts
        case linuxResult of
            Just infos -> return (Just infos)
            Nothing -> tryBSDMounts
    
    tryLinuxMounts :: IO (Maybe [MountInfo])
    tryLinuxMounts = handle (\(_ :: SomeException) -> return Nothing) $ do
        -- Try /proc/self/mountinfo first (more detailed), then /proc/mounts
        mountInfoResult <- tryReadFile "/proc/self/mountinfo"
        case mountInfoResult of
            Just content -> Just <$> parseLinuxMountInfo content
            Nothing -> do
                mountsResult <- tryReadFile "/proc/mounts"
                case mountsResult of
                    Just content -> Just <$> parseLinuxMounts content
                    Nothing -> return Nothing
    
    tryBSDMounts :: IO (Maybe [MountInfo])
    tryBSDMounts = handle (\(_ :: SomeException) -> return Nothing) $ do
        -- On BSD systems, this could be extended to use getmntinfo() via FFI
        -- For now, return Nothing to use the ultimate fallback
        return Nothing
    
    tryReadFile :: FilePath -> IO (Maybe BS.ByteString)
    tryReadFile path = handle (\(_ :: SomeException) -> return Nothing) $ do
        content <- BS.readFile path
        return (Just content)
    
    parseLinuxMountInfo :: BS.ByteString -> IO [MountInfo]
    parseLinuxMountInfo content = do
        let lines' = BS8.lines content
        mountInfos <- mapM parseMountInfoLine lines'
        return $ catMaybes mountInfos
    
    parseLinuxMounts :: BS.ByteString -> IO [MountInfo]
    parseLinuxMounts content = do
        let lines' = BS8.lines content
        mountInfos <- mapM parseMountsLine lines'
        return $ catMaybes mountInfos
    
    parseMountInfoLine :: BS.ByteString -> IO (Maybe MountInfo)
    parseMountInfoLine line = handle (\(_ :: SomeException) -> return Nothing) $ do
        -- /proc/self/mountinfo format: ID PARENT-ID MAJOR:MINOR ROOT MOUNT-POINT OPTIONS...
        case BS8.words line of
            (_:_:majorMinor:_:mountPoint:_) -> do
                let mountPointStr = BS8.unpack mountPoint
                osPath <- encodeFS mountPointStr
                pathExists <- doesPathExist mountPointStr
                if pathExists
                    then return $ Just $ MountInfo osPath (BS8.unpack majorMinor)
                    else return Nothing
            _ -> return Nothing
    
    parseMountsLine :: BS.ByteString -> IO (Maybe MountInfo)
    parseMountsLine line = handle (\(_ :: SomeException) -> return Nothing) $ do
        -- /proc/mounts format: DEVICE MOUNT-POINT FSTYPE OPTIONS FREQ PASSNO
        case BS8.words line of
            (device:mountPoint:_) -> do
                let mountPointStr = BS8.unpack mountPoint
                osPath <- encodeFS mountPointStr
                pathExists <- doesPathExist mountPointStr
                if pathExists
                    then return $ Just $ MountInfo osPath (BS8.unpack device)
                    else return Nothing
            _ -> return Nothing
    
    catMaybes :: [Maybe a] -> [a]
    catMaybes = foldr (\x acc -> case x of Just y -> y:acc; Nothing -> acc) []
#endif

-- Check if there's a filesystem boundary between two paths
isFileSystemBoundary :: MonadIO m => MountCache -> OsPath -> OsPath -> m Bool
isFileSystemBoundary cache path1 path2 = liftIO $ do
    -- Ensure mount cache is populated
    (MountCache cacheRef) <- return cache
    mountInfos <- readIORef cacheRef
    when (Set.null mountInfos) $ refreshMountCache cache
    
    -- Get updated mount points
    mountInfos' <- readIORef cacheRef
    let sortedMounts = sortOn (negate . length . splitDirectories . mountPath) (Set.toList mountInfos')
    
    -- Find mount points for both paths
    mount1 <- findMountInfo sortedMounts (normalise path1)
    mount2 <- findMountInfo sortedMounts (normalise path2)
    
    -- Paths are on different filesystems if they have different mount points
    return $ case (mount1, mount2) of
        (Just m1, Just m2) -> mountDevice m1 /= mountDevice m2
        _ -> False  -- If we can't determine, assume same filesystem
  where
    when :: Monad m => Bool -> m () -> m ()
    when True action = action
    when False _ = return ()

-- Find the mount info that contains the given path
findMountInfo :: [MountInfo] -> OsPath -> IO (Maybe MountInfo)
findMountInfo mountInfos path = return $ find (isPathUnderMount path) mountInfos
  where
    isPathUnderMount :: OsPath -> MountInfo -> Bool
    isPathUnderMount checkPath mountInfo = 
        let mountComponents = splitDirectories (mountPath mountInfo)
            pathComponents = splitDirectories checkPath
        in length mountComponents <= length pathComponents && 
           take (length mountComponents) pathComponents == mountComponents

-- Get the specific mount point for a given path
getMountPointFor :: MonadIO m => MountCache -> OsPath -> m (Maybe OsPath)
getMountPointFor cache path = liftIO $ do
    (MountCache cacheRef) <- return cache
    mountInfos <- readIORef cacheRef
    when (Set.null mountInfos) $ refreshMountCache cache
    
    mountInfos' <- readIORef cacheRef
    let sortedMounts = sortOn (negate . length . splitDirectories . mountPath) (Set.toList mountInfos')
    result <- findMountInfo sortedMounts (normalise path)
    return (mountPath <$> result)
  where
    when :: Monad m => Bool -> m () -> m ()
    when True action = action
    when False _ = return ()
