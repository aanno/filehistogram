{-# LANGUAGE ScopedTypeVariables #-}

module CannonizedDirectoryCache
  ( CannonizedDirectoryCache
  , CacheStats(..)  -- Export the constructor and all fields
  , newCache
  , newCacheWithSize
  , getCanonicalized
  , clearCache
  , cacheSize
  , cacheStats
  ) where

import qualified Data.Cache.LRU as LRU
import Data.IORef
import System.OsPath hiding (isAbsolute)  -- Hide to avoid ambiguity
import System.OsPath.Types (OsPath)
import System.Directory (doesPathExist, getCurrentDirectory, pathIsSymbolicLink, getSymbolicLinkTarget)
import qualified System.FilePath as FP
import Control.Exception (try, SomeException)
import Control.Monad (foldM)

-- Cache maps original directory paths to their canonicalized equivalents
-- If key == value, the original path is not a symlink
type CacheMap = LRU.LRU OsPath OsPath

-- Default cache size
defaultCacheSize :: Integer
defaultCacheSize = 100000

data CacheStats = CacheStats
  { cacheHits :: !Int
  , cacheMisses :: !Int
  , totalLookups :: !Int
  , cacheEvictions :: !Int  -- Track evictions
  } deriving (Show, Eq)

data CannonizedDirectoryCache = CannonizedDirectoryCache
  { cacheMap :: !(IORef CacheMap)
  , cacheStatsRef :: !(IORef CacheStats)
  , maxCacheSize :: !Integer
  }

-- Create a new empty cache with default size
newCache :: IO CannonizedDirectoryCache
newCache = newCacheWithSize defaultCacheSize

-- Create a new empty cache with specified size
newCacheWithSize :: Integer -> IO CannonizedDirectoryCache
newCacheWithSize size = CannonizedDirectoryCache 
  <$> newIORef (LRU.newLRU (Just size))
  <*> newIORef (CacheStats 0 0 0 0)
  <*> pure size

-- Get the canonicalized version of a directory path, using cache when possible
getCanonicalized :: CannonizedDirectoryCache -> OsPath -> IO OsPath
getCanonicalized cache path = do
    let normalizedPath = normalise path
    lookupResult <- lookupInCache cache normalizedPath
    case lookupResult of
        Just canonPath -> do
            recordCacheHit cache
            return canonPath
        Nothing -> do
            recordCacheMiss cache
            canonPath <- canonicalizePathWithCache cache normalizedPath
            return canonPath

-- Look up a path in the cache
lookupInCache :: CannonizedDirectoryCache -> OsPath -> IO (Maybe OsPath)
lookupInCache cache path = do
    modifyIORef' (cacheStatsRef cache) $ \stats -> 
        stats { totalLookups = totalLookups stats + 1 }
    
    -- LRU lookup returns (newLRU, Maybe value)
    -- We need to update the cache with the new LRU state
    cacheMap' <- readIORef (cacheMap cache)
    let (newLRU, result) = LRU.lookup path cacheMap'
    writeIORef (cacheMap cache) newLRU
    return result

-- Record a cache hit
recordCacheHit :: CannonizedDirectoryCache -> IO ()
recordCacheHit cache = 
    modifyIORef' (cacheStatsRef cache) $ \stats -> 
        stats { cacheHits = cacheHits stats + 1 }

-- Record a cache miss  
recordCacheMiss :: CannonizedDirectoryCache -> IO ()
recordCacheMiss cache = 
    modifyIORef' (cacheStatsRef cache) $ \stats -> 
        stats { cacheMisses = cacheMisses stats + 1 }

-- Canonicalize a path by resolving symlinks component by component,
-- caching intermediate results
canonicalizePathWithCache :: CannonizedDirectoryCache -> OsPath -> IO OsPath
canonicalizePathWithCache cache path = do
    -- Convert to FilePath for directory operations, then back to OsPath
    pathStr <- decodeFS path
    let components = splitDirectories path
    
    -- Handle absolute vs relative paths
    (initialPath, remainingComponents) <- if FP.isAbsolute pathStr
        then return (head components, tail components)
        else do
            cwd <- getCurrentDirectory
            cwdOsPath <- encodeFS cwd
            return (cwdOsPath, components)
        
    -- Resolve each component incrementally
    result <- foldM (resolveNextComponent cache) initialPath remainingComponents
    let finalPath = normalise result
    
    -- Cache the final result
    insertIntoCache cache path finalPath
    return finalPath

-- Resolve the next component in the path
resolveNextComponent :: CannonizedDirectoryCache -> OsPath -> OsPath -> IO OsPath
resolveNextComponent cache basePath component = do
    let fullPath = basePath </> component
        normalizedPath = normalise fullPath
    
    -- Check if we already have this path cached
    cached <- lookupInCache cache normalizedPath
    case cached of
        Just result -> do
            recordCacheHit cache
            return result
        Nothing -> do
            recordCacheMiss cache
            resolved <- resolveSingleDirectory normalizedPath
            insertIntoCache cache normalizedPath resolved
            
            -- If the resolved path is different (i.e., it was a symlink),
            -- we might need to resolve it further
            if resolved /= normalizedPath
                then getCanonicalized cache resolved  -- Recursively resolve the target
                else return resolved

-- Resolve a single directory, following one level of symlink if necessary
resolveSingleDirectory :: OsPath -> IO OsPath
resolveSingleDirectory path = do
    -- Convert to FilePath for directory operations
    pathStr <- decodeFS path
    
    -- First check if the path exists
    pathExists <- doesPathExist pathStr
    if not pathExists
        then return path  -- Return as-is if path doesn't exist
        else do
            -- Check if it's a symbolic link
            result <- try (pathIsSymbolicLink pathStr) :: IO (Either SomeException Bool)
            case result of
                Left _ -> return path  -- If we can't check, assume it's not a symlink
                Right isSymlink -> 
                    if isSymlink
                        then do
                            -- Get the symlink target
                            targetResult <- try (getSymbolicLinkTarget pathStr) :: IO (Either SomeException FilePath)
                            case targetResult of
                                Left _ -> return path  -- If we can't read target, return original
                                Right target -> do
                                    if FP.isAbsolute target
                                        then do
                                            targetOsPath <- encodeFS target
                                            return (normalise targetOsPath)
                                        else do
                                            baseDir <- decodeFS (takeDirectory' path)
                                            let resolvedPath = baseDir ++ "/" ++ target  -- Simple path joining
                                            resolvedOsPath <- encodeFS resolvedPath
                                            return (normalise resolvedOsPath)
                        else return path  -- Not a symlink, return as-is
  where
    -- Helper function to get directory part of OsPath
    takeDirectory' :: OsPath -> OsPath
    takeDirectory' p = 
        let components = splitDirectories p
        in if length components <= 1 
           then p  -- Root or current directory
           else joinPath (init components)

-- Insert a mapping into the cache
insertIntoCache :: CannonizedDirectoryCache -> OsPath -> OsPath -> IO ()
insertIntoCache cache original canonical = do
    oldLRU <- readIORef (cacheMap cache)
    let (newLRU, mEvicted) = LRU.insertInforming original canonical oldLRU
    
    -- Track evictions
    case mEvicted of
        Nothing -> return ()
        Just _ -> modifyIORef' (cacheStatsRef cache) $ \stats ->
            stats { cacheEvictions = cacheEvictions stats + 1 }
    
    writeIORef (cacheMap cache) newLRU

-- Clear all cached entries
clearCache :: CannonizedDirectoryCache -> IO ()
clearCache cache = do
    let size = maxCacheSize cache
    writeIORef (cacheMap cache) (LRU.newLRU (Just size))
    writeIORef (cacheStatsRef cache) (CacheStats 0 0 0 0)

-- Get the number of cached entries (for debugging/monitoring)
cacheSize :: CannonizedDirectoryCache -> IO Int
cacheSize cache = do
    lru <- readIORef (cacheMap cache)
    return $ LRU.size lru

-- Get cache statistics (for debugging/monitoring)
cacheStats :: CannonizedDirectoryCache -> IO CacheStats
cacheStats cache = readIORef (cacheStatsRef cache)

-- Calculate cache hit ratio
cacheHitRatio :: CacheStats -> Double
cacheHitRatio stats = 
    if totalLookups stats == 0 
        then 0.0 
        else fromIntegral (cacheHits stats) / fromIntegral (totalLookups stats)
