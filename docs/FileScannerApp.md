# High-Performance File Scanner

A Haskell file scanner optimized for scanning millions of files with advanced caching and mount boundary detection.

## Features

- **OsPath Integration**: Uses `System.OsPath` for maximum performance
- **Smart Canonicalization Cache**: Avoids expensive system calls by caching path resolutions
- **Mount Boundary Detection**: Uses `System.MountPoints` library for robust filesystem boundary detection
- **Streaming API**: Memory-efficient streaming for large directory trees
- **Cross-Platform**: Works on Linux, macOS, BSD, and Windows

## Quick Start

### 1. Build with Stack

```bash
stack build
```

### 2. Run Examples

```bash
# Streaming scan (memory efficient, shows progress)
stack exec file-scanner-exe -- /path/to/scan --streaming

# Fast scan (limited depth)
stack exec file-scanner-exe -- /path/to/scan --streaming --fast

# Deep scan (follows symlinks, crosses mount boundaries)
stack exec file-scanner-exe -- /path/to/scan --streaming --deep

# Batch scan (collects all results first)
stack exec file-scanner-exe -- /path/to/scan --batch
```

## Performance Optimizations

### For 3+ Million Files

The scanner is optimized for very large directory trees:

1. **Canonicalization Cache**: Each directory component is cached, so `/usr/local/bin/prog1` and `/usr/local/bin/prog2` reuse cached results for `/usr`, `/usr/local`, and `/usr/local/bin`

2. **Mount Point Cache**: All mount points are loaded once and cached, avoiding repeated filesystem queries

3. **OsPath Performance**: Native path operations without String conversion overhead

4. **Streaming Processing**: Process files as they're discovered, not after collecting all results

### Cache Hit Ratios

For typical filesystem trees, expect:
- **Canonicalization cache**: 80-95% hit ratio
- **Mount boundary checks**: Near 100% hit ratio after warmup

## API Usage

```haskell
import FileScanner
import System.OsPath

main :: IO ()
main = do
    -- Initialize caches once
    caches <- newScanCaches
    
    -- Convert path to OsPath for performance
    startPath <- encodeFS "/path/to/scan"
    
    -- Stream files (memory efficient)
    S.fold Fold.drain $ 
        S.mapM processFile $ 
        scanFilesStream defaultScanOptions caches startPath
  where
    processFile fileInfo = do
        putStrLn $ "Found: " ++ T.unpack (filePath fileInfo)
        putStrLn $ "Size: " ++ show (fileSize fileInfo) ++ " bytes"
```

## Configuration

### package.yaml

The project uses direct `/proc` parsing on Unix systems for reliability:

```yaml
dependencies:
- bytestring         # Unix systems - for /proc parsing  
- Win32              # Windows only
- filepath >= 1.4 && < 1.5  # OsPath support
```

### Scan Options

```haskell
-- Safe defaults
defaultScanOptions :: ScanOptions

-- Performance tuning
fastScanOptions = defaultScanOptions
    { maxDepth = Just 10
    , concurrentWorkers = 32
    }

-- Comprehensive scanning  
deepScanOptions = defaultScanOptions
    { followSymlinks = True
    , crossMountBoundaries = True
    }
```

## Platform Support

- **Linux**: Uses `/proc/mounts` and `/proc/self/mountinfo` parsing
- **macOS/BSD**: Uses fallback implementation (extensible for `getmntinfo()`)
- **Windows**: Uses `GetLogicalDrives` API

## Compatibility

- **filepath 1.4**: Compatible with OsPath basic operations (requires FilePath conversions for some operations)
- **GHC 8.10+**: Recommended for best OsPath support
- **Mount Detection**: Uses reliable `/proc` parsing on Unix systems

## Dependencies

- `bytestring`: For `/proc` parsing on Unix systems
- `streamly`: High-performance streaming
- `filepath >= 1.4 && < 1.5`: OsPath support
- `directory >= 1.3.7`: Directory operations
- `unordered-containers`: For HashSet