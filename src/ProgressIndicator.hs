{-# LANGUAGE OverloadedStrings #-}

module ProgressIndicator 
    ( ProgressConfig(..)
    , defaultProgressConfig
    , progressConfigWithOverride
    , staticProgressConfig
    , ProgressState(..)
    , initProgress
    , updateProgress
    , finishProgress
    , withProgress
    , progressStream
    , fileScanProgress
    , countingProgress
    , fileSizeProgress
    , clearProgressLine
    , showSpinnerOperation
    ) where

import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (getCurrentTime, diffUTCTime, UTCTime)
import System.IO (hFlush, stdout, hIsTerminalDevice)
import Text.Printf (printf)
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Data.Fold as Fold

-- | Configuration for progress reporting
data ProgressConfig = ProgressConfig
    { enableProgress :: Bool          -- ^ Enable/disable progress reporting
    , updateInterval :: Int           -- ^ Update interval in milliseconds
    , showElapsed :: Bool            -- ^ Show elapsed time
    , showRate :: Bool               -- ^ Show processing rate (items/sec)
    , showPercentage :: Bool         -- ^ Show percentage (if total known)
    , showSpinnerFlag :: Bool        -- ^ Show spinner animation
    , progressPrefix :: String       -- ^ Prefix for progress messages
    , useStderr :: Bool              -- ^ Use stderr instead of stdout
    } deriving (Show, Eq)

-- | Default progress configuration (automatically detects if connected to terminal)
defaultProgressConfig :: IO ProgressConfig
defaultProgressConfig = do
    isInteractive <- hIsTerminalDevice stdout
    return $ ProgressConfig
        { enableProgress = isInteractive  -- Only enable if connected to interactive terminal
        , updateInterval = 100  -- 100ms updates
        , showElapsed = True
        , showRate = True
        , showPercentage = False  -- Usually don't know total
        , showSpinnerFlag = True
        , progressPrefix = "Processing"
        , useStderr = False
        }

-- | Progress state
data ProgressState = ProgressState
    { psConfig :: ProgressConfig
    , psStartTime :: UTCTime
    , psLastUpdate :: UTCTime
    , psCount :: Int
    , psTotal :: Maybe Int
    , psSpinnerState :: Int
    , psLastMessage :: String
    } deriving (Show)

-- | Static progress config for non-interactive use
staticProgressConfig :: ProgressConfig
staticProgressConfig = ProgressConfig
    { enableProgress = True  -- Can be overridden
    , updateInterval = 100
    , showElapsed = True
    , showRate = True
    , showPercentage = False
    , showSpinnerFlag = True
    , progressPrefix = "Processing"
    , useStderr = False
    }

-- | Create progress config with manual override
progressConfigWithOverride :: Bool -> IO ProgressConfig
progressConfigWithOverride forceEnable = do
    if forceEnable
        then return staticProgressConfig
        else do
            isInteractive <- hIsTerminalDevice stdout
            return $ staticProgressConfig { enableProgress = isInteractive }

-- | Spinner characters
spinnerChars :: [Char]
spinnerChars = ['|', '/', '-', '\\']

-- | Initialize progress state
initProgress :: MonadIO m => ProgressConfig -> Maybe Int -> m (MVar ProgressState)
initProgress config totalItems = liftIO $ do
    now <- getCurrentTime
    let state = ProgressState
            { psConfig = config
            , psStartTime = now
            , psLastUpdate = now
            , psCount = 0
            , psTotal = totalItems
            , psSpinnerState = 0
            , psLastMessage = ""
            }
    newMVar state

-- | Update progress with current count
updateProgress :: MonadIO m => MVar ProgressState -> Int -> m ()
updateProgress stateMVar newCount = liftIO $ do
    modifyMVar_ stateMVar $ \state -> do
        now <- getCurrentTime
        let config = psConfig state
            timeSinceLastUpdate = diffUTCTime now (psLastUpdate state)
            shouldUpdate = timeSinceLastUpdate * 1000 >= fromIntegral (updateInterval config)
        
        when (enableProgress config && shouldUpdate) $ do
            let newState = state 
                    { psCount = newCount
                    , psLastUpdate = now
                    , psSpinnerState = (psSpinnerState state + 1) `mod` length spinnerChars
                    }
            displayProgress newState
        
        return state { psCount = newCount }

-- | Display current progress
displayProgress :: ProgressState -> IO ()
displayProgress state = do
    let config = psConfig state
        count = psCount state
        elapsed = diffUTCTime (psLastUpdate state) (psStartTime state)
        elapsedSeconds = realToFrac elapsed :: Double
        rate = if elapsedSeconds > 0 then fromIntegral count / elapsedSeconds else 0
        
        -- Build progress message components
        spinnerChar = if showSpinnerFlag config 
                     then [spinnerChars !! psSpinnerState state, ' ']
                     else ""
        
        countStr = show count ++ " files"
        
        percentageStr = case psTotal state of
            Just total | showPercentage config && total > 0 -> 
                printf " (%.1f%%)" (fromIntegral count * 100.0 / fromIntegral total :: Double)
            _ -> ""
        
        elapsedStr = if showElapsed config
                    then printf " [%.1fs]" elapsedSeconds
                    else ""
        
        rateStr = if showRate config && count > 0
                 then printf " (%.0f files/s)" rate
                 else ""
        
        message = progressPrefix config ++ ": " ++ spinnerChar ++ countStr ++ percentageStr ++ elapsedStr ++ rateStr
    
    -- Clear previous line and show new progress
    putStr "\r\ESC[K"  -- Clear current line
    putStr message
    hFlush stdout

-- | Finish progress reporting with final summary
finishProgress :: MonadIO m => MVar ProgressState -> m ()
finishProgress stateMVar = liftIO $ do
    state <- readMVar stateMVar
    let config = psConfig state
    
    when (enableProgress config) $ do
        now <- getCurrentTime
        let elapsed = diffUTCTime now (psStartTime state)
            elapsedSeconds = realToFrac elapsed :: Double
            count = psCount state
            rate = if elapsedSeconds > 0 then fromIntegral count / elapsedSeconds else 0
            
            finalMessage = printf "\r\ESC[K%s: Completed %d files in %.1fs (%.0f files/s)\n" 
                          (progressPrefix config) count elapsedSeconds rate
        
        putStr finalMessage
        hFlush stdout

-- | Run an action with progress reporting
withProgress :: MonadIO m => ProgressConfig -> Maybe Int -> (MVar ProgressState -> m a) -> m a
withProgress config totalItems action = do
    when (enableProgress config) $ liftIO $ putStrLn $ progressPrefix config ++ "..."
    
    stateMVar <- initProgress config totalItems
    result <- action stateMVar
    finishProgress stateMVar
    return result

-- | Add progress reporting to a stream
progressStream :: MonadIO m => ProgressConfig -> S.Stream m a -> S.Stream m a
progressStream config stream = 
    if not (enableProgress config)
    then stream
    else S.mapM reportProgress (S.indexed stream)
  where
    reportProgress (index, item) = do
        when (index `mod` max 1 (updateInterval config `div` 10) == 0) $ liftIO $ do
            let count = index + 1 :: Int
                elapsed = 0.1 * fromIntegral index :: Double  -- Rough estimate
                rate = if elapsed > 0 then fromIntegral count / elapsed else 0 :: Double
                
                spinnerChar = spinnerChars !! ((index `div` 10) `mod` length spinnerChars)
                message = printf "\r\ESC[K%s: %c %d files (%.0f files/s)" 
                         (progressPrefix config) spinnerChar count rate
            
            putStr message
            hFlush stdout
        return item

-- | Progress for file scanning specifically (auto-detects terminal)
fileScanProgress :: MonadIO m => String -> S.Stream m a -> m (S.Stream m a)
fileScanProgress prefix stream = do
    config <- liftIO defaultProgressConfig
    return $ progressStream (config { progressPrefix = prefix }) stream

-- | Simple progress indicator for counting (auto-detects terminal)
countingProgress :: MonadIO m => String -> S.Stream m a -> m Int
countingProgress prefix stream = do
    config <- liftIO defaultProgressConfig
    liftIO $ when (enableProgress config) $ putStrLn $ prefix ++ "..."
    progressStreamWithProgress <- fileScanProgress prefix stream
    count <- S.fold Fold.length progressStreamWithProgress
    liftIO $ when (enableProgress config) $ putStrLn ""  -- New line after progress
    return count

-- | Progress for file size collection (auto-detects terminal)
fileSizeProgress :: MonadIO m => String -> S.Stream m Integer -> m [Integer]
fileSizeProgress prefix stream = do
    config <- liftIO defaultProgressConfig
    liftIO $ when (enableProgress config) $ putStrLn $ prefix ++ "..."
    progressStreamWithProgress <- fileScanProgress prefix stream
    sizes <- S.fold Fold.toList progressStreamWithProgress
    liftIO $ when (enableProgress config) $ putStrLn ""  -- New line after progress
    return sizes

-- | Utility function to clear progress line
clearProgressLine :: IO ()
clearProgressLine = do
    putStr "\r\ESC[K"
    hFlush stdout

-- | Show a simple spinner for indefinite operations
showSpinnerOperation :: MonadIO m => String -> IO a -> m a
showSpinnerOperation message action = liftIO $ do
    putStr $ message ++ " "
    hFlush stdout
    
    result <- action
    
    putStr "\r\ESC[K"  -- Clear spinner
    putStrLn $ message ++ " ✓"
    hFlush stdout
    
    return result
