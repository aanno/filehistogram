{-# LANGUAGE OverloadedStrings #-}

module FileHistogram where

import Graphics.Vega.VegaLite
import System.Directory
import System.FilePath
import Control.Monad
import Data.Aeson
import qualified Data.Text as T
import System.IO

-- | Get all file sizes recursively from a directory
getFileSizes :: FilePath -> IO [Integer]
getFileSizes path = do
    exists <- doesDirectoryExist path
    if exists
        then do
            contents <- listDirectory path
            sizes <- forM contents $ \item -> do
                let fullPath = path </> item
                isFile <- doesFileExist fullPath
                isDir <- doesDirectoryExist fullPath
                if isFile
                    then do
                        size <- getFileSize fullPath
                        return [size]
                    else if isDir
                        then getFileSizes fullPath
                        else return []
            return $ concat sizes
        else do
            putStrLn $ "Directory does not exist: " ++ path
            return []

-- | Create histogram specification using hvega
createHistogram :: [Integer] -> VegaLite
createHistogram sizes = 
    let fileSizeData = dataFromJson $ toJSON $ map (\size -> object ["size" .= size]) sizes
        
        enc = encoding
            . position X [ PName "size"
                        , PmType Quantitative
                        , PTitle "File Size (bytes)"
                        , PScale [SType ScLog]  -- Log scale for better visualization of wide range
                        ]
            . position Y [ PAggregate Count
                        , PmType Quantitative
                        , PTitle "Number of Files"
                        ]
        
        spec = mark Bar [MBinned True]
            . enc
            . resolve
                . resolution (RScale [(ChX, Independent)])
    
    in toVegaLite 
        [ title "File Size Distribution" []
        , width 600
        , height 400
        , fileSizeData
        , transform 
            . binAs [Step 0.5] "size" "binned_size"  -- Auto binning with step size
        $ []
        , spec []
        ]

-- | Alternative histogram with linear scale and auto binning
createLinearHistogram :: [Integer] -> VegaLite
createLinearHistogram sizes = 
    let fileSizeData = dataFromJson $ toJSON $ map (\size -> object ["size" .= size]) sizes
        
        enc = encoding
            . position X [ PName "size"
                        , PmType Quantitative
                        , PTitle "File Size (bytes)"
                        , PBin [MaxBins 30]  -- Auto binning with max 30 bins
                        ]
            . position Y [ PAggregate Count
                        , PmType Quantitative
                        , PTitle "Number of Files"
                        ]
        
        spec = mark Bar []
            . enc
    
    in toVegaLite 
        [ title "File Size Distribution (Linear Scale)" []
        , width 600
        , height 400
        , fileSizeData
        , spec []
        ]

-- | Create histogram with human-readable file sizes
createReadableHistogram :: [Integer] -> VegaLite
createReadableHistogram sizes = 
    let fileSizeData = dataFromJson $ toJSON $ map (\size -> 
            let readable = formatFileSize size
            in object ["size" .= size, "readable_size" .= readable]
            ) sizes
        
        enc = encoding
            . position X [ PName "size"
                        , PmType Quantitative
                        , PTitle "File Size"
                        , PBin [MaxBins 25]
                        , PAxis [AxLabelExpr "datum.value < 1024 ? datum.value + ' B' : datum.value < 1048576 ? (datum.value/1024).toFixed(1) + ' KB' : datum.value < 1073741824 ? (datum.value/1048576).toFixed(1) + ' MB' : (datum.value/1073741824).toFixed(1) + ' GB'"]
                        ]
            . position Y [ PAggregate Count
                        , PmType Quantitative
                        , PTitle "Number of Files"
                        ]
            . color [MString "#4682B4"]
        
        spec = mark Bar [MTooltip TTEncoding]
            . enc
            . selection
                . select "brush" Interval [BindScales]
    
    in toVegaLite 
        [ title "File Size Distribution" []
        , width 700
        , height 450
        , fileSizeData
        , spec []
        ]

-- | Format file size in human-readable format
formatFileSize :: Integer -> String
formatFileSize bytes
    | bytes < 1024 = show bytes ++ " B"
    | bytes < 1024^2 = show (bytes `div` 1024) ++ " KB"
    | bytes < 1024^3 = show (bytes `div` (1024^2)) ++ " MB"
    | otherwise = show (bytes `div` (1024^3)) ++ " GB"

-- | Main function to generate and save histogram
generateHistogram :: FilePath -> FilePath -> IO ()
generateHistogram inputPath outputPath = do
    putStrLn $ "Scanning files in: " ++ inputPath
    sizes <- getFileSizes inputPath
    
    if null sizes
        then putStrLn "No files found or directory doesn't exist"
        else do
            putStrLn $ "Found " ++ show (length sizes) ++ " files"
            putStrLn $ "Size range: " ++ formatFileSize (minimum sizes) ++ " - " ++ formatFileSize (maximum sizes)
            
            let histogram = createReadableHistogram sizes
            
            -- Save to HTML file
            writeFile outputPath $ toHtml histogram
            putStrLn $ "Histogram saved to: " ++ outputPath

-- | Example usage
main :: IO ()
main = do
    putStrLn "Enter directory path to analyze:"
    path <- getLine
    let outputFile = "file_size_histogram.html"
    generateHistogram path outputFile
    
