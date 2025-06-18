{-# LANGUAGE OverloadedStrings #-}

module FileHistogram 
    ( getFileSizes
    , createSimpleHistogram
    , formatFileSize
    , generateHistogram
    , fileHistogramCli
    ) where

import Graphics.Vega.VegaLite
import System.Directory
import System.FilePath
import Control.Monad
import Data.Aeson
import qualified Data.Text.Lazy as TL

-- | Create histogram with human-readable file sizes
createSimpleHistogram :: [Integer] -> VegaLite
createSimpleHistogram sizes =
    let fileSizeData = dataFromColumns []
            . dataColumn "size" (Numbers $ map fromInteger sizes)
            $ []

        enc = encoding
            . position X [ PName "size"
                        , PmType Quantitative
                        , PTitle "File Size (bytes)"
                        , PBin [MaxBins 20]
                        ]
            . position Y [ PAggregate Count
                        , PmType Quantitative
                        , PTitle "Number of Files"
                        ]

    in toVegaLite
        [ title "File Size Distribution" []
        , width 600
        , height 400
        , fileSizeData
        , mark Bar []
        , enc []
        ]

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
                        fileSize <- getFileSize fullPath
                        return [fileSize]
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
    let fileSizeData = dataFromJson (toJSON $ map (\fileSize -> object ["size" .= fileSize]) sizes) []
        
        enc = encoding
            . position X [ PName "size"
                        , PmType Quantitative
                        , PTitle "File Size (bytes)"
                        , PScale [SType ScLog]  -- Log scale for better visualization of wide range
                        , PBin [MaxBins 30]
                        ]
            . position Y [ PAggregate Count
                        , PmType Quantitative
                        , PTitle "Number of Files"
                        ]
    
    in toVegaLite 
        [ title "File Size Distribution" []
        , width 600
        , height 400
        , fileSizeData
        , mark Bar []
        , enc []
        ]

-- | Alternative histogram with linear scale and auto binning
createLinearHistogram :: [Integer] -> VegaLite
createLinearHistogram sizes = 
    let fileSizeData = dataFromJson (toJSON $ map (\fileSize -> object ["size" .= fileSize]) sizes) []
        
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
    
    in toVegaLite 
        [ title "File Size Distribution (Linear Scale)" []
        , width 600
        , height 400
        , fileSizeData
        , mark Bar []
        , enc []
        ]

-- | Create histogram with human-readable file sizes
createReadableHistogram :: [Integer] -> VegaLite
createReadableHistogram sizes = 
    let fileSizeData = dataFromJson (toJSON $ map (\fileSize -> 
            let readableSize = formatFileSize fileSize
            in object ["size" .= fileSize, "readable_size" .= readableSize]
            ) sizes) []
        
        enc = encoding
            . position X [ PName "size"
                        , PmType Quantitative
                        , PTitle "File Size (bytes)"
                        , PBin [MaxBins 25]
                        ]
            . position Y [ PAggregate Count
                        , PmType Quantitative
                        , PTitle "Number of Files"
                        ]
            . color [MString "#4682B4"]
    
    in toVegaLite 
        [ title "File Size Distribution" []
        , width 700
        , height 450
        , fileSizeData
        , mark Bar [MTooltip TTEncoding]
        , enc []
        ]

-- | Format file size in human-readable format
formatFileSize :: Integer -> String
formatFileSize bytes
    | bytes < 1024 = show bytes ++ " B"
    | bytes < (1024 :: Integer)^(2 :: Integer) = show (bytes `div` 1024) ++ " KB"
    | bytes < (1024 :: Integer)^(3 :: Integer) = show (bytes `div` ((1024 :: Integer)^(2 :: Integer))) ++ " MB"
    | otherwise = show (bytes `div` ((1024 :: Integer)^(3 :: Integer))) ++ " GB"

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
            
            let histogram = createSimpleHistogram sizes
            
            -- Save to HTML file
            writeFile outputPath $ TL.unpack $ toHtml histogram
            putStrLn $ "Histogram saved to: " ++ outputPath

-- | Example usage
fileHistogramCli :: IO ()
fileHistogramCli = do
    putStrLn "Enter directory path to analyze:"
    path <- getLine
    let outputFile = "file_size_histogram.html"
    generateHistogram path outputFile
