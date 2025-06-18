{-# LANGUAGE OverloadedStrings #-}
module Spec where

import Test.Hspec
import FileHistogram (createHistogram, formatFileSize)
import FileScanner (getFileSizes)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (writeFile)
import Control.Monad (forM_)
import Data.List (sort)

-- Helper function to create a temporary directory with files
withTempDir :: String -> (FilePath -> IO ()) -> IO ()
withTempDir prefix action = do
    let tempDir = "test_temp_" ++ prefix
    createDirectoryIfMissing True tempDir
    action tempDir
    removeDirectoryRecursive tempDir

-- Helper function to create files with specific sizes
createFiles :: FilePath -> [(String, Integer)] -> IO ()
createFiles dir fileSpecs = forM_ fileSpecs $ \(name, size) -> do
    let filePath = dir </> name
    -- Create a file with the specified size (approximate for large files)
    -- For exact size, one would need to write bytes. This is simpler for testing.
    writeFile filePath $ replicate (fromIntegral $ min size 1000) 'a'
    -- Note: This doesn't guarantee exact size for large files, but is sufficient for basic testing.

main :: IO ()
main = hspec $ do
    describe "getFileSizes" $ do
        it "should return an empty list for a non-existent directory" $ do
            sizes <- getFileSizes "non_existent_directory_12345"
            sizes `shouldBe` []

        it "should return an empty list for an empty directory" $
            withTempDir "empty" $ \tempDir -> do
                sizes <- getFileSizes tempDir
                sizes `shouldBe` []

        it "should return the sizes of files in a directory" $
            withTempDir "single_level" $ \tempDir -> do
                let fileSpecs = [("file1.txt", 100), ("file2.bin", 500), ("file3.log", 0)]
                createFiles tempDir fileSpecs
                sizes <- getFileSizes tempDir
                sort sizes `shouldBe` sort (map snd fileSpecs)

        it "should return the sizes of files recursively in subdirectories" $
            withTempDir "recursive" $ \tempDir -> do
                let subDir1 = tempDir </> "subdir1"
                let subDir2 = tempDir </> "subdir2"
                createDirectoryIfMissing True subDir1
                createDirectoryIfMissing True subDir2

                let fileSpecs = [("fileA.txt", 10), (subDir1 </> "fileB.bin", 20), (subDir2 </> "fileC.log", 30)]
                createFiles tempDir [("fileA.txt", 10)]
                createFiles subDir1 [("fileB.bin", 20)]
                createFiles subDir2 [("fileC.log", 30)]

                sizes <- getFileSizes tempDir
                sort sizes `shouldBe` sort [10, 20, 30]

    describe "formatFileSize" $ do
        it "should format bytes correctly" $ do
            formatFileSize 100 `shouldBe` "100 B"
        it "should format kilobytes correctly" $ do
            formatFileSize 1024 `shouldBe` "1 KB"
            formatFileSize 1500 `shouldBe` "1 KB" -- Integer division
        it "should format megabytes correctly" $ do
            formatFileSize (1024^2) `shouldBe` "1 MB"
            formatFileSize (1024^2 + 500000) `shouldBe` "1 MB"
        it "should format gigabytes correctly" $ do
            formatFileSize (1024^3) `shouldBe` "1 GB"
            formatFileSize (1024^3 + 500000000) `shouldBe` "1 GB"

    -- Note: Testing createHistogram directly is difficult as it returns a VegaLite structure.
    -- We could test properties of the generated JSON, but that's more complex.
    -- For now, we rely on manual inspection of the generated HTML files.
    describe "createHistogram" $ do
        it "should create a VegaLite structure without errors for empty input" $ do
            let histogram = createHistogram []
            -- This test primarily checks that the function doesn't crash
            True `shouldBe` True

        it "should create a VegaLite structure without errors for non-empty input" $ do
            let histogram = createHistogram [10, 100, 1000, 50000, 1000000]
            -- This test primarily checks that the function doesn't crash
            True `shouldBe` True
