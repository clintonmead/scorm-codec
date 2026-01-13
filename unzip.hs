#!/usr/bin/env runhaskell

import Codec.Archive.Zip
import System.Directory
import System.FilePath
import Control.Monad
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  putStrLn "Unzipping test packages..."
  packagesDir <- doesDirectoryExist "test-data/packages"
  if not packagesDir
    then error "test-data/packages directory not found"
    else do
      createDirectoryIfMissing True "test-data/unzipped"
      files <- listDirectory "test-data/packages"
      let zipFiles = filter (".zip" `isSuffixOf`) files
      forM_ zipFiles $ \zipFile -> do
        let dirname = dropExtension zipFile
            zipPath = "test-data/packages" </> zipFile
            destDir = "test-data/unzipped" </> dirname
        putStrLn $ "Extracting " ++ zipFile ++ " to " ++ destDir
        createDirectoryIfMissing True destDir
        content <- BL.readFile zipPath
        let archive = toArchive content
        extractFilesFromArchive [OptDestination destDir] archive
        putStrLn $ "✓ Extracted " ++ zipFile
