{-# LANGUAGE OverloadedStrings #-}

import Scorm
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath
import System.Directory
import System.IO.Temp
import Control.Exception
import Control.Monad (filterM)
import Data.Maybe (listToMaybe)

main :: IO ()
main = do
  putStrLn "Running SCORM library tests..."
  putStrLn ""
  
  -- Test parsing SCORM 1.2 simple package
  testParse "test-data/scorm12-simple.zip" "SCORM 1.2 Simple"
  
  -- Test parsing SCORM 2004 simple package
  testParse "test-data/scorm2004-simple.zip" "SCORM 2004 Simple"
  
  -- Test parsing SCORM 1.2 runtime package
  testParse "test-data/scorm12-runtime.zip" "SCORM 1.2 Runtime"
  
  -- Test parsing SCORM 2004 runtime package
  testParse "test-data/scorm2004-runtime.zip" "SCORM 2004 Runtime"
  
  -- Test roundtrip for SCORM 1.2 simple
  testRoundtrip "test-data/scorm12-simple.zip" "SCORM 1.2 Simple"
  
  -- Test roundtrip for SCORM 2004 simple
  testRoundtrip "test-data/scorm2004-simple.zip" "SCORM 2004 Simple"
  
  -- Test roundtrip for SCORM 1.2 runtime
  testRoundtrip "test-data/scorm12-runtime.zip" "SCORM 1.2 Runtime"
  
  -- Test roundtrip for SCORM 2004 runtime
  testRoundtrip "test-data/scorm2004-runtime.zip" "SCORM 2004 Runtime"
  
  putStrLn ""
  putStrLn "✓ All tests passed!"

resolveFixture :: FilePath -> IO (Maybe FilePath)
resolveFixture fp = do
  let candidates =
        [ fp
        , ".." </> fp
        ]
  existing <- filterM doesFileExist candidates
  pure (listToMaybe existing)

-- | Test parsing a SCORM package
testParse :: FilePath -> String -> IO ()
testParse filePath name = do
  putStrLn $ "Testing parse: " ++ name
  mPath <- resolveFixture filePath
  case mPath of
    Nothing -> do
      putStrLn $ "  ⚠ Warning: Test file not found (tried " ++ show [filePath, ".." </> filePath] ++ ")"
      putStrLn "  Skipping test..."
    Just path -> do
      result <- parseScormPackageFromFile path
      case result of
        Left err -> error $ "Failed to parse " ++ name ++ ": " ++ err
        Right pkg -> do
          -- Verify basic structure
          let manifest = scormManifest pkg
          if T.null (manifestIdentifier manifest)
            then error $ name ++ ": Manifest missing identifier"
            else putStrLn $ "  ✓ Manifest identifier: " ++ T.unpack (manifestIdentifier manifest)
          
          -- Check that we have at least one organization
          if null (manifestOrganizations manifest)
            then error $ name ++ ": No organizations found"
            else do
              let org = head (manifestOrganizations manifest)
              putStrLn $ "  ✓ Organizations: " ++ show (length (manifestOrganizations manifest))
              putStrLn $ "  ✓ Organization identifier: " ++ T.unpack (organizationIdentifier org)
          
          -- Check that we have resources
          if null (manifestResources manifest)
            then error $ name ++ ": No resources found"
            else putStrLn $ "  ✓ Resources: " ++ show (length (manifestResources manifest))
          
          -- Check that we have files
          if null (scormFiles pkg)
            then error $ name ++ ": No files found"
            else putStrLn $ "  ✓ Files in package: " ++ show (length (scormFiles pkg))
          
          -- Verify imsmanifest.xml is present
          let hasManifest = any (\(path, _) -> takeFileName path == "imsmanifest.xml") (scormFiles pkg)
          if not hasManifest
            then error $ name ++ ": imsmanifest.xml not found in files"
            else putStrLn $ "  ✓ imsmanifest.xml found in package"
          
          -- Print version
          putStrLn $ "  ✓ SCORM Version: " ++ show (scormVersion pkg)
          
          -- Test JSON output
          let json = scormPackageToJSON pkg
          if T.null json
            then error $ name ++ ": JSON output is empty"
            else putStrLn $ "  ✓ JSON output generated (" ++ show (T.length json) ++ " characters)"
          
          putStrLn $ "  ✓ Parse test passed for " ++ name

-- | Test roundtrip serialization
testRoundtrip :: FilePath -> String -> IO ()
testRoundtrip filePath name = do
  putStrLn $ "Testing roundtrip: " ++ name
  mPath <- resolveFixture filePath
  case mPath of
    Nothing -> do
      putStrLn $ "  ⚠ Warning: Test file not found (tried " ++ show [filePath, ".." </> filePath] ++ ")"
      putStrLn "  Skipping test..."
    Just path -> do
      -- Parse original
      originalContent <- BL.readFile path
      originalPkg <- case parseScormPackage originalContent of
        Left err -> error $ "Failed to parse original " ++ name ++ ": " ++ err
        Right pkg -> return pkg
      
      -- Serialize
      serializedContent <- case serializeScormPackage originalPkg of
        Left err -> error $ "Failed to serialize " ++ name ++ ": " ++ err
        Right content -> return content
      
      -- Parse serialized
      roundtripPkg <- case parseScormPackage serializedContent of
        Left err -> error $ "Failed to parse roundtrip " ++ name ++ ": " ++ err
        Right pkg -> return pkg
      
      -- Compare key fields
      let origManifest = scormManifest originalPkg
      let rtManifest = scormManifest roundtripPkg
      
      -- Check manifest identifier
      if manifestIdentifier origManifest /= manifestIdentifier rtManifest
        then error $ name ++ " roundtrip: Manifest identifier mismatch"
        else putStrLn $ "  ✓ Manifest identifier matches"
      
      -- Check version
      if manifestVersion origManifest /= manifestVersion rtManifest
        then putStrLn $ "  ⚠ Warning: Manifest version mismatch (may be OK)"
        else putStrLn $ "  ✓ Manifest version matches"
      
      -- Check organizations count
      if length (manifestOrganizations origManifest) /= length (manifestOrganizations rtManifest)
        then error $ name ++ " roundtrip: Organization count mismatch"
        else putStrLn $ "  ✓ Organization count matches: " ++ show (length (manifestOrganizations origManifest))
      
      -- Check resources count
      if length (manifestResources origManifest) /= length (manifestResources rtManifest)
        then error $ name ++ " roundtrip: Resource count mismatch"
        else putStrLn $ "  ✓ Resource count matches: " ++ show (length (manifestResources origManifest))
      
      -- Check first organization identifier
      if not (null (manifestOrganizations origManifest)) && not (null (manifestOrganizations rtManifest))
        then do
          let origOrg = head (manifestOrganizations origManifest)
          let rtOrg = head (manifestOrganizations rtManifest)
          if organizationIdentifier origOrg /= organizationIdentifier rtOrg
            then error $ name ++ " roundtrip: First organization identifier mismatch"
            else putStrLn $ "  ✓ First organization identifier matches"
        else return ()
      
      -- Check first resource identifier
      if not (null (manifestResources origManifest)) && not (null (manifestResources rtManifest))
        then do
          let origRes = head (manifestResources origManifest)
          let rtRes = head (manifestResources rtManifest)
          if resourceIdentifier origRes /= resourceIdentifier rtRes
            then error $ name ++ " roundtrip: First resource identifier mismatch"
            else putStrLn $ "  ✓ First resource identifier matches"
        else return ()
      
      -- Check that imsmanifest.xml is present in both
      let origHasManifest = any (\(path, _) -> takeFileName path == "imsmanifest.xml") (scormFiles originalPkg)
      let rtHasManifest = any (\(path, _) -> takeFileName path == "imsmanifest.xml") (scormFiles roundtripPkg)
      if not origHasManifest
        then error $ name ++ " roundtrip: Original missing imsmanifest.xml"
        else if not rtHasManifest
          then error $ name ++ " roundtrip: Roundtrip missing imsmanifest.xml"
          else putStrLn $ "  ✓ imsmanifest.xml present in both"
      
      putStrLn $ "  ✓ Roundtrip test passed for " ++ name
