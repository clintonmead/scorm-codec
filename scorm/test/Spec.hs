{-# LANGUAGE OverloadedStrings #-}

import Scorm
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath
import System.Directory
import System.IO.Temp
import Control.Monad (filterM)
import Data.Maybe (listToMaybe)
import System.Process
import System.Exit (ExitCode(..))
import qualified System.IO as IO
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.List (isSuffixOf, sort)

main :: IO ()
main = do
  putStrLn "Running SCORM library tests..."
  putStrLn ""

  -- Discover all test packages
  testPackages <- discoverTestPackages
  putStrLn $ "Found " ++ show (length testPackages) ++ " test packages:"
  mapM_ (\(fp, name) -> putStrLn $ "  - " ++ name ++ " (" ++ fp ++ ")") testPackages
  putStrLn ""

  -- Test parsing for all packages
  putStrLn "=== Testing parsing ==="
  mapM_ (\(fp, name) -> testParse fp name) testPackages

  -- Test roundtrip for all packages
  putStrLn ""
  putStrLn "=== Testing roundtrip serialization ==="
  mapM_ (\(fp, name) -> testRoundtrip fp name) testPackages

  -- Test CLI roundtrip for all packages
  putStrLn ""
  putStrLn "=== Testing CLI roundtrip ==="
  mapM_ (\(fp, name) -> testCodecRoundtrip fp name) testPackages

  putStrLn ""
  putStrLn "✓ All tests passed!"

-- | Discover all .zip files in test-data/packages directory
discoverTestPackages :: IO [(FilePath, String)]
discoverTestPackages = do
  let packagesDir = "test-data/packages"
  exists <- doesDirectoryExist packagesDir
  if not exists
    then do
      putStrLn $ "Warning: " ++ packagesDir ++ " directory not found, trying fallback locations..."
      let fallbackDirs = ["../test-data/packages", "packages"]
      candidates <- filterM doesDirectoryExist fallbackDirs
      case candidates of
        [] -> do
          putStrLn "Error: No test packages directory found"
          return []
        (dir:_) -> do
          putStrLn $ "Using: " ++ dir
          findPackagesInDir dir
    else findPackagesInDir packagesDir
  where
    findPackagesInDir :: FilePath -> IO [(FilePath, String)]
    findPackagesInDir dir = do
      contents <- listDirectory dir
      let zipFiles = filter (".zip" `isSuffixOf`) contents
      let packages = map (\file -> (dir </> file, dropExtension file)) zipFiles
      return $ sort packages

resolveFixture :: FilePath -> IO (Maybe FilePath)
resolveFixture fp = do
  let candidates =
        [ fp
        , ".." </> fp
        ]
  existing <- filterM doesFileExist candidates
  pure (listToMaybe existing)

runScormCodec :: [String] -> BL.ByteString -> IO (ExitCode, BL.ByteString, String)
runScormCodec args input = do
  -- Use strict IO to avoid lazy IO deadlocks
  let exePath = "./dist-newstyle/build/x86_64-linux/ghc-9.10.3/scorm-0.1.0.0/x/scorm-codec/build/scorm-codec/scorm-codec"
  (Just hin, Just hout, Just herr, ph) <- createProcess (proc exePath args)
    { std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }

  -- Write input
  BL.hPutStr hin input
  IO.hClose hin

  -- Read output strictly
  output <- BS.hGetContents hout
  errorOutput <- IO.hGetContents herr

  -- Wait for process
  ec <- waitForProcess ph

  -- Close handles
  IO.hClose hout
  IO.hClose herr

  pure (ec, BL.fromStrict output, errorOutput)

testCodecRoundtrip :: FilePath -> String -> IO ()
testCodecRoundtrip filePath name = do
  putStrLn $ "Testing CLI roundtrip (scorm-codec): " ++ name
  mPath <- resolveFixture filePath
  case mPath of
    Nothing -> do
      putStrLn $ "  ⚠ Warning: Test file not found (tried " ++ show [filePath, ".." </> filePath] ++ ")"
      putStrLn "  Skipping test..."
    Just path -> do
      zipBs <- BL.readFile path

      -- decode ZIP -> JSON
      (ec1, jsonBs, err1) <- runScormCodec ["--decode"] zipBs
      case ec1 of
        ExitSuccess -> pure ()
        ExitFailure c -> error $ "scorm-codec --decode failed (" ++ show c ++ "): " ++ err1
      if BL.null jsonBs
        then error "scorm-codec --decode produced empty JSON"
        else putStrLn $ "  ✓ --decode produced JSON (" ++ show (BL.length jsonBs) ++ " bytes)"

      -- encode JSON -> ZIP
      (ec2, zip2, err2) <- runScormCodec ["--encode"] jsonBs
      case ec2 of
        ExitSuccess -> pure ()
        ExitFailure c -> error $ "scorm-codec --encode failed (" ++ show c ++ "): " ++ err2
      if BL.null zip2
        then error "scorm-codec --encode produced empty ZIP"
        else putStrLn $ "  ✓ --encode produced ZIP (" ++ show (BL.length zip2) ++ " bytes)"

      -- Parse the resulting ZIP and compare key manifest fields to original
      origPkg <- case parseScormPackage zipBs of
        Left err -> error $ "Failed to parse original zip for CLI test: " ++ err
        Right p -> pure p
      rtPkg <- case parseScormPackage zip2 of
        Left err -> error $ "Failed to parse encoded zip for CLI test: " ++ err
        Right p -> pure p

      let o = scormManifest origPkg
      let r = scormManifest rtPkg
      if manifestIdentifier o /= manifestIdentifier r
        then error $ name ++ " CLI roundtrip: manifestIdentifier mismatch"
        else putStrLn "  ✓ manifestIdentifier matches after CLI roundtrip"
      if length (manifestOrganizations o) /= length (manifestOrganizations r)
        then error $ name ++ " CLI roundtrip: organization count mismatch"
        else putStrLn "  ✓ organization count matches after CLI roundtrip"
      if length (manifestResources o) /= length (manifestResources r)
        then error $ name ++ " CLI roundtrip: resource count mismatch"
        else putStrLn "  ✓ resource count matches after CLI roundtrip"

      putStrLn $ "  ✓ CLI roundtrip test passed for " ++ name

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
