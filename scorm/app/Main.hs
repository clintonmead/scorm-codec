{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Codec.Archive.Zip as Zip
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Scorm
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeExtension, takeFileName, takeBaseName, dropExtension, takeDirectory)
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import Data.Char (toLower)
import Data.List (sortBy)

-- JSON format for CLI roundtrips (includes file contents).
data CodecFile = CodecFile
  { path :: FilePath
  , contentBase64 :: T.Text
  } deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data CodecPackage = CodecPackage
  { manifest :: Manifest
  , version :: ScormVersion
  , files :: [CodecFile]
  } deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

toCodec :: ScormPackage -> CodecPackage
toCodec pkg =
  CodecPackage
    { manifest = scormManifest pkg
    , version = scormVersion pkg
    , files =
        [ CodecFile
            { path = p
            , contentBase64 = TE.decodeUtf8 (B64.encode (BL.toStrict bs))
            }
        | (p, bs) <- scormFiles pkg
        ]
    }

fromCodec :: CodecPackage -> Either String ScormPackage
fromCodec cp = do
  decodedFiles <- traverse decodeFile (files cp)
  pure
    ScormPackage
      { scormManifest = manifest cp
      , scormVersion = version cp
      , scormFiles = decodedFiles
      }
  where
    decodeFile :: CodecFile -> Either String (FilePath, BL.ByteString)
    decodeFile f = do
      bs <-
        firstLeft
          (\e -> "base64 decode failed for " <> path f <> ": " <> e)
          (B64.decode (TE.encodeUtf8 (contentBase64 f)))
      pure (path f, BL.fromStrict bs)

firstLeft :: (e1 -> e2) -> Either e1 a -> Either e2 a
firstLeft f (Left e) = Left (f e)
firstLeft _ (Right a) = Right a

-- | Check if a file is an image based on extension
isImageFile :: FilePath -> Bool
isImageFile path =
  let ext = map toLower (takeExtension path)
  in ext `elem` [".png", ".jpg", ".jpeg", ".gif", ".bmp", ".svg", ".webp", ".ico"]

-- | Replace image references in text content, avoiding double replacements
-- Only replaces when the pattern appears in a valid image reference context
replaceImageRefsInText :: String -> [(FilePath, a)] -> T.Text -> T.Text
replaceImageRefsInText scormName imageFiles content =
  let -- For each image, we want to replace various forms of the path
      -- We process them in order of specificity (longest first)
      replacements = 
        -- Full paths with ../ prefix
        [(T.pack ("../" ++ imgPath), T.pack $ "http://localhost:8081/" ++ scormName ++ "/" ++ imgPath) 
          | (imgPath, _) <- imageFiles] ++
        -- Full paths with ./ prefix
        [(T.pack ("./" ++ imgPath), T.pack $ "http://localhost:8081/" ++ scormName ++ "/" ++ imgPath) 
          | (imgPath, _) <- imageFiles] ++
        -- Full paths without prefix
        [(T.pack imgPath, T.pack $ "http://localhost:8081/" ++ scormName ++ "/" ++ imgPath) 
          | (imgPath, _) <- imageFiles] ++
        -- Just the filename (for relative references)
        [(T.pack (takeFileName imgPath), T.pack $ "http://localhost:8081/" ++ scormName ++ "/" ++ imgPath) 
          | (imgPath, _) <- imageFiles]
      
      -- Sort by pattern length (longest first) to match specific paths before generic filenames
      sorted = sortBy (\(a, _) (b, _) -> compare (T.length b) (T.length a)) replacements
      
      -- Check if the context suggests this is an image reference
      -- (preceded by quotes, parentheses, or equals sign)
      isImageContext :: T.Text -> Bool
      isImageContext before =
        let lastFew = T.takeEnd 20 before
            markers = [T.pack "\"", T.pack "'", T.pack "=", T.pack "(", 
                      T.pack "=\"", T.pack "='", T.pack "url(", T.pack "src="]
        in any (\marker -> marker `T.isSuffixOf` lastFew) markers
      
      -- Apply replacements but skip if we're inside an already-replaced URL or not in image context
      applyReplace txt (pattern, replacement) =
        let parts = T.splitOn pattern txt
            -- Rejoin parts, but check context before each join
            rebuild [] = T.empty
            rebuild [x] = x
            rebuild (x:y:rest) =
              -- Check if we're joining inside a URL we already replaced
              let inReplacedUrl = T.isSuffixOf "http://localhost:8081/" x ||
                                   T.isSuffixOf ("localhost:8081/" <> T.pack scormName <> "/") x ||
                                   ("localhost:8081/" <> T.pack scormName <> "/") `T.isInfixOf` (x <> pattern)
                  -- Check if this looks like an image reference context
                  validContext = isImageContext x
              in if inReplacedUrl
                 then x <> pattern <> rebuild (y:rest)  -- Don't replace - already replaced
                 else if validContext
                      then x <> replacement <> rebuild (y:rest)  -- Do replace - valid context
                      else x <> pattern <> rebuild (y:rest)  -- Don't replace - not in valid context
        in rebuild parts
  in foldl applyReplace content sorted

-- | Process a SCORM package file: if it's text-based, replace image refs
processFileContent :: String -> [(FilePath, a)] -> FilePath -> BL.ByteString -> BL.ByteString
processFileContent scormName imageFiles path content =
  let ext = map toLower (takeExtension path)
      isTextFile = ext `elem` [".html", ".htm", ".xml", ".js", ".css", ".xsd"]
  in if isTextFile
     then 
       -- Try to decode as UTF-8, but if it fails, leave content unchanged
       case TE.decodeUtf8' (BL.toStrict content) of
         Left _ -> content  -- Not valid UTF-8, leave as is
         Right textContent ->
           let updatedContent = replaceImageRefsInText scormName imageFiles textContent
           in BL.fromStrict (TE.encodeUtf8 updatedContent)
     else content

-- | Strip images from a SCORM package
stripScormImages :: FilePath -> FilePath -> IO ()
stripScormImages scormPath staticBaseDir = do
  -- Read the SCORM package
  zipContent <- BL.readFile scormPath
  case parseScormPackage zipContent of
    Left err -> do
      hPutStrLn stderr ("Failed to parse SCORM package: " ++ err)
      exitFailure
    Right pkg -> do
      -- Determine the output directory name (basename of the scorm file)
      let scormBaseName = dropExtension (takeFileName scormPath)
      -- Append scormBaseName to the provided static base directory
      let staticDir = staticBaseDir </> scormBaseName
      
      -- Create the static directory
      createDirectoryIfMissing True staticDir
      
      -- Separate image files from other files
      let allFiles = scormFiles pkg
          imageFiles = [(p, bs) | (p, bs) <- allFiles, isImageFile p]
          nonImageFiles = [(p, bs) | (p, bs) <- allFiles, not (isImageFile p)]
      
      hPutStrLn stderr $ "Found " ++ show (length imageFiles) ++ " image files"
      
      -- Copy images to static directory
      mapM_ (\(imgPath, imgContent) -> do
        let targetPath = staticDir </> imgPath
        let targetDir = takeDirectory targetPath
        createDirectoryIfMissing True targetDir
        BL.writeFile targetPath imgContent
        hPutStrLn stderr $ "  Copied: " ++ imgPath
        ) imageFiles
      
      -- Process non-image files to update image references
      let processedFiles = map (\(p, bs) -> 
            (p, processFileContent scormBaseName imageFiles p bs)) nonImageFiles
      
      -- Create new SCORM package with updated references
      let updatedPkg = pkg { scormFiles = processedFiles }
      
      -- Serialize and write the new SCORM package
      let outputPath = dropExtension scormPath ++ "_stripped.zip"
      case serializeScormPackage updatedPkg of
        Left err -> do
          hPutStrLn stderr ("Failed to serialize SCORM package: " ++ err)
          exitFailure
        Right outZip -> do
          BL.writeFile outputPath outZip
          hPutStrLn stderr $ "Created stripped SCORM package: " ++ outputPath
          hPutStrLn stderr $ "Images extracted to: " ++ staticDir

usage :: IO ()
usage = do
  hPutStrLn stderr "Usage:"
  hPutStrLn stderr "  scorm-codec --decode                        # ZIP on stdin -> JSON on stdout"
  hPutStrLn stderr "  scorm-codec --encode                        # JSON on stdin -> ZIP on stdout"
  hPutStrLn stderr "  scorm-codec --strip <file.zip> <static-dir> # Extract images and rewrite refs"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--decode"] -> do
      zipBs <- BL.getContents
      case parseScormPackage zipBs of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right pkg -> BL.putStr (AesonPretty.encodePretty (toCodec pkg)) >> hFlush stdout
    ["--encode"] -> do
      jsonBs <- BL.getContents
      case Aeson.eitherDecode jsonBs of
        Left err -> hPutStrLn stderr ("JSON parse failed: " <> err) >> exitFailure
        Right cp ->
          case fromCodec cp of
            Left err -> hPutStrLn stderr err >> exitFailure
            Right pkg ->
              case serializeScormPackage pkg of
                Left err -> hPutStrLn stderr err >> exitFailure
                Right outZip -> BL.putStr outZip >> hFlush stdout
    ["--strip", scormFile, staticDir] -> stripScormImages scormFile staticDir
    _ -> usage >> exitFailure


