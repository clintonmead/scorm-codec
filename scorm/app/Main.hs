{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Scorm
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, hFlush, stdout)

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

usage :: IO ()
usage = do
  hPutStrLn stderr "Usage:"
  hPutStrLn stderr "  scorm-codec --decode   # ZIP on stdin -> JSON on stdout"
  hPutStrLn stderr "  scorm-codec --encode   # JSON on stdin -> ZIP on stdout"

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
    _ -> usage >> exitFailure


