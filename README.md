# Vibe Coded SCORM parser

A common disclaimer is "use at your own risk".

But for this code, just "don't use it".

This is an entirely vibe coded SCORM codec. It needs a lot of work but it does seem to parse the "Golf" examples on the [scorm.com](scorm.com) website and roundtrips them.

Indeed, like all the code, the rest of this readme documentation is AI generated.

Don't use this code blindly. 

## Installation

### Development Environment with Moodle

For a complete development environment with Moodle (automatically configured):

```bash
nix develop
```

This will automatically:
- Start MariaDB, PHP-FPM, and Nginx
- Install and configure Moodle (if not already installed)
- Display admin credentials (Username: `admin`, Password: `Admin123!`)
- Start a static file server on port 8081

Access Moodle at: http://localhost:8080

### Building the SCORM Parser

Build with Cabal:

```bash
cabal build
```

Or with Nix (if you have flakes enabled):

```bash
nix build
```

## Usage

### Library API

The main `Scorm` module provides functions to parse and serialize SCORM packages.

#### Basic Parsing

```haskell
import Scorm

-- Parse a SCORM ZIP file
result <- parseScormPackageFromFile "my-course.zip"
case result of
  Left err -> putStrLn $ "Parse error: " ++ err
  Right pkg -> do
    -- Print package summary
    printScormPackage pkg

    -- Convert to JSON
    let json = scormPackageToJSON pkg
    writeFile "course.json" (Text.unpack json)
```

#### Roundtrip Serialization

```haskell
import Scorm

main = do
  -- Parse original package
  Right original <- parseScormPackageFromFile "input.zip"

  -- Serialize to new ZIP file
  result <- serializeScormPackageToFile original "output.zip"
  case result of
    Left err -> putStrLn $ "Serialization error: " ++ err
    Right () -> putStrLn "Package saved successfully"
```

#### Working with Package Data

```haskell
import Scorm

-- Extract manifest information
getManifestInfo :: ScormPackage -> IO ()
getManifestInfo pkg = do
  let manifest = scormManifest pkg
  putStrLn $ "SCORM Version: " ++ show (scormVersion pkg)
  putStrLn $ "Manifest ID: " ++ Text.unpack (manifestIdentifier manifest)
  putStrLn $ "Organizations: " ++ show (length $ manifestOrganizations manifest)
  putStrLn $ "Resources: " ++ show (length $ manifestResources manifest)
  putStrLn $ "Files: " ++ show (length $ scormFiles pkg)
```

### CLI Tool

The `scorm-codec` executable provides command-line access to parsing and serialization.

#### Decode ZIP to JSON

```bash
# Read ZIP from file and output JSON
cat my-course.zip | cabal run scorm-codec -- --decode > course.json

# Or directly from a file
cabal run scorm-codec -- --decode < my-course.zip > course.json
```

#### Encode JSON to ZIP

```bash
# Read JSON and output ZIP
cat course.json | cabal run scorm-codec -- --encode > output.zip

# Or directly from a file
cabal run scorm-codec -- --encode < course.json > output.zip
```

#### CLI Examples

```bash
# Parse a SCORM package and pretty-print info
cabal run scorm-codec -- --decode < test-data/scorm12-simple.zip | jq '.manifest.identifier'

# Roundtrip: ZIP -> JSON -> ZIP
cat input.zip | cabal run scorm-codec -- --decode | cabal run scorm-codec -- --encode > output.zip
```

### Testing

Run the test suite:

```bash
cabal test
```

This will test parsing and roundtrip serialization of various SCORM 1.2 and 2004 packages.

### Supported SCORM Versions

- SCORM 1.2
- SCORM 2004 (2nd, 3rd, and 4th editions)

The parser attempts to auto-detect the version from the manifest content.

### Dependencies

- `base >= 4.14`
- `zip-archive >= 0.4` - for ZIP file handling
- `xml-conduit >= 1.9` - for XML parsing
- `aeson >= 2.0` - for JSON serialization
- `bytestring`, `text`, `containers` - standard Haskell libraries
