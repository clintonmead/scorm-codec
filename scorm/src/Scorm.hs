{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Scorm
  ( -- * Types
    ScormPackage(..)
  , Manifest(..)
  , Metadata(..)
  , Organization(..)
  , Item(..)
  , Resource(..)
  , File(..)
  , Dependency(..)
  , ScormVersion(..)
  , ScormType(..)
  , -- * Parsing
    parseScormPackage
  , parseScormPackageFromFile
  , -- * Serialization
    serializeScormPackage
  , serializeScormPackageToFile
  , -- * Pretty printing / JSON
    printScormPackage
  , scormPackageToJSON
  ) where

import qualified Codec.Archive.Zip as Zip
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import System.FilePath (takeFileName)
import qualified Text.XML as XML
import qualified Text.XML.Cursor as Cursor
import Control.Applicative ((<|>))

-- | SCORM version (best-effort detection).
data ScormVersion
  = Scorm11
  | Scorm12
  | Scorm2004_2nd
  | Scorm2004_3rd
  | Scorm2004_4th
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- | SCORM type for resources.
data ScormType = SCO | Asset
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- | File reference in a resource.
data File = File
  { fileHref :: T.Text
  } deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- | Dependency reference.
data Dependency = Dependency
  { dependencyIdentifierRef :: T.Text
  } deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- | Resource definition.
data Resource = Resource
  { resourceIdentifier :: T.Text
  , resourceType :: Maybe T.Text
  , resourceHref :: Maybe T.Text
  , resourceScormType :: Maybe ScormType
  , resourceFiles :: [File]
  , resourceDependencies :: [Dependency]
  } deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- | Item in the organization tree.
data Item = Item
  { itemIdentifier :: T.Text
  , itemIdentifierRef :: Maybe T.Text
  , itemTitle :: Maybe T.Text
  , itemIsVisible :: Bool
  , itemItems :: [Item]
  } deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- | Organization structure.
data Organization = Organization
  { organizationIdentifier :: T.Text
  , organizationStructure :: Maybe T.Text
  , organizationTitle :: Maybe T.Text
  , organizationItems :: [Item]
  } deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- | Basic metadata from the manifest.
data Metadata = Metadata
  { metadataSchema :: Maybe T.Text
  , metadataSchemaVersion :: Maybe T.Text
  } deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- | Manifest (imsmanifest.xml).
data Manifest = Manifest
  { manifestIdentifier :: T.Text
  , manifestVersion :: Maybe T.Text
  , manifestMetadata :: Maybe Metadata
  , manifestOrganizations :: [Organization]
  , manifestResources :: [Resource]
  } deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- | Complete SCORM package.
--
-- We keep all files so we can re-emit a zip (roundtrip) while regenerating
-- the manifest XML from the parsed structure.
data ScormPackage = ScormPackage
  { scormManifest :: Manifest
  , scormFiles :: [(FilePath, BL.ByteString)]
  , scormVersion :: ScormVersion
  } deriving (Eq, Show, Generic)

instance Aeson.ToJSON ScormPackage where
  toJSON pkg =
    Aeson.object
      [ "scormManifest" Aeson..= scormManifest pkg
      , "scormFiles" Aeson..= map fst (scormFiles pkg)
      , "scormVersion" Aeson..= scormVersion pkg
      ]

-- | Parse SCORM package from a file path.
parseScormPackageFromFile :: FilePath -> IO (Either String ScormPackage)
parseScormPackageFromFile path = parseScormPackage <$> BL.readFile path

-- | Parse SCORM package from a ZIP bytestring.
parseScormPackage :: BL.ByteString -> Either String ScormPackage
parseScormPackage zipContent = do
  let archive = Zip.toArchive zipContent
  let entries = Zip.zEntries archive

  manifestEntry <- case findManifestEntry entries of
    Nothing -> Left "imsmanifest.xml not found in package"
    Just e -> Right e

  let manifestContent = Zip.fromEntry manifestEntry
  doc <- case XML.parseLBS XML.def manifestContent of
    Left err -> Left ("Failed to parse imsmanifest.xml: " <> show err)
    Right d -> Right d

  let cursor = Cursor.fromDocument doc
  let version = detectVersion cursor
  manifest <- parseManifest cursor

  let files = [(Zip.eRelativePath e, Zip.fromEntry e) | e <- entries]
  pure (ScormPackage manifest files version)

findManifestEntry :: [Zip.Entry] -> Maybe Zip.Entry
findManifestEntry =
  listToMaybe
    . filter (\e -> Zip.eRelativePath e == "imsmanifest.xml" || takeFileName (Zip.eRelativePath e) == "imsmanifest.xml")

detectVersion :: Cursor.Cursor -> ScormVersion
detectVersion cur =
  let sv = listToMaybe (cur Cursor.$// Cursor.laxElement "schemaVersion" Cursor.&/ Cursor.content)
      ident = fromMaybe "" (attr1 "identifier" cur)
      txt = T.toLower (fromMaybe "" sv <> " " <> ident)
      has2004 = "2004" `T.isInfixOf` txt || "cam" `T.isInfixOf` txt
      is4th = any (`T.isInfixOf` txt) ["4th", "20044th", "1.4"]
      is3rd = any (`T.isInfixOf` txt) ["3rd", "20043rd", "1.3"]
  in if has2004
        then if is4th
              then Scorm2004_4th
              else if is3rd
                    then Scorm2004_3rd
                    else Scorm2004_2nd
        else Scorm12

-- Cursor helpers (namespace-insensitive).
attr1 :: T.Text -> Cursor.Cursor -> Maybe T.Text
attr1 k c = listToMaybe (c Cursor.$| Cursor.laxAttribute k)

child1Text :: T.Text -> Cursor.Cursor -> Maybe T.Text
child1Text tag c = listToMaybe (c Cursor.$/ Cursor.laxElement tag Cursor.&/ Cursor.content)

children :: T.Text -> Cursor.Cursor -> [Cursor.Cursor]
children tag c = c Cursor.$/ Cursor.laxElement tag

descendants :: T.Text -> Cursor.Cursor -> [Cursor.Cursor]
descendants tag c = c Cursor.$// Cursor.laxElement tag

parseManifest :: Cursor.Cursor -> Either String Manifest
parseManifest cur = do
  -- Root cursor is the manifest element.
  let manifestC = cur
  identifier <- case attr1 "identifier" manifestC of
    Just i -> Right i
    Nothing -> Left "Manifest missing identifier attribute"
  let version = attr1 "version" manifestC

  let mdC = listToMaybe (children "metadata" manifestC)
  let metadata = fmap parseMetadata mdC

  let orgs =
        manifestC
          Cursor.$/ Cursor.laxElement "organizations"
          Cursor.&/ Cursor.laxElement "organization"
  organizations <- mapM parseOrganization orgs

  let res =
        manifestC
          Cursor.$/ Cursor.laxElement "resources"
          Cursor.&/ Cursor.laxElement "resource"
  resources <- mapM parseResource res

  pure (Manifest identifier version metadata organizations resources)

parseMetadata :: Cursor.Cursor -> Metadata
parseMetadata c =
  Metadata
    { metadataSchema = child1Text "schema" c
    , metadataSchemaVersion = child1Text "schemaVersion" c
    }

parseOrganization :: Cursor.Cursor -> Either String Organization
parseOrganization c = do
  identifier <- case attr1 "identifier" c of
    Just i -> Right i
    Nothing -> Left "Organization missing identifier attribute"
  let structure = attr1 "structure" c
  let title = child1Text "title" c
  items <- mapM parseItem (children "item" c)
  pure (Organization identifier structure title items)

parseItem :: Cursor.Cursor -> Either String Item
parseItem c = do
  identifier <- case attr1 "identifier" c of
    Just i -> Right i
    Nothing -> Left "Item missing identifier attribute"
  let identifierRef = attr1 "identifierref" c
  let isVisible =
        case fmap T.toLower (attr1 "isvisible" c) of
          Just "false" -> False
          _ -> True
  let title = child1Text "title" c
  items <- mapM parseItem (children "item" c)
  pure (Item identifier identifierRef title isVisible items)

parseResource :: Cursor.Cursor -> Either String Resource
parseResource c = do
  identifier <- case attr1 "identifier" c of
    Just i -> Right i
    Nothing -> Left "Resource missing identifier attribute"
  let rType = attr1 "type" c
  let href = attr1 "href" c

  -- scormType may be namespaced; laxAttribute ignores namespace, so local name match works.
  let scormTypeTxt = attr1 "scormType" c <|> attr1 "scormtype" c
  let scormType =
        case fmap T.toLower scormTypeTxt of
          Just "sco" -> Just SCO
          Just "asset" -> Just Asset
          _ -> Nothing

  let files = [File h | fc <- children "file" c, h <- maybeToList (attr1 "href" fc)]
  let deps = [Dependency r | dc <- children "dependency" c, r <- maybeToList (attr1 "identifierref" dc)]
  pure (Resource identifier rType href scormType files deps)

maybeToList :: Maybe a -> [a]
maybeToList = maybe [] pure

-- | Serialize SCORM package to a ZIP bytestring.
serializeScormPackage :: ScormPackage -> Either String BL.ByteString
serializeScormPackage pkg = do
  manifestXML <- manifestToXML (scormManifest pkg)

  let base = Zip.addEntryToArchive (Zip.toEntry "imsmanifest.xml" 0 manifestXML) Zip.emptyArchive
  let others = filter (\(p, _) -> takeFileName p /= "imsmanifest.xml") (scormFiles pkg)
  let archive = foldl (\a (p, bs) -> Zip.addEntryToArchive (Zip.toEntry p 0 bs) a) base others
  pure (Zip.fromArchive archive)

serializeScormPackageToFile :: ScormPackage -> FilePath -> IO (Either String ())
serializeScormPackageToFile pkg outPath =
  case serializeScormPackage pkg of
    Left e -> pure (Left e)
    Right bs -> BL.writeFile outPath bs >> pure (Right ())

manifestToXML :: Manifest -> Either String BL.ByteString
manifestToXML m = do
  let attrs =
        Map.fromList $
          [ (XML.Name "identifier" Nothing Nothing, manifestIdentifier m)
          ] ++ maybeToList (fmap (\v -> (XML.Name "version" Nothing Nothing, v)) (manifestVersion m))

  let metadataNodes =
        case manifestMetadata m of
          Nothing -> []
          Just md ->
            [ XML.NodeElement $
                XML.Element (XML.Name "metadata" Nothing Nothing) Map.empty $
                  concat
                    [ maybe [] (\s -> [XML.NodeElement (textElem "schema" s)]) (metadataSchema md)
                    , maybe [] (\sv -> [XML.NodeElement (textElem "schemaVersion" sv)]) (metadataSchemaVersion md)
                    ]
            ]

  let orgNodes =
        [ XML.NodeElement $
            XML.Element (XML.Name "organizations" Nothing Nothing) Map.empty $
              map (XML.NodeElement . organizationToXML) (manifestOrganizations m)
        ]

  let resNodes =
        [ XML.NodeElement $
            XML.Element (XML.Name "resources" Nothing Nothing) Map.empty $
              map (XML.NodeElement . resourceToXML) (manifestResources m)
        ]

  let root =
        XML.Element
          (XML.Name "manifest" Nothing Nothing)
          attrs
          (metadataNodes ++ orgNodes ++ resNodes)

  let doc = XML.Document (XML.Prologue [] Nothing []) root []
  pure (XML.renderLBS XML.def doc)

textElem :: T.Text -> T.Text -> XML.Element
textElem name txt =
  XML.Element
    (XML.Name name Nothing Nothing)
    Map.empty
    [XML.NodeContent txt]

organizationToXML :: Organization -> XML.Element
organizationToXML o =
  XML.Element
    (XML.Name "organization" Nothing Nothing)
    (Map.fromList $
      [ (XML.Name "identifier" Nothing Nothing, organizationIdentifier o)
      ] ++ maybeToList (fmap (\s -> (XML.Name "structure" Nothing Nothing, s)) (organizationStructure o))
    )
    ( maybeToList (fmap (\t -> XML.NodeElement (textElem "title" t)) (organizationTitle o))
      ++ map (XML.NodeElement . itemToXML) (organizationItems o)
    )

itemToXML :: Item -> XML.Element
itemToXML it =
  XML.Element
    (XML.Name "item" Nothing Nothing)
    (Map.fromList $
      [ (XML.Name "identifier" Nothing Nothing, itemIdentifier it)
      ] ++ maybeToList (fmap (\r -> (XML.Name "identifierref" Nothing Nothing, r)) (itemIdentifierRef it))
        ++ (if itemIsVisible it then [] else [(XML.Name "isvisible" Nothing Nothing, "false")])
    )
    ( maybeToList (fmap (\t -> XML.NodeElement (textElem "title" t)) (itemTitle it))
      ++ map (XML.NodeElement . itemToXML) (itemItems it)
    )

resourceToXML :: Resource -> XML.Element
resourceToXML r =
  XML.Element
    (XML.Name "resource" Nothing Nothing)
    (Map.fromList $
      [ (XML.Name "identifier" Nothing Nothing, resourceIdentifier r)
      ] ++ maybeToList (fmap (\t -> (XML.Name "type" Nothing Nothing, t)) (resourceType r))
        ++ maybeToList (fmap (\h -> (XML.Name "href" Nothing Nothing, h)) (resourceHref r))
        ++ maybeToList (fmap (\st -> (XML.Name "scormType" Nothing Nothing, scormTypeText st)) (resourceScormType r))
    )
    ( map (XML.NodeElement . fileToXML) (resourceFiles r)
      ++ map (XML.NodeElement . dependencyToXML) (resourceDependencies r)
    )

scormTypeText :: ScormType -> T.Text
scormTypeText SCO = "sco"
scormTypeText Asset = "asset"

fileToXML :: File -> XML.Element
fileToXML f =
  XML.Element
    (XML.Name "file" Nothing Nothing)
    (Map.singleton (XML.Name "href" Nothing Nothing) (fileHref f))
    []

dependencyToXML :: Dependency -> XML.Element
dependencyToXML d =
  XML.Element
    (XML.Name "dependency" Nothing Nothing)
    (Map.singleton (XML.Name "identifierref" Nothing Nothing) (dependencyIdentifierRef d))
    []

printScormPackage :: ScormPackage -> IO ()
printScormPackage pkg = do
  putStrLn "=== SCORM Package ==="
  putStrLn ("Version: " <> show (scormVersion pkg))
  putStrLn ("Manifest identifier: " <> T.unpack (manifestIdentifier (scormManifest pkg)))
  putStrLn ("Organizations: " <> show (length (manifestOrganizations (scormManifest pkg))))
  putStrLn ("Resources: " <> show (length (manifestResources (scormManifest pkg))))
  putStrLn ("Files: " <> show (length (scormFiles pkg)))

scormPackageToJSON :: ScormPackage -> T.Text
scormPackageToJSON =
  TE.decodeUtf8 . BL.toStrict . AesonPretty.encodePretty


