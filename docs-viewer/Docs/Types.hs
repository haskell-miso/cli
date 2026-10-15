{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | What miso docs extracts from a library, as the viewer reads it from the
-- JSON of data/: the index (the package, its modules, the names search
-- finds), and a module at a time.  Documentation comes as HTML (the tool
-- draws it), so a module is a few nodes for each declaration.
module Docs.Types where

import qualified Data.Map as M
import Miso.JSON (FromJSON (..), Parser, Value, withArray, withObject, (.:), (.:?))
import qualified Miso.JSON as J
import Miso.String (MisoString)

data Index = Index
  { ixName     :: MisoString
  , ixVersion  :: MisoString
  , ixSynopsis :: MisoString
  , ixModules  :: [Summary]
  , ixDefs     :: [Def]
  }

-- | Indexes are equal when they are of the same package: one is loaded.
instance Eq Index where
  a == b = ixName a == ixName b

data Summary = Summary
  { sumName    :: MisoString
  , sumText    :: MisoString       -- ^ HTML: the first sentence of its docs
  , sumLines   :: Int
  , sumExports :: Int
  , sumHidden  :: Bool
  }

-- | A name for search: name, kind, module, anchor.
data Def = Def MisoString MisoString MisoString MisoString

data Module = Module
  { modName   :: MisoString
  , modDoc    :: MisoString       -- ^ HTML
  , modItems  :: [Item]
  , modSource :: [MisoString]   -- ^ highlighted HTML, a line each
  , modHeader :: [(MisoString, MisoString)]  -- ^ Copyright, License, ...
  }

-- | Modules are equal when they have the same name: a module is loaded once.
instance Eq Module where
  a == b = modName a == modName b

data Item
  = Heading Int MisoString
  | Prose MisoString            -- ^ HTML
  | Decl Declaration
  | ReExport MisoString Bool

data Declaration = Declaration
  { declName       :: MisoString
  , declAnchor     :: MisoString
  , declKind       :: MisoString
  , declSig        :: MisoString   -- ^ highlighted HTML
  , declDoc        :: MisoString   -- ^ HTML
  , declMembers    :: [Declaration]
  , declSubs       :: [Sub]
  , declArgs       :: [(MisoString, MisoString)]  -- ^ a type and its docs, HTML
  , declInstances  :: [Instance]
  , declLine       :: Int
  , declEnd        :: Int
  , declFrom       :: MisoString
  , declDeprecated :: MisoString
  , declWarning    :: MisoString   -- ^ "deprecated", "warning", or ""
  , declFixity     :: MisoString
  , declMinimal    :: MisoString
  }

data Sub = Sub
  { subName   :: MisoString
  , subAnchor :: MisoString
  , subKind   :: MisoString
  , subSig    :: MisoString
  , subDoc    :: MisoString       -- ^ HTML
  , subFields :: [Sub]
  }

data Instance = Instance
  { instHead    :: MisoString
  , instDoc     :: MisoString     -- ^ HTML
  , instModule  :: MisoString
  , instLine    :: Int
  , instEnd     :: Int
  , instDerived :: Bool
  }

-----------------------------------------------------------------------------
-- JSON (see Miso.CLI.Docs.Json in the miso tool)

instance FromJSON Index where
  parseJSON = withObject "Index" $ \o ->
    Index <$> o .: "name" <*> o .: "version" <*> o .: "synopsis" <*> o .: "modules" <*> o .: "defs"

instance FromJSON Summary where
  parseJSON = withObject "Summary" $ \o ->
    Summary <$> o .: "n" <*> o .: "sum" <*> o .: "lines" <*> o .: "exports" <*> (maybe False id <$> o .:? "hidden")

instance FromJSON Def where
  parseJSON = withArray "Def" $ \case
    [n, k, m, a] -> Def <$> parseJSON n <*> parseJSON k <*> parseJSON m <*> parseJSON a
    _ -> fail "Def"

instance FromJSON Module where
  parseJSON = withObject "Module" $ \o ->
    Module <$> o .: "name" <*> o .: "doc" <*> o .: "items" <*> o .: "src"
      <*> (map (\(Pair k v) -> (k, v)) <$> opt o "header")

instance FromJSON Item where
  parseJSON = withArray "Item" $ \case
    [J.String "h", n, t] -> Heading <$> parseJSON n <*> parseJSON t
    [J.String "p", bs] -> Prose <$> parseJSON bs
    [J.String "d", d] -> Decl <$> parseJSON d
    [J.String "re", m, b] -> ReExport <$> parseJSON m <*> parseJSON b
    _ -> fail "Item"

instance FromJSON Declaration where
  parseJSON = withObject "Declaration" $ \o ->
    Declaration <$> o .: "n" <*> o .: "a" <*> o .: "k" <*> o .: "s" <*> o .: "d"
      <*> opt o "m" <*> opt o "c" <*> (map pair <$> opt o "args") <*> opt o "i"
      <*> o .: "l" <*> o .: "e" <*> o .: "f" <*> str o "dep" <*> str o "wk" <*> str o "fx" <*> str o "min"
    where pair (Pair a b) = (a, b)

instance FromJSON Sub where
  parseJSON = withObject "Sub" $ \o ->
    Sub <$> o .: "n" <*> o .: "a" <*> o .: "k" <*> o .: "s" <*> o .: "d" <*> opt o "fs"

instance FromJSON Instance where
  parseJSON = withObject "Instance" $ \o ->
    Instance <$> o .: "h" <*> o .: "d" <*> o .: "m" <*> o .: "l" <*> o .: "e" <*> o .: "dv"

-- | Two things in an array.
data Pair a b = Pair a b

instance (FromJSON a, FromJSON b) => FromJSON (Pair a b) where
  parseJSON = withArray "Pair" $ \case
    [a, b] -> Pair <$> parseJSON a <*> parseJSON b
    _ -> fail "Pair"

-- | A string that is left out when it is empty.
str :: M.Map MisoString Value -> MisoString -> Parser MisoString
str o k = maybe "" id <$> o .:? k

-- | A list that is left out when it is empty.
opt :: FromJSON a => M.Map MisoString Value -> MisoString -> Parser [a]
opt o k = maybe [] id <$> o .:? k
