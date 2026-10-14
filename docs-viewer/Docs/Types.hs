{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | What miso docs extracts from a library, as the viewer reads it from the
-- JSON of data/: the index (the package, its modules, the names search
-- finds), and a module at a time.
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
  , sumText    :: [Inline]
  , sumLines   :: Int
  , sumExports :: Int
  }

-- | A name for search: name, kind, module, anchor.
data Def = Def MisoString MisoString MisoString MisoString

data Module = Module
  { modName   :: MisoString
  , modDoc    :: [Block]
  , modItems  :: [Item]
  , modSource :: [MisoString]   -- ^ highlighted HTML, a line each
  }

-- | Modules are equal when they have the same name: a module is loaded once.
instance Eq Module where
  a == b = modName a == modName b

data Item
  = Heading Int MisoString
  | Prose [Block]
  | Decl Declaration
  | ReExport MisoString Bool

data Declaration = Declaration
  { declName       :: MisoString
  , declAnchor     :: MisoString
  , declKind       :: MisoString
  , declSig        :: MisoString   -- ^ highlighted HTML
  , declDoc        :: [Block]
  , declMembers    :: [Declaration]
  , declSubs       :: [Sub]
  , declArgs       :: [(MisoString, [Block])]
  , declInstances  :: [Instance]
  , declLine       :: Int
  , declEnd        :: Int
  , declFrom       :: MisoString
  , declDeprecated :: MisoString
  }

data Sub = Sub
  { subName   :: MisoString
  , subAnchor :: MisoString
  , subKind   :: MisoString
  , subSig    :: MisoString
  , subDoc    :: [Block]
  , subFields :: [Sub]
  }

data Instance = Instance
  { instHead    :: MisoString
  , instDoc     :: [Block]
  , instModule  :: MisoString
  , instLine    :: Int
  , instEnd     :: Int
  , instDerived :: Bool
  }

data Block
  = Para [Inline]
  | CodeBlock MisoString                -- ^ highlighted HTML
  | Pre [Inline]
  | Example MisoString [MisoString]
  | Bullets [[Block]]
  | Numbered [[Block]]
  | Definitions [([Inline], [Block])]
  | Table [[[Inline]]] Int
  | Header Int [Inline]
  | MathBlock MisoString
  | Since MisoString

data Inline
  = Str MisoString
  | Code MisoString
  | Mono [Inline]
  | Emph [Inline]
  | Strong [Inline]
  | Ref MisoString MisoString           -- ^ an identifier, and its URL ("" if not here)
  | ModRef MisoString Bool
  | Link MisoString MisoString
  | Image MisoString MisoString
  | Math MisoString
  | Anchor MisoString

-----------------------------------------------------------------------------
-- JSON (see Miso.CLI.Docs.Json in the miso tool)

instance FromJSON Index where
  parseJSON = withObject "Index" $ \o ->
    Index <$> o .: "name" <*> o .: "version" <*> o .: "synopsis" <*> o .: "modules" <*> o .: "defs"

instance FromJSON Summary where
  parseJSON = withObject "Summary" $ \o ->
    Summary <$> o .: "n" <*> o .: "sum" <*> o .: "lines" <*> o .: "exports"

instance FromJSON Def where
  parseJSON = withArray "Def" $ \case
    [n, k, m, a] -> Def <$> parseJSON n <*> parseJSON k <*> parseJSON m <*> parseJSON a
    _ -> fail "Def"

instance FromJSON Module where
  parseJSON = withObject "Module" $ \o ->
    Module <$> o .: "name" <*> o .: "doc" <*> o .: "items" <*> o .: "src"

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
      <*> o .: "l" <*> o .: "e" <*> o .: "f" <*> (maybe "" id <$> o .:? "dep")
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

-- | A list that is left out when it is empty.
opt :: FromJSON a => M.Map MisoString Value -> MisoString -> Parser [a]
opt o k = maybe [] id <$> o .:? k

instance FromJSON Block where
  parseJSON = withArray "Block" $ \case
    [J.String "p", is] -> Para <$> parseJSON is
    [J.String "code", c] -> CodeBlock <$> parseJSON c
    [J.String "pre", is] -> Pre <$> parseJSON is
    [J.String "ex", e, rs] -> Example <$> parseJSON e <*> parseJSON rs
    [J.String "ul", xs] -> Bullets <$> parseJSON xs
    [J.String "ol", xs] -> Numbered <$> parseJSON xs
    [J.String "dl", xs] -> Definitions . map (\(Pair a b) -> (a, b)) <$> parseJSON xs
    [J.String "table", rows, h] -> Table <$> parseJSON rows <*> parseJSON h
    [J.String "hd", n, is] -> Header <$> parseJSON n <*> parseJSON is
    [J.String "math", m] -> MathBlock <$> parseJSON m
    [J.String "since", v] -> Since <$> parseJSON v
    _ -> fail "Block"

instance FromJSON Inline where
  parseJSON = \case
    J.String s -> pure (Str s)
    v -> flip (withArray "Inline") v $ \case
      [J.String "c", s] -> Code <$> parseJSON s
      [J.String "m", is] -> Mono <$> parseJSON is
      [J.String "e", is] -> Emph <$> parseJSON is
      [J.String "b", is] -> Strong <$> parseJSON is
      [J.String "r", n, u] -> Ref <$> parseJSON n <*> parseJSON u
      [J.String "M", m, k] -> ModRef <$> parseJSON m <*> parseJSON k
      [J.String "l", l, u] -> Link <$> parseJSON l <*> parseJSON u
      [J.String "img", u, t] -> Image <$> parseJSON u <*> parseJSON t
      [J.String "x", m] -> Math <$> parseJSON m
      [J.String "a", a] -> Anchor <$> parseJSON a
      _ -> fail "Inline"
