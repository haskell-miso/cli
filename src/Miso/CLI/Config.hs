-- | The project file, miso.yml.
--
-- > name: counter
-- > version: 0.1.0            # the version of the project (required)
-- > main: Main                 # the main module (default Main)
-- > source-dirs: [app]         # default [app]
-- > target: emscripten         # an mhs target (optional, default native)
-- > mhs-options: [-O]          # passed to mhs (optional)
-- > dependencies:
-- >   - containers             # from Hackage, installed with mcabal
-- >   - name: lens-family      # from git, at a revision, with cabal flags
-- >     git: https://github.com/example/lens-family
-- >     rev: v2.1.3
-- >     flags: -foo +bar
-- >   - name: my-lib           # a local checkout (relative to miso.yml)
-- >     path: ../my-lib
--
-- main, source-dirs and target there are a shorthand for a project with one
-- program.  A project with a library, or with several programs, has
-- sections for them instead:
--
-- > library:                   # code the programs share
-- >   source-dirs: [src]       # default [src]
-- >   exposed-modules: [Todo]  # default: every module under source-dirs
-- > executables:
-- >   web:                     # a name, for miso build web, miso dev web, ...
-- >     main: Main
-- >     source-dirs: [app]
-- >     target: browser
-- >   cli:
-- >     main: Cli
-- >     source-dirs: [cli]
-- >     dependencies: [split]  # added to the project's
--
-- The library and each program may also have dependencies and
-- mhs-options, added to the project's.
--
-- miso itself, and base, containers, mtl, ... come installed with the tool.
--
-- Only the part of YAML these files need is supported: mappings, lists
-- (block and [a, b]), scalars, and comments.
module Miso.CLI.Config
  ( Config (..)
  , Library (..)
  , Executable (..)
  , Native (..)
  , Dep (..)
  , allDeps
  , isNative
  , programDirs
  , programOptions
  , webDir
  , allSourceDirs
  , findProgram
  , noProgram
  , readConfig
  , parseConfig
  , configFile
  , isLocal
  , isGit
  , isHackage
    -- * The YAML subset (miso.lock uses it too)
  , Value (..)
  , parseYAML
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (intercalate, isPrefixOf)
import System.Directory (doesFileExist)

configFile :: FilePath
configFile = "miso.yml"

data Config = Config
  { cfgName        :: String
  , cfgVersion     :: String
  , cfgMhsOptions  :: [String]      -- ^ for every component
  , cfgDeps        :: [Dep]         -- ^ the top-level list, which miso add edits
  , cfgLibrary     :: Maybe Library
  , cfgExecutables :: [Executable]  -- ^ in the order of miso.yml
  } deriving (Show)

data Library = Library
  { libSourceDirs  :: [FilePath]
  , libExposed     :: Maybe [String]  -- ^ Nothing: every module under the source dirs
  , libMhsOptions  :: [String]
  , libDeps        :: [Dep]
  , libNative      :: Native
  } deriving (Show)

data Executable = Executable
  { exeName        :: String
  , exeMain        :: String
  , exeSourceDirs  :: [FilePath]
  , exeTarget      :: Maybe String
  , exeMhsOptions  :: [String]
  , exeDeps        :: [Dep]
  , exeNative      :: Native
  , exeStaticDir   :: FilePath        -- ^ copied next to a browser program
  } deriving (Show)

-- | What goes to the C compiler, cpphs and emcc, of the library or of a
-- program.  The lists of files are globs (cbits/*.c); Nothing is the
-- default: cbits/*.c, jsbits/*.js and include/, of the library, or when
-- there is none, of the programs.
data Native = Native
  { cSources       :: Maybe [String]
  , jsSources      :: Maybe [String]
  , includeDirs    :: Maybe [FilePath]
  , cppOptions     :: [String]        -- ^ -D, -I for cpphs (and the C compiler)
  , extraLibraries :: [String]        -- ^ -l for the linker
  , ccOptions      :: [String]
  , ldOptions      :: [String]
  } deriving (Show)

nativeFields :: [String]
nativeFields = ["c-sources", "js-sources", "include-dirs", "cpp-options", "extra-libraries", "cc-options", "ld-options"]

native :: [(String, Value)] -> Either String Native
native kvs =
  Native <$> optList "c-sources" <*> optList "js-sources" <*> optList "include-dirs"
         <*> list' "cpp-options" <*> list' "extra-libraries" <*> list' "cc-options" <*> list' "ld-options"
  where
    optList k = maybe (Right Nothing) (fmap Just . strs) (lookup k kvs)
    list' k = maybe (Right []) strs (lookup k kvs)

-- | A program for this machine, rather than for a browser.
isNative :: Executable -> Bool
isNative e = maybe True (== "native") (exeTarget e)

-- | The dependencies of the project and of all its components.
allDeps :: Config -> [Dep]
allDeps cfg = nubOn depName (cfgDeps cfg ++ maybe [] libDeps (cfgLibrary cfg) ++ concatMap exeDeps (cfgExecutables cfg))

-- | Where mhs looks for a program's modules: the library's, then its own.
programDirs :: Config -> Executable -> [FilePath]
programDirs cfg e = maybe [] libSourceDirs (cfgLibrary cfg) ++ exeSourceDirs e

-- | The mhs options of a program: the project's, the library's, its own.
programOptions :: Config -> Executable -> [String]
programOptions cfg e = cfgMhsOptions cfg ++ maybe [] libMhsOptions (cfgLibrary cfg) ++ exeMhsOptions e

-- | Where a browser program goes: public/, or with several browser
-- programs, public/NAME/.
webDir :: Config -> Executable -> FilePath
webDir cfg e
  | length (filter (not . isNative) (cfgExecutables cfg)) > 1 = "public/" ++ exeName e
  | otherwise = "public"

-- | The source dirs of every component.
allSourceDirs :: Config -> [FilePath]
allSourceDirs cfg = nubOn id (maybe [] libSourceDirs (cfgLibrary cfg) ++ concatMap exeSourceDirs (cfgExecutables cfg))

findProgram :: Config -> String -> Either String Executable
findProgram cfg n =
  case [ e | e <- cfgExecutables cfg, exeName e == n ] of
    e : _ -> Right e
    [] -> Left (noProgram cfg ("no program " ++ n ++ " in miso.yml"))

-- | A message that there is no such program, with the ones there are.
noProgram :: Config -> String -> String
noProgram cfg msg =
  case map exeName (cfgExecutables cfg) of
    [] -> msg ++ " (" ++ cfgName cfg ++ " is a library: it has no executables)"
    ns -> msg ++ " (its programs: " ++ intercalate ", " ns ++ ")"

nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f = go []
  where
    go _ [] = []
    go seen (x : xs) | f x `elem` seen = go seen xs
                     | otherwise = x : go (f x : seen) xs

data Dep = Dep
  { depName  :: String
  , depGit   :: Maybe String        -- ^ a git repository
  , depPath  :: Maybe FilePath      -- ^ a local directory (otherwise from Hackage)
  , depRev   :: Maybe String        -- ^ a git revision
  , depFlags :: Maybe String        -- ^ cabal flags, as for mcabal -f
  } deriving (Show, Eq)

-- | Where a dependency comes from: a local directory, git, or Hackage.
isLocal, isGit, isHackage :: Dep -> Bool
isLocal d = depPath d /= Nothing
isGit d = depGit d /= Nothing
isHackage d = not (isLocal d) && not (isGit d)

readConfig :: IO (Either String Config)
readConfig = do
  ok <- doesFileExist configFile
  if ok
    then parseConfig <$> readFile configFile
    else pure (Left ("no " ++ configFile ++ " here (miso new <name> makes a project)"))

parseConfig :: String -> Either String Config
parseConfig s = parseYAML s >>= toConfig

-----------------------------------------------------------------------------
-- A small YAML subset

data Value = Str String | List [Value] | Map [(String, Value)]
  deriving (Show)

-- Lines with their indentation, without comments and blank lines.
type Line = (Int, String)

parseYAML :: String -> Either String Value
parseYAML s =
  case block (map indent (filter (not . blank) (map stripComment (lines s)))) of
    Right (v, []) -> Right v
    Right (_, (_, l) : _) -> Left ("unexpected: " ++ l)
    Left e -> Left e
  where
    blank = all isSpace
    indent l = let (sp, r) = span (== ' ') l in (length sp, trimEnd r)

stripComment :: String -> String
stripComment = go False
  where
    -- a # starts a comment at the start of a line or after a space, outside quotes
    go _ [] = []
    go q (c : cs)
      | c == '"' = c : go (not q) cs
      | c == '#' && not q = []
      | otherwise = c : go' q c cs
    go' q prev (c : cs)
      | c == '#' && not q && isSpace prev = []
      | c == '"' = c : go' (not q) c cs
      | otherwise = c : go' q c cs
    go' _ _ [] = []

trimEnd :: String -> String
trimEnd = reverse . dropWhile isSpace . reverse

trim :: String -> String
trim = dropWhile isSpace . trimEnd

-- A block: all the lines at the indentation of the first one (and the more
-- indented lines under them).
block :: [Line] -> Either String (Value, [Line])
block [] = Right (Map [], [])
block ls@((n, l) : _)
  | "- " `isPrefixOf` l || l == "-" = list n ls []
  | otherwise = mapping n ls []

list :: Int -> [Line] -> [Value] -> Either String (Value, [Line])
list n ((m, l) : rest) acc
  | m == n && ("- " `isPrefixOf` l || l == "-") = do
      let item = trim (drop 1 l)
          (sub, rest') = span ((> n) . fst) rest
      v <- if null item
             then fst <$> block sub
             else if isKey item
               then fst <$> block ((n + 2, item) : sub)      -- "- key: v" starts a mapping
               else if null sub then Right (scalar item) else Left ("unexpected: " ++ snd (head sub))
      list n rest' (v : acc)
list _ rest acc = Right (List (reverse acc), rest)

mapping :: Int -> [Line] -> [(String, Value)] -> Either String (Value, [Line])
mapping n ((m, l) : rest) acc
  | m == n =
      case break (== ':') l of
        (k, ':' : v) -> do
          let (sub, rest') = span ((> n) . fst) rest
          val <- if null (trim v) then fst <$> block sub
                 else if null sub then Right (scalar (trim v))
                 else Left ("unexpected: " ++ snd (head sub))
          mapping n rest' ((trim k, val) : acc)
        _ -> Left ("expected key: value, found: " ++ l)
mapping _ rest acc = Right (Map (reverse acc), rest)

isKey :: String -> Bool
isKey s = case break (== ':') s of
            (k, ':' : r) -> not (null k) && all (\c -> not (isSpace c) && c /= '"') k && (null r || " " `isPrefixOf` r)
            _ -> False

scalar :: String -> Value
scalar s
  | "[" `isPrefixOf` s && last s == ']' = List [ Str (unquote (trim x)) | x <- splitOn ',' (init (tail s)), not (null (trim x)) ]
  | otherwise = Str (unquote s)

unquote :: String -> String
unquote s
  | length s >= 2 && head s == last s && head s `elem` "\"'" = init (tail s)
  | otherwise = s

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
                (a, _ : r) -> a : splitOn c r
                (a, []) -> [a]

-----------------------------------------------------------------------------

toConfig :: Value -> Either String Config
toConfig (Map kvs) = do
  name <- maybe (Left "miso.yml: missing name") str (lookup "name" kvs)
  ver <- case lookup "version" kvs of
           Just (Map []) -> Left missingVersion    -- "version:" with no value
           Just v -> str v
           Nothing -> Left missingVersion
  checkVersion ver
  opts <- maybe (Right []) strs (lookup "mhs-options" kvs)
  deps <- maybe (Right []) depsOf (lookup "dependencies" kvs)
  mapM_ (known "" (["name", "version", "mhs-options", "dependencies", "library", "executables"] ++ flatKeys)) (map fst kvs)
  lib <- maybe (Right Nothing) (fmap Just . library) (lookup "library" kvs)
  let flat = [ k | (k, _) <- kvs, k `elem` flatKeys ]
      hasLib = maybe False (const True) lib
  exes <- case lookup "executables" kvs of
    Just v | not (null flat) -> Left ("miso.yml: with executables:, " ++ intercalate ", " flat ++ " go under a program's name there (or library:), not at the top level")
           | otherwise -> executables v
    Nothing | hasLib && null flat -> Right []
            | otherwise -> (: []) <$> executable name (Map [ kv | kv@(k, _) <- kvs, k `elem` flatKeys ])
  let cfg = Config name ver opts deps lib exes
  checkSources (cfgDeps cfg ++ maybe [] libDeps lib ++ concatMap exeDeps exes)
  Right cfg
  where
    flatKeys = ["main", "source-dirs", "target", "static-dir"] ++ nativeFields
toConfig _ = Left "miso.yml: expected fields (name: ...)"

library :: Value -> Either String Library
library (Map kvs) = do
  mapM_ (known "library" (["source-dirs", "exposed-modules", "mhs-options", "dependencies"] ++ nativeFields)) (map fst kvs)
  Library <$> maybe (Right ["src"]) strs (lookup "source-dirs" kvs)
          <*> maybe (Right Nothing) (fmap Just . strs) (lookup "exposed-modules" kvs)
          <*> maybe (Right []) strs (lookup "mhs-options" kvs)
          <*> maybe (Right []) depsOf (lookup "dependencies" kvs)
          <*> native kvs
library v = Left ("miso.yml: library should have fields (source-dirs: ...), found " ++ show v)

executables :: Value -> Either String [Executable]
executables (Map kvs) = do
  when' (null kvs) (Left "miso.yml: executables: has none (a name, then its main: and source-dirs:)")
  mapM (\(n, v) -> executable n v) kvs
executables v = Left ("miso.yml: executables should be names with their fields (web: ...), found " ++ show v)

executable :: String -> Value -> Either String Executable
executable n (Map kvs) = do
  when' (not (validName n)) (Left ("miso.yml: " ++ n ++ " is not a name for a program (a letter, then letters, digits and -)"))
  mapM_ (known ("executables." ++ n) (["main", "source-dirs", "target", "mhs-options", "dependencies", "static-dir"] ++ nativeFields)) (map fst kvs)
  Executable n <$> maybe (Right "Main") str (lookup "main" kvs)
               <*> maybe (Right ["app"]) strs (lookup "source-dirs" kvs)
               <*> maybe (Right Nothing) (fmap Just . str) (lookup "target" kvs)
               <*> maybe (Right []) strs (lookup "mhs-options" kvs)
               <*> maybe (Right []) depsOf (lookup "dependencies" kvs)
               <*> native kvs
               <*> maybe (Right "static") str (lookup "static-dir" kvs)
executable n v = Left ("miso.yml: executables." ++ n ++ " should have fields (main: ...), found " ++ show v)

known :: String -> [String] -> String -> Either String ()
known section ks k
  | k `elem` ks = Right ()
  | otherwise = Left ("miso.yml: unknown field " ++ (if null section then "" else section ++ ".") ++ k)

validName :: String -> Bool
validName n = case n of
  c : cs -> isAlpha c && all (\x -> isAlphaNum x || x == '-') cs
  [] -> False

when' :: Bool -> Either String () -> Either String ()
when' b e = if b then e else Right ()

-- | A dependency listed in several places must come from the same place.
checkSources :: [Dep] -> Either String ()
checkSources ds =
  case [ depName a | (i, a) <- zip [0 :: Int ..] ds, (j, b) <- zip [0 ..] ds, i < j, depName a == depName b, a /= b ] of
    n : _ -> Left ("miso.yml: " ++ n ++ " is listed twice, differently; list it once, at the top level")
    [] -> Right ()

missingVersion :: String
missingVersion = "miso.yml: missing version (every miso.yml has one, e.g. version: 0.1.0)"

-- | A version is numbers separated by dots, as on Hackage: 0.1.0, 1.14.0.0.
checkVersion :: String -> Either String ()
checkVersion v
  | not (null parts) && all (\p -> not (null p) && all isDigit p) parts = Right ()
  | otherwise = Left ("miso.yml: version should be numbers separated by dots, like 0.1.0; found " ++ v)
  where parts = splitOn '.' v

str :: Value -> Either String String
str (Str s) = Right s
str v = Left ("miso.yml: expected a string, found " ++ show v)

strs :: Value -> Either String [String]
strs (List vs) = mapM str vs
strs (Str s) = Right [s]
strs v = Left ("miso.yml: expected a list, found " ++ show v)

depsOf :: Value -> Either String [Dep]
depsOf (List vs) = mapM dep vs
depsOf v = Left ("miso.yml: dependencies should be a list, found " ++ show v)

dep :: Value -> Either String Dep
dep (Str s) = Right (Dep s Nothing Nothing Nothing Nothing)
dep (Map kvs) = do
  name <- maybe (Left "miso.yml: a dependency without a name") str (lookup "name" kvs)
  git <- opt "git"
  path <- opt "path"
  rev <- opt "rev"
  flags <- opt "flags"
  mapM_ known (map fst kvs)
  case (git, path) of
    (Just _, Just _) -> Left ("miso.yml: dependency " ++ name ++ " has both git and path")
    _ -> Right (Dep name git path rev flags)
  where
    opt k = maybe (Right Nothing) (fmap Just . str) (lookup k kvs)
    known k | k `elem` ["name", "git", "path", "rev", "flags"] = Right ()
            | otherwise = Left ("miso.yml: unknown dependency field " ++ k)
dep v = Left ("miso.yml: bad dependency " ++ show v)
