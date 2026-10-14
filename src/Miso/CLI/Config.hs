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
-- miso itself, and base, containers, mtl, ... come installed with the tool.
--
-- Only the part of YAML these files need is supported: mappings, lists
-- (block and [a, b]), scalars, and comments.
module Miso.CLI.Config
  ( Config (..)
  , Dep (..)
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

import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist)

configFile :: FilePath
configFile = "miso.yml"

data Config = Config
  { cfgName       :: String
  , cfgVersion    :: String
  , cfgMain       :: String
  , cfgSourceDirs :: [FilePath]
  , cfgTarget     :: Maybe String
  , cfgMhsOptions :: [String]
  , cfgDeps       :: [Dep]
  } deriving (Show)

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
  main' <- maybe (Right "Main") str (lookup "main" kvs)
  dirs <- maybe (Right ["app"]) strs (lookup "source-dirs" kvs)
  target <- maybe (Right Nothing) (fmap Just . str) (lookup "target" kvs)
  opts <- maybe (Right []) strs (lookup "mhs-options" kvs)
  deps <- maybe (Right []) depsOf (lookup "dependencies" kvs)
  mapM_ known (map fst kvs)
  Right (Config name ver main' dirs target opts deps)
  where
    known k | k `elem` ["name", "version", "main", "source-dirs", "target", "mhs-options", "dependencies"] = Right ()
            | otherwise = Left ("miso.yml: unknown field " ++ k)
toConfig _ = Left "miso.yml: expected fields (name: ...)"

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
