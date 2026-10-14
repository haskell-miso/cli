-- | miso.lock: the exact versions of the dependencies of miso.yml.
--
-- > # miso.lock: written by miso update.  Commit it; do not edit it.
-- > # Local (path:) dependencies are not locked.
-- > miso: 0.1.0
-- > mhs: 0.16.10.0
-- > packages:
-- >   - name: split               # from Hackage: the version, and the
-- >     hackage: 0.2.5.1          # sha256 of its tarball
-- >     sha256: 1e72...
-- >   - name: lens-family         # from git: the rev of miso.yml (if any),
-- >     git: https://github.com/example/lens-family
-- >     rev: v2.1.3               # and the commit it was
-- >     commit: 3d0e...
--
-- miso update writes it; miso install and miso build install what it says.
-- A dependency of miso.yml that is not in it (or no longer matches it) is
-- installed as without a lock, with a warning.  Local dependencies and the
-- packages that come with the tool are never in it.
module Miso.CLI.Lock
  ( Locked (..)
  , lockFile
  , readLock
  , writeLock
  , lockedFor
  , lockedName
  , resolveDep
  , describeChanges
  , hackageTarball
  , sha256File
  ) where

import Control.Monad (unless)
import Data.Char (isDigit, isHexDigit, isSpace)
import Data.List (isPrefixOf)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeFile)

import MicroHs.Compile (mhsVersion)

import Miso.CLI.Color
import Miso.CLI.Config
import Miso.CLI.Util

lockFile :: FilePath
lockFile = "miso.lock"

data Locked
  = LockedHackage
      { lkName    :: String
      , lkVersion :: String
      , lkSha256  :: String
      }
  | LockedGit
      { lkName   :: String
      , lkRepo   :: String
      , lkRev    :: Maybe String    -- ^ the rev of miso.yml, if it had one
      , lkCommit :: String
      }
  deriving (Show, Eq)

lockedName :: Locked -> String
lockedName = lkName

-- | The entry of the lock for a dependency of miso.yml, if the lock has one
-- that still matches it (same source, and for git the same repository and
-- rev).
lockedFor :: [Locked] -> Dep -> Maybe Locked
lockedFor lock d
  | isLocal d = Nothing
  | otherwise =
      case [ l | l <- lock, lkName l == depName d, matches l ] of
        l : _ -> Just l
        [] -> Nothing
  where
    matches (LockedHackage _ _ _) = isHackage d
    matches (LockedGit _ repo rev _) = depGit d == Just repo && depRev d == rev

-----------------------------------------------------------------------------
-- Reading and writing

-- | The lock, or Nothing if there is no miso.lock.
readLock :: IO (Maybe [Locked])
readLock = do
  ok <- doesFileExist lockFile
  if not ok
    then pure Nothing
    else do
      s <- readFile' lockFile
      case parseYAML s >>= toLock of
        Right l -> pure (Just l)
        Left e -> failWith (lockFile ++ ": " ++ e ++ " (miso update writes it again)")

toLock :: Value -> Either String [Locked]
toLock (Map kvs) =
  case lookup "packages" kvs of
    Nothing -> Right []
    Just (List vs) -> mapM entry vs
    Just v -> Left ("packages should be a list, found " ++ show v)
toLock _ = Left "expected fields (packages: ...)"

entry :: Value -> Either String Locked
entry (Map kvs) = do
  name <- req "name"
  case (lookup "hackage" kvs, lookup "git" kvs) of
    (Just _, Nothing) -> do
      v <- req "hackage"
      h <- req "sha256"
      Right (LockedHackage name v h)
    (Nothing, Just _) -> do
      repo <- req "git"
      rev <- opt "rev"
      c <- req "commit"
      Right (LockedGit name repo rev c)
    _ -> Left ("package " ++ name ++ ": expected hackage: or git:")
  where
    req k = maybe (Left ("a package without " ++ k)) str (lookup k kvs)
    opt k = maybe (Right Nothing) (fmap Just . str) (lookup k kvs)
    str (Str s) = Right s
    str v = Left ("expected a string, found " ++ show v)
entry v = Left ("bad package " ++ show v)

writeLock :: [Locked] -> IO ()
writeLock ls =
  writeFile lockFile $ unlines $
    [ "# miso.lock: written by miso update.  Commit it; do not edit it."
    , "# Local (path:) dependencies are not locked."
    , "miso: " ++ version
    , "mhs: " ++ mhsVersion
    ] ++ (if null ls then ["packages: []"] else "packages:" : concatMap item ls)
  where
    item (LockedHackage n v h) =
      [ "  - name: " ++ n
      , "    hackage: " ++ v
      , "    sha256: " ++ h ]
    item (LockedGit n repo rev c) =
      [ "  - name: " ++ n
      , "    git: " ++ repo ]
      ++ maybe [] (\r -> ["    rev: " ++ r]) rev
      ++ [ "    commit: " ++ c ]

-----------------------------------------------------------------------------
-- Resolving: what miso update locks a dependency to

-- | Lock a (Hackage or git) dependency.  An entry of the old lock for the
-- same Hackage version is kept as it is (its tarball was checked when it was
-- locked).
resolveDep :: [Locked] -> Dep -> IO Locked
resolveDep old d
  | isGit d = resolveGit d
  | otherwise = do
      v <- newestVersion (depName d)
      case [ l | l@(LockedHackage n v' _) <- old, n == depName d, v' == v ] of
        l : _ -> pure l
        [] -> do
          f <- hackageTarball (depName d) v
          h <- sha256File f
          pure (LockedHackage (depName d) v h)

-- | The newest version of a package on Hackage that is not deprecated.
newestVersion :: String -> IO String
newestVersion n = do
  out <- readCommand ("curl -fsSL -H 'Accept: application/json' "
                      ++ quote ("https://hackage.haskell.org/package/" ++ n ++ "/preferred"))
  case out of
    Nothing -> failWith ("no package " ++ n ++ " on Hackage (or Hackage cannot be reached)")
    Just json ->
      case normalVersions json of
        [] -> failWith ("package " ++ n ++ " has no versions on Hackage that are not deprecated")
        vs -> pure (maxVersion vs)

-- | The "normal-version" list of Hackage's /preferred JSON.
normalVersions :: String -> [String]
normalVersions json =
  case breakOn "\"normal-version\"" json of
    Nothing -> []
    Just rest ->
      let list = takeWhile (/= ']') (drop 1 (dropWhile (/= '[') rest))
      in  [ v | x <- splitOn ',' list, let v = filter (/= '"') (trim x), not (null v) ]

maxVersion :: [String] -> String
maxVersion = foldr1 (\a b -> if versionKey a >= versionKey b then a else b)

versionKey :: String -> [Int]
versionKey v = [ read p | p <- splitOn '.' v, not (null p), all isDigit p ]

-- | The tarball of a package version from Hackage, in $MISO_HOME/src/hackage
-- (downloaded if it is not there).
hackageTarball :: String -> String -> IO FilePath
hackageTarball n v = do
  home <- misoHome
  let dir = home ++ "/src/hackage"
      nv = n ++ "-" ++ v
      file = dir ++ "/" ++ nv ++ ".tar.gz"
  createDirectoryIfMissing True dir
  there <- doesFileExist file
  unless there $ do
    step ("downloading " ++ nv)
    ok <- tryRun ("curl -fsSL -o " ++ quote file ++ " "
                  ++ quote ("https://hackage.haskell.org/package/" ++ nv ++ "/" ++ nv ++ ".tar.gz"))
    unless ok $ do
      gone <- not <$> doesFileExist file
      unless gone (removeFile file)
      failWith ("could not download " ++ nv ++ " from Hackage")
  pure file

-- | The sha256 of a file, with sha256sum (Linux, MSYS2) or shasum (macOS).
sha256File :: FilePath -> IO String
sha256File f = do
  hasSum <- have "sha256sum"
  let c = if hasSum then "sha256sum " ++ quote f else "shasum -a 256 " ++ quote f
  out <- readCommand c
  case fmap (takeWhile isHexDigit) out of
    Just h | length h == 64 -> pure h
    _ -> failWith ("could not compute the sha256 of " ++ f ++ " (sha256sum or shasum is needed)")

-- | The commit a git dependency is at: its rev (a branch, a tag, or a
-- commit), or HEAD.
resolveGit :: Dep -> IO Locked
resolveGit d = do
  let repo = maybe "" id (depGit d)
      rev = depRev d
  c <- case rev of
         Just r | isCommit r -> pure r
         _ -> do
           remote <- lsRemote repo (maybe "HEAD" id rev)
           case remote of
             Just c -> pure c
             Nothing -> case rev of
               Just r | length r >= 4 && all isHexDigit r -> expandCommit d repo r
               _ -> failWith ("dependency " ++ depName d ++ ": " ++ repo ++ " has no "
                              ++ maybe "HEAD" ("branch or tag " ++) rev)
  pure (LockedGit (depName d) repo rev c)
  where
    isCommit r = length r == 40 && all isHexDigit r

-- | The commit of a ref of a remote repository.  For an annotated tag it is
-- the commit the tag points to (the "^{}" line).
lsRemote :: String -> String -> IO (Maybe String)
lsRemote repo ref = do
  -- the peeled line of an annotated tag is only listed when asked for
  out <- readCommand ("git ls-remote " ++ quote repo ++ " " ++ quote ref ++ " " ++ quote (ref ++ "^{}"))
  case out of
    Nothing -> failWith ("could not reach " ++ repo ++ " (git ls-remote failed)")
    Just s -> do
      let refs = [ (r, c) | l <- lines s, (c, r0) <- [break isSpace l], let r = trim r0, length c == 40 ]
          wanted r = r == ref || r == "refs/tags/" ++ ref || r == "refs/heads/" ++ ref
          peeled = [ c | (r, c) <- refs, "^{}" `isSuffixOf'` r, wanted (take (length r - 3) r) ]
          plain = [ c | (r, c) <- refs, wanted r ]
      pure $ case peeled ++ plain of
        c : _ -> Just c
        [] -> Nothing

-- | The full commit of an abbreviated one: clone the repository (once, in
-- $MISO_HOME/src) and ask git.
expandCommit :: Dep -> String -> String -> IO String
expandCommit d repo short = do
  home <- misoHome
  let dir = home ++ "/src/" ++ depName d ++ "-resolve"
  there <- doesDirectoryExist dir
  if there
    then run ("git -C " ++ quote dir ++ " fetch --quiet")
    else run ("git clone --quiet --no-checkout " ++ quote repo ++ " " ++ quote dir)
  out <- readCommand ("git -C " ++ quote dir ++ " rev-parse --verify --quiet " ++ quote (short ++ "^{commit}"))
  case fmap trim out of
    Just c | length c == 40 -> pure c
    _ -> failWith ("dependency " ++ depName d ++ ": " ++ repo ++ " has no commit " ++ short)

-----------------------------------------------------------------------------
-- What changed

-- | The differences between two locks, one line each (nothing if equal).
describeChanges :: [Locked] -> [Locked] -> [String]
describeChanges old new =
  [ "+ " ++ lkName l ++ " " ++ what l | l <- new, lkName l `notElem` map lkName old ]
  ++ [ lkName l ++ " " ++ what o ++ " -> " ++ what l
     | l <- new, o <- old, lkName o == lkName l, o /= l ]
  ++ [ "- " ++ lkName o ++ " " ++ what o | o <- old, lkName o `notElem` map lkName new ]
  where
    what (LockedHackage _ v _) = v
    what (LockedGit _ _ rev c) = maybe "" (++ " ") rev ++ "(" ++ take 7 c ++ ")"

-----------------------------------------------------------------------------

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
                (a, _ : r) -> a : splitOn c r
                (a, []) -> [a]

-- | The rest of a string after the first occurrence of a needle.
breakOn :: String -> String -> Maybe String
breakOn needle = go
  where
    go [] = Nothing
    go s@(_ : r)
      | needle `isPrefixOf` s = Just (drop (length needle) s)
      | otherwise = go r
