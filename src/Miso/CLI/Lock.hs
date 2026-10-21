-- | miso.lock: the exact versions of the dependencies of miso.yml, and of
-- the packages they depend on (Miso.CLI.Resolve), in the order they install
-- in.
--
-- > # miso.lock: written by miso lock and miso update.  Commit it; do not edit it.
-- > # Local (path:) dependencies are not locked.
-- > miso: 0.1.0
-- > mhs: 0.16.10.0
-- > bundled: containers-0.8 miso-1.14.0.0 mtl-2.3.1 transformers-0.6.1.2
-- > packages:
-- >   - name: split               # from Hackage: the version, and the
-- >     hackage: 0.2.5.1          # sha256 of its tarball
-- >     sha256: 1e72...
-- >   - name: lens-family         # from git: the rev of miso.yml, the
-- >     git: https://github.com/example/lens-family
-- >     rev: v2.1.3               # commit it was, and the version of the
-- >     commit: 3d0e...           # package there
-- >     version: 2.1.3
--
-- bundled: the versions of the packages that came with the tool and that the
-- project uses from it (not the ones it installs itself).  A newer tool may
-- come with others: miso install says so, and miso lock records them.
--
-- miso lock writes it (keeping what is locked already), miso update writes it
-- with the versions of mcabal's new package list; miso install and miso
-- build install what it says, in its order.
-- A dependency of miso.yml that is not in it (or no longer matches it) is
-- installed as without a lock, with a warning.  Local dependencies and the
-- packages that come with the tool are never in it.
module Miso.CLI.Lock
  ( Locked (..)
  , lockFile
  , readLock
  , readLockBundled
  , writeLock
  , lockedFor
  , lockedName
  , resolveGit
  , newestVersion
  , describeChanges
  , hackageTarball
  , sha256File
  , sha256Tree
  , gitCheckout
  ) where

import Control.Monad (unless)
import Data.Char (isDigit, isHexDigit, isSpace, toLower)
import Data.List (isPrefixOf)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeFile)

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
      , lkGitVersion :: Maybe String  -- ^ the package's version at the commit
      }
  deriving (Show, Eq)

lockedName :: Locked -> String
lockedName = lkName

-- | The entry of the lock for a dependency of miso.yml, if the lock has one
-- that still matches it (same source, and for git the same repository and
-- rev).  The lock has the name mcabal installs a package by, which the
-- function gives (random is random-mhs).
lockedFor :: (String -> String) -> [Locked] -> Dep -> Maybe Locked
lockedFor pn lock d
  | isLocal d = Nothing
  | otherwise =
      case [ l | l <- lock, lkName l `elem` [depName d, pn (depName d)], matches l ] of
        l : _ -> Just l
        [] -> Nothing
  where
    matches (LockedHackage _ v _) = isHackage d && maybe True (== v) (depVersion d)
    matches (LockedGit _ repo rev _ _) = depGit d == Just repo && depRev d == rev

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
        Left e -> failWith (lockFile ++ ": " ++ e ++ " (miso lock writes it again)")

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
      v <- opt "version"
      Right (LockedGit name repo rev c v)
    _ -> Left ("package " ++ name ++ ": expected hackage: or git:")
  where
    req k = maybe (Left ("a package without " ++ k)) str (lookup k kvs)
    opt k = maybe (Right Nothing) (fmap Just . str) (lookup k kvs)
    str (Str s) = Right s
    str v = Left ("expected a string, found " ++ show v)
entry v = Left ("bad package " ++ show v)

-- | The packages of the tool that miso.lock was written with (name-version),
-- or Nothing if it does not say.
readLockBundled :: IO (Maybe [String])
readLockBundled = do
  ok <- doesFileExist lockFile
  if not ok then pure Nothing else do
    s <- readFile' lockFile
    pure $ case parseYAML s of
      Right (Map kvs) | Just (Str b) <- lookup "bundled" kvs -> Just (words b)
      _ -> Nothing

-- | Write the lock, with the tool's packages the project uses (name-version).
writeLock :: [String] -> [Locked] -> IO ()
writeLock bundled ls =
  writeFile lockFile $ unlines $
    [ "# miso.lock: written by miso lock and miso update.  Commit it; do not edit it."
    , "# Local (path:) dependencies are not locked."
    , "miso: " ++ version
    , "mhs: " ++ mhsVersion
    ] ++ [ "bundled: " ++ unwords bundled | not (null bundled) ]
    ++ (if null ls then ["packages: []"] else "packages:" : concatMap item ls)
  where
    item (LockedHackage n v h) =
      [ "  - name: " ++ n
      , "    hackage: " ++ v
      , "    sha256: " ++ h ]
    item (LockedGit n repo rev c v) =
      [ "  - name: " ++ n
      , "    git: " ++ repo ]
      ++ maybe [] (\r -> ["    rev: " ++ r]) rev
      ++ [ "    commit: " ++ c ]
      ++ maybe [] (\x -> ["    version: " ++ x]) v

-----------------------------------------------------------------------------
-- Resolving: Hackage's newest version, and the commit of a git dependency

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

-- | One sha256 of the files under a directory, their paths and contents,
-- leaving out what mcabal builds there (dist-mcabal/).
sha256Tree :: FilePath -> IO String
sha256Tree dir = do
  hasSum <- have "sha256sum"
  let sha = if hasSum then "sha256sum" else "shasum -a 256"
  out <- readCommand ("cd " ++ quote dir ++ " && find . -type f ! -path './dist-mcabal/*' | LC_ALL=C sort"
                      ++ " | while IFS= read -r f; do " ++ sha ++ " \"$f\"; done | " ++ sha)
  case fmap (takeWhile isHexDigit) out of
    Just h | length h == 64 -> pure h
    _ -> failWith ("could not compute the sha256 of the files in " ++ dir ++ " (sha256sum or shasum is needed)")

-- | The commit a git dependency is at: its rev (a branch, a tag, or a
-- commit), or HEAD; and the version of the package there.
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
  dir <- gitCheckout (depName d) repo c
  v <- packageVersion dir
  case (rev, v) of
    (Just r, Just ver) | Just tagged <- tagVersion r, tagged /= ver ->
      warn (depName d ++ ": rev " ++ r ++ ", but the version in its package description is " ++ ver)
    _ -> pure ()
  pure (LockedGit (depName d) repo rev c v)
  where
    isCommit r = length r == 40 && all isHexDigit r

-- | The version a tag names: v1.2.3 or 1.2.3 (and not a branch or a commit).
tagVersion :: String -> Maybe String
tagVersion r =
  let v = case r of { c : rest | c `elem` "vV" -> rest; _ -> r }
  in  if not (null v) && isDigit (head v) && all (\c -> isDigit c || c == '.') v then Just v else Nothing

-- | A checkout of a git dependency at a commit, in $MISO_HOME/src (cloned
-- once; miso install builds it there).
gitCheckout :: String -> String -> String -> IO FilePath
gitCheckout n repo c = do
  home <- misoHome
  let dir = home ++ "/src/" ++ n ++ "-" ++ c
  cloned <- doesDirectoryExist dir
  unless cloned $ do
    step ("cloning " ++ repo)
    run ("git clone --quiet " ++ quote repo ++ " " ++ quote dir)
    run ("git -C " ++ quote dir ++ " checkout --quiet " ++ quote c)
  pure dir

-- | The version of the package in a directory: its miso.yml's, or (with
-- none) its .cabal file's.
packageVersion :: FilePath -> IO (Maybe String)
packageVersion dir = do
  yml <- doesFileExist (dir ++ "/" ++ configFile)
  if yml
    then do
      s <- readFile' (dir ++ "/" ++ configFile)
      pure (case parseYAML s of
              Right (Map kvs) | Just (Str v) <- lookup "version" kvs -> Just v
              _ -> Nothing)
    else do
      cabals <- filter (".cabal" `isSuffixOf'`) <$> listDirectory dir
      case cabals of
        [f] -> do
          s <- readFile' (dir ++ "/" ++ f)
          pure (case [ trim (drop 8 l) | l <- lines s, map toLower (take 8 l) == "version:" ] of
                  v : _ | not (null v) -> Just v
                  _ -> Nothing)
        _ -> pure Nothing

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
    what (LockedGit _ _ rev c v) = maybe "" (++ " ") rev ++ "(" ++ take 7 c ++ maybe "" (", version " ++) v ++ ")"

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
