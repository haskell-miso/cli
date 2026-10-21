-- | The packages of the tool, and the store of the packages projects build.
--
-- The tool comes with packages (bundled): some are part of its compiler
-- (base, ...), and the others are libraries (containers, mtl, transformers,
-- miso), which the release ships the sources of too, in
-- mhs-VERSION/miso-sources/NAME-VERSION.tar.gz (with the cabal flags they
-- are built with in NAME-VERSION.flags).  A project can replace a library
-- (a dependency with git:, path:, or another version:); then the tool's
-- libraries that need it are built in the project again, from those
-- sources, against the replacement.
--
-- What a project installs goes in its .miso/.  A package built for one
-- project is kept in the store, $MISO_HOME/store/mhs-VERSION/ID, and copied
-- from there for the next project that needs the same one: ID names the
-- package, its source (a Hackage sha256, a git commit, or the tool's
-- source), its cabal flags, the compiler, and the IDs of the packages it was
-- built against.  So a package built against a replaced library is another
-- one in the store.
module Miso.CLI.Store
  ( Bundled (..)
  , bundledPackages
  , bundledDepends
  , dependentsOf
  , bundledSourceDir
  , toolPkgId
  , storeId
  , fromStore
  , toStore
  , pkgSums
  , removeOtherVersions
  , toolView
  , MisoDefault (..)
  , misoDefault
  , isDefaultMiso
  , defaultMisoDep
  , checkoutAt
  , fromPrebuilt
  ) where

import Control.Monad (filterM, forM, forM_, unless, when)
import Data.List (isPrefixOf, nub, sort)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeFile)

import MicroHs.Compile (mhsVersion)

import Miso.CLI.Color
import Miso.CLI.Config (Dep (..))
import Miso.CLI.Lock (gitCheckout, sha256File)
import Miso.CLI.Util

-- | A package that comes with the tool.
data Bundled = Bundled
  { bName    :: String
  , bVersion :: String
  , bFile    :: String          -- ^ NAME-VERSION.pkg, in the tool's packages/
  , bSource  :: Maybe FilePath  -- ^ its source (a library), or Nothing (part of the compiler)
  , bFlags   :: Maybe String    -- ^ the cabal flags it was built with
  } deriving (Show, Eq)

-- | The packages that come with the tool.
bundledPackages :: IO [Bundled]
bundledPackages = do
  db <- toolDb
  let listed = db ++ "/miso-bundled"
      dir = db ++ "/packages"
      srcs = db ++ "/miso-sources"
  hasList <- doesFileExist listed
  hasDir <- doesDirectoryExist dir
  files <- if hasList then lines <$> readFile' listed
           else if hasDir then filter (".pkg" `isSuffixOf'`) <$> listDirectory dir
           else pure []
  forM [ f | f <- files, ".pkg" `isSuffixOf'` f ] $ \f -> do
    let nv = take (length f - 4) f
        (n, v) = splitNameVersion nv
        tarball = srcs ++ "/" ++ nv ++ ".tar.gz"
        flagsFile = srcs ++ "/" ++ nv ++ ".flags"
    src <- doesFileExist tarball
    hasFlags <- doesFileExist flagsFile
    fl <- if hasFlags then Just . unwords . words <$> readFile' flagsFile else pure Nothing
    pure (Bundled n v f (if src then Just tarball else Nothing) fl)

-- | NAME-1.2.3 into NAME and 1.2.3.
splitNameVersion :: String -> (String, String)
splitNameVersion nv =
  let (rv, rn) = break (== '-') (reverse nv)
  in  (reverse (drop 1 rn), reverse rv)

-- | The names of the packages a package of the tool was built against (its
-- package says, mhs -L).
bundledDepends :: Bundled -> IO [String]
bundledDepends b = do
  db <- toolDb
  (ok, out) <- captureSelf ("mhs " ++ quote ("-L" ++ db ++ "/packages/" ++ bFile b))
  unless ok $ failWith ("could not read the package " ++ bFile b ++ " of the tool:\n" ++ out)
  pure [ fst (splitNameVersion d) | l <- lines out, "depends:" `isPrefixOf` l, d <- words (drop 8 l) ]

-- | The libraries of the tool that need one of these packages (which the
-- project replaces), directly or through another one, in the order they
-- are built in (each after the ones it needs).
dependentsOf :: [Bundled] -> [String] -> IO [Bundled]
dependentsOf bundled replaced = do
  let candidates = [ b | b <- bundled, bName b `notElem` replaced ]
  deps <- forM candidates $ \b -> (,) b <$> bundledDepends b
  let grow affected =
        let more = [ bName b | (b, ds) <- deps, bName b `notElem` affected, any (`elem` affected) ds ]
        in  if null more then affected else grow (affected ++ more)
      affected = grow replaced
      needed = [ (b, ds) | (b, ds) <- deps, bName b `elem` affected ]
      -- each after the ones it needs (among those built here)
      order done [] = reverse done
      order done todo =
        case [ x | x@(_, ds) <- todo, all (\d -> d `notElem` map (bName . fst) todo || d `elem` map bName done) ds ] of
          (b, _) : _ -> order (b : done) [ x | x@(c, _) <- todo, bName c /= bName b ]
          [] -> reverse done ++ map fst todo
  pure (order [] needed)

-- | The source of a library of the tool, unpacked in
-- $MISO_HOME/src/bundled/mhs-VERSION (once).
bundledSourceDir :: Bundled -> IO FilePath
bundledSourceDir b = do
  home <- misoHome
  let base = home ++ "/src/bundled/mhs-" ++ mhsVersion
      nv = bName b ++ "-" ++ bVersion b
      dir = base ++ "/" ++ nv
  tarball <- maybe (failWith (bName b ++ " comes with miso as part of its compiler: it cannot be built again")) pure (bSource b)
  there <- doesDirectoryExist dir
  unless there $ do
    createDirectoryIfMissing True base
    run ("rm -rf " ++ quote (dir ++ ".tmp") ++ " && mkdir " ++ quote (dir ++ ".tmp")
         ++ " && tar -xzf " ++ quote tarball ++ " -C " ++ quote (dir ++ ".tmp")
         ++ " && mv " ++ quote (dir ++ ".tmp/" ++ nv) ++ " " ++ quote dir
         ++ " && rm -rf " ++ quote (dir ++ ".tmp"))
  pure dir

-- | What a package of the tool is, for the IDs of the packages built
-- against it: its file and its checksum (another release may build the same
-- version differently).
toolPkgId :: Bundled -> IO String
toolPkgId b = do
  db <- toolDb
  out <- readCommand ("cksum " ++ quote (db ++ "/packages/" ++ bFile b))
  pure ("tool:" ++ bFile b ++ maybe "" ((":" ++) . takeWhile (/= ' ')) out)

-- | The tool's package database without the packages a project replaces:
-- .miso/tool/mhs-VERSION, which mhs uses instead of the tool's when there is
-- one (Miso.CLI.Util.packagePath).  mhs finds a package by name in every
-- database of its path (mhs -L, as mcabal checks that a dependency is
-- installed), and one that is in two is ambiguous; so the replaced packages
-- (and their modules) are left out, and the rest are links to the tool's.
-- With no packages replaced there is no view.
toolView :: FilePath -> [Bundled] -> [String] -> IO ()
toolView root tool replaced = do
  db <- toolDb
  let base = localDir root ++ "/tool"
      view = base ++ "/mhs-" ++ mhsVersion
      gone = [ bFile b | b <- tool, bName b `elem` replaced ]
      key = unlines (db : sort gone)
  if null gone
    then do
      there <- doesDirectoryExist base
      when there $ run ("rm -rf " ++ quote base)
    else do
      has <- doesFileExist (base ++ "/key")
      old <- if has then readFile' (base ++ "/key") else pure ""
      unless (old == key) $ do
        run ("rm -rf " ++ quote base)
        createDirectoryIfMissing True (view ++ "/packages")
        -- the packages, and their data directories, that are not replaced
        es <- listDirectory (db ++ "/packages")
        forM_ [ e | e <- es, e `notElem` gone, e `notElem` map (\f -> take (length f - 4) f) gone ] $ \e ->
          link (db ++ "/packages/" ++ e) (view ++ "/packages/" ++ e)
        -- the module files (A/B.txt) of the packages that are not replaced
        let walk rel = do
              let dir = db ++ (if null rel then "" else "/" ++ rel)
              createDirectoryIfMissing True (view ++ (if null rel then "" else "/" ++ rel))
              fs <- listDirectory dir
              forM_ fs $ \f -> do
                let r = if null rel then f else rel ++ "/" ++ f
                isDir <- doesDirectoryExist (db ++ "/" ++ r)
                if isDir
                  then unless (null rel && f `elem` ["packages", "miso-sources"]) $ walk r
                  else if ".txt" `isSuffixOf'` f
                    then do
                      c <- readFile' (db ++ "/" ++ r)
                      unless (filter (`notElem` "\r\n") c `elem` gone) $ do
                        createDirectoryIfMissing True (view ++ "/" ++ (if null rel then "" else rel))
                        writeFile (view ++ "/" ++ r) c
                    else link (db ++ "/" ++ r) (view ++ "/" ++ r)
        walk ""
        writeFile (base ++ "/key") key
  where
    -- a link (on Windows, MSYS2 copies)
    link from to = run ("ln -s " ++ quote from ++ " " ++ quote to)

-----------------------------------------------------------------------------
-- The miso of the release
--
-- miso is not one of the tool's packages: a project depends on it, as on any
-- other, and locks it.  A dependency on miso with no source of its own
-- (- miso) is the miso of the release: mhs-VERSION/miso-default has its
-- repository, commit, version and cabal flags, mhs-VERSION/miso-sources its
-- source, and mhs-VERSION/miso-prebuilt the package built from them
-- (scripts/prebuilt-miso.sh), which is installed instead of building it
-- again.  A release without miso-default has miso among its packages.

data MisoDefault = MisoDefault
  { mdRepo    :: String
  , mdCommit  :: String
  , mdVersion :: String
  , mdFlags   :: String
  } deriving (Show, Eq)

misoDefault :: IO (Maybe MisoDefault)
misoDefault = do
  db <- toolDb
  let f = db ++ "/miso-default"
  there <- doesFileExist f
  if not there then pure Nothing else do
    s <- readFile' f
    let kvs = [ (k, dropWhile (== ' ') (drop 1 v)) | l <- lines s, let (k, v) = break (== ':') l ]
    pure (MisoDefault <$> lookup "repository" kvs <*> lookup "commit" kvs <*> lookup "version" kvs
                      <*> Just (maybe "" id (lookup "flags" kvs)))

-- | The dependency on the miso of the release: from its repository, with no
-- rev of its own (it is locked at the release's commit), and its flags.
defaultMisoDep :: MisoDefault -> Maybe String -> Dep
defaultMisoDep m fl = Dep "miso" (Just (mdRepo m)) Nothing Nothing (Just (maybe (mdFlags m) id fl)) Nothing

isDefaultMiso :: MisoDefault -> Dep -> Bool
isDefaultMiso m d = depName d == "miso" && depGit d == Just (mdRepo m) && depRev d == Nothing

-- | The source of a package at a commit of a repository: the release's
-- (miso at its commit, without a download), or a checkout.
checkoutAt :: String -> String -> String -> IO FilePath
checkoutAt n repo c = do
  md <- misoDefault
  db <- toolDb
  case md of
    Just m | n == "miso", repo == mdRepo m, c == mdCommit m -> do
      let tarball = db ++ "/miso-sources/miso-" ++ mdVersion m ++ ".tar.gz"
      has <- doesFileExist tarball
      if has then bundledSourceDir (Bundled "miso" (mdVersion m) "" (Just tarball) (Just (mdFlags m)))
             else gitCheckout n repo c
    _ -> gitCheckout n repo c

-- | Copy the release's prebuilt miso into a package database, if it is the
-- one: the release's commit and flags (and built against the tool's
-- packages, which the caller knows).
fromPrebuilt :: FilePath -> String -> String -> String -> IO Bool
fromPrebuilt db repo c fl = do
  md <- misoDefault
  tdb <- toolDb
  let dir = tdb ++ "/miso-prebuilt"
  there <- doesDirectoryExist (dir ++ "/packages")
  case md of
    Just m | there, repo == mdRepo m, c == mdCommit m, words fl == words (mdFlags m) -> do
      createDirectoryIfMissing True db
      run ("cp -R " ++ quote dir ++ "/. " ++ quote db ++ "/")
      run ("chmod -R u+w " ++ quote db)
      pure True
    _ -> pure False

-----------------------------------------------------------------------------
-- The store

storeRoot :: IO FilePath
storeRoot = (\h -> h ++ "/store/mhs-" ++ mhsVersion) <$> misoHome

-- | The ID of a package in the store: NAME-VERSION-HASH, where the hash is
-- of what the package was built from (the lines).
storeId :: String -> String -> [String] -> IO String
storeId n v keyLines = do
  root <- storeRoot
  createDirectoryIfMissing True root
  let tmp = root ++ "/.key-" ++ n ++ "-" ++ v
  writeFile tmp (unlines keyLines)
  h <- sha256File tmp
  removeFile tmp
  pure (n ++ "-" ++ v ++ "-" ++ take 16 h)

-- | Copy a package from the store into a package database; is it there?
fromStore :: FilePath -> String -> IO Bool
fromStore db sid = do
  root <- storeRoot
  let entry = root ++ "/" ++ sid
  there <- doesDirectoryExist (entry ++ "/db/packages")
  when there $ do
    createDirectoryIfMissing True db
    run ("cp -R " ++ quote (entry ++ "/db") ++ "/. " ++ quote db ++ "/")
  pure there

-- | Keep a package that was installed in a package database in the store:
-- its .pkg file, its data directory, and the module files that name it, with
-- what it was built from (key).
toStore :: FilePath -> String -> String -> [String] -> IO ()
toStore db sid pkgFile keyLines = do
  root <- storeRoot
  let entry = root ++ "/" ++ sid
      tmp = root ++ "/.tmp-" ++ sid
      dataDir = take (length pkgFile - 4) pkgFile
  there <- doesDirectoryExist entry
  unless there $ do
    run ("rm -rf " ++ quote tmp)
    createDirectoryIfMissing True (tmp ++ "/db/packages")
    run ("cp " ++ quote (db ++ "/packages/" ++ pkgFile) ++ " " ++ quote (tmp ++ "/db/packages/"))
    hasData <- doesDirectoryExist (db ++ "/packages/" ++ dataDir)
    when hasData $ run ("cp -R " ++ quote (db ++ "/packages/" ++ dataDir) ++ " " ++ quote (tmp ++ "/db/packages/"))
    -- the modules: A/B.txt has the name of the package of module A.B
    run ("cd " ++ quote db ++ " && grep -rlxF --include='*.txt' --exclude-dir=packages " ++ quote pkgFile ++ " . "
         ++ "| tar -cf - -T - | (cd " ++ quote (tmp ++ "/db") ++ " && tar -xf -)")
    writeFile (tmp ++ "/key") (unlines keyLines)
    run ("mv " ++ quote tmp ++ " " ++ quote entry)

-- | The .pkg files of a package database, with a checksum each (to tell
-- which ones an install wrote).
pkgSums :: FilePath -> IO [(String, String)]
pkgSums db = do
  let dir = db ++ "/packages"
  there <- doesDirectoryExist dir
  fs <- if there then sort . filter (".pkg" `isSuffixOf'`) <$> listDirectory dir else pure []
  forM fs $ \f -> do
    out <- readCommand ("cksum " ++ quote (dir ++ "/" ++ f))
    pure (f, maybe "" (unwords . take 2 . words) out)

-- | Remove the other versions of a package from a package database (and the
-- module files that name them), before installing one.
removeOtherVersions :: FilePath -> String -> String -> IO ()
removeOtherVersions db n v = do
  let dir = db ++ "/packages"
  there <- doesDirectoryExist dir
  fs <- if there then listDirectory dir else pure []
  let others = nub [ f | f <- fs, let nv = if ".pkg" `isSuffixOf'` f then take (length f - 4) f else f
                       , (n ++ "-") `isPrefixOf` nv, isVersion (drop (length n + 1) nv), nv /= n ++ "-" ++ v ]
  old <- filterM (\f -> pure (".pkg" `isSuffixOf'` f)) others
  unless (null others) $ do
    run ("cd " ++ quote dir ++ " && rm -rf " ++ unwords (map quote others))
    -- the module files of the removed versions
    mapM_ (\f -> run ("cd " ++ quote db ++ " && grep -rlxF --include='*.txt' --exclude-dir=packages " ++ quote f
                      ++ " . | while IFS= read -r m; do rm -f \"$m\"; done")) old
  where isVersion s = not (null s) && all (`elem` "0123456789.") s
