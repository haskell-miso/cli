-- | What miso lock locks: the dependencies of miso.yml, and the packages
-- they depend on, as mcabal would install them.
--
-- mcabal installs a package by name at the version of its package list (a
-- Stackage snapshot, from mcabal update), and in a package's directory
-- installs nothing it depends on.  So miso locks the whole tree: for each
-- package, the build-depends of its library and executables (for this OS,
-- with MicroHs's names: random is random-mhs), at the versions of the
-- package list (or, for a package that is not in it, Hackage's newest), and
-- installs them in order, each after what it needs.  The packages that come
-- with the tool, and those built into MicroHs, are not locked, unless
-- miso.yml replaces one (git:, path:, or another version:).
module Miso.CLI.Resolve
  ( resolveTree
  , mcabalNames
  , builtInNames
  , bundledNames
  , packageDeps
  , mcabalEnv
  , isReplacement
  , withMiso
  ) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (filterM, forM_, unless, when)
import Data.Maybe (listToMaybe)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List (intercalate, isPrefixOf, nub)
import Data.Version (showVersion)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import qualified System.Info as Info

import qualified MicroCabal.Cabal as MC
import qualified MicroCabal.Env as MC
import qualified MicroCabal.Main as MC
import qualified MicroCabal.Parse as MC
import qualified MicroCabal.StackageList as MC

import MicroHs.Compile (mhsVersion)

import Miso.CLI.Cabalize (cabalized)
import Miso.CLI.Color
import Miso.CLI.Config
import Miso.CLI.Lock
import Miso.CLI.Store
import Miso.CLI.Util

-- | mcabal's environment, with the MicroHs backend (miso mhs).
mcabalEnv :: IO MC.Env
mcabalEnv = do
  setupEnv
  env <- MC.setupEnv
  -- quiet: mcabal says "Changing package random to random-mhs" otherwise
  pure env { MC.verbose = -1 }

-- | The name mcabal installs a package by (random is random-mhs).
patchedName :: MC.Env -> String -> String
patchedName env n = fst (MC.patchName (MC.backend env) env (n, MC.makeVersion []))

-- | mcabal's names, for matching miso.yml's dependencies with miso.lock's
-- packages.  (It runs miso mhs, so it is only asked for when needed.)
mcabalNames :: IO (String -> String)
mcabalNames = patchedName <$> mcabalEnv

-- | Which of these packages need no installing, as MicroHs has them built in
-- (text, bytestring, ...) or they come with the tool.  (It runs miso mhs, so
-- it is only asked for when needed.)
builtInNames :: [String] -> IO [String]
builtInNames ns = do
  env <- mcabalEnv
  bundled <- bundledNames
  filterM (fmap not . needsInstall env bundled . patchedName env) ns

-- | The names of the packages that come with the tool.
bundledNames :: IO [String]
bundledNames = map bName <$> bundledPackages

-- | Does a dependency of miso.yml replace a package of the tool: one that
-- comes with it, from git or a local directory, or at another version?  It
-- fails for a package that is part of the compiler (it has no source).
isReplacement :: [Bundled] -> Dep -> IO Bool
isReplacement bundled d =
  case [ b | b <- bundled, bName b == depName d ] of
    b : _ | isLocal d || isGit d || maybe False (/= bVersion b) (depVersion d) -> do
      when (bSource b == Nothing) $
        failWith (depName d ++ " comes with miso as part of its compiler (MicroHs " ++ mhsVersion ++ "), so it cannot be "
                  ++ "replaced: remove the git:, path: or version: of " ++ depName d ++ " in miso.yml")
      pure True
    _ -> pure False

-- | Does a package need installing: not one of the tool's, nor one built
-- into MicroHs (base, text, bytestring, ...)?  A package the project
-- installed already still needs it, on another machine.
needsInstall :: MC.Env -> [String] -> String -> IO Bool
needsInstall env bundled n
  | n `elem` bundled = pure False
  | otherwise = do
      available <- MC.doesPkgExist (MC.backend env) env n
      if not available then pure True else installedHere
  where
    installedHere = do
      md <- pkgDir
      let d = maybe "" (++ "/packages") md
      ok <- if null d then pure False else doesDirectoryExist d
      ps <- if ok then listDirectory d else pure []
      pure (any isOurs ps)
    -- n-1.2.3.pkg (and not n-foo-1.2.3.pkg)
    isOurs p = (n ++ "-") `isPrefixOf` p && ".pkg" `isSuffixOf'` p
               && all (`elem` "0123456789.") (drop (length n + 1) (take (length p - 4) p))

-- | The project's dependencies, with miso as a package like the others: a
-- dependency on miso with no source of its own (- miso) is the release's
-- (Miso.CLI.Store), and a project whose code imports Miso but does not
-- depend on it (miso.yml from a release that had miso among its packages)
-- depends on it too, with a warning.  With such a release, as it is.
withMiso :: Config -> IO Config
withMiso cfg = do
  md <- misoDefault
  case md of
    Nothing -> pure cfg
    Just m -> do
      let has = any ((== "miso") . depName) (allDeps cfg)
      imports <- if has then pure False else importsMiso cfg
      when imports $
        warn ("the code imports Miso, but miso.yml has no dependency on miso; using the miso of this release "
              ++ mdVersion m ++ " (miso add miso adds it)")
      let plain d = depName d == "miso" && isHackage d && depVersion d == Nothing
          fix ds = [ if plain d then defaultMisoDep m (depFlags d) else d | d <- ds ]
      pure cfg { cfgDeps = fix (cfgDeps cfg) ++ [ defaultMisoDep m Nothing | imports ]
               , cfgLibrary = fmap (\l -> l { libDeps = fix (libDeps l) }) (cfgLibrary cfg)
               , cfgExecutables = [ e { exeDeps = fix (exeDeps e) } | e <- cfgExecutables cfg ]
               , cfgTests = [ t { exeDeps = fix (exeDeps t) } | t <- cfgTests cfg ] }

-- | Does the project's code import a module of miso?
importsMiso :: Config -> IO Bool
importsMiso cfg = do
  dirs <- filterM doesDirectoryExist (allSourceDirs cfg)
  if null dirs then pure False
    else tryRun ("grep -rqE --include='*.hs' '^import +(qualified +)?Miso' " ++ unwords (map quote dirs))

-- | The packages a package needs: the build-depends of its libraries and
-- executables that are buildable, for this OS, architecture and compiler
-- and the cabal flags given, with MicroHs's names, as mcabal reads them;
-- not the package's own (internal) libraries.
packageDeps :: MC.Env -> [(String, Bool)] -> String -> FilePath -> IO [String]
packageDeps env fl why dir0 = do
  -- a miso project: its library, as the .cabal file miso makes of it
  dir <- cabalized Nothing dir0
  cabals <- filter (".cabal" `isSuffixOf'`) <$> listDirectory dir
  f <- case cabals of
         [f] -> pure f
         [] -> failWith (dir ++ " has no .cabal file")
         _ -> failWith (dir ++ " has several .cabal files")
  s <- readFile' (dir ++ "/" ++ f)
  let info = MC.FlagInfo { MC.os = Info.os, MC.arch = Info.arch, MC.flags = fl
                         , MC.impl = MC.backendNameVers (MC.backend env) }
      cbl = MC.normalizeAndPatch env info (MC.parseCabal f s)
      MC.Cabal sects = cbl
      sects' = MC.addMissing sects
      own = MC.getCabalName cbl : [ n | MC.Section t n _ <- sects', t `elem` ["library", "foreign-library"] ]
      deps = [ p | sect@(MC.Section t _ flds) <- sects', t `elem` ["library", "foreign-library", "executable"]
                 , MC.isBuildable sect, (p, _, _) <- MC.getBuildDepends flds ]
  -- mcabal's parser fails with error: say which package, and why it is needed
  r <- try (evaluate (forceList (nub [ p | p <- deps, p `notElem` own ])))
  case r of
    Right ps -> pure ps
    Left e -> failWith ("could not read " ++ dir ++ "/" ++ f ++ why ++ ": " ++ show (e :: SomeException))
  where forceList xs = sum (map length xs) `seq` xs

-- | Lock the dependencies of miso.yml and what they depend on, in the order
-- they install in (each package after the packages it needs).  With keep (miso
-- lock), a package miso.lock has stays at its version; otherwise (miso
-- update) it moves to the version of the package list.  Local dependencies
-- are not locked, but what they need is.
resolveTree :: Bool -> [Locked] -> Config -> IO [Locked]
resolveTree keep old cfg = do
  env <- mcabalEnv
  -- mcabal's package list, which every project shares (it runs mcabal
  -- update if there is none)
  snapshot <- withGlobalCabal $ do
    -- (not mcabalEnv, whose setupEnv is the project's $CABALDIR)
    genv <- (\e -> e { MC.verbose = -1 }) <$> MC.setupEnv
    map (\p -> (MC.stName p, showVersion (MC.stVersion p))) <$> MC.getPackageList genv
  tool <- bundledPackages
  replaced <- filterM (isReplacement tool) (allDeps cfg)
  -- a package of the tool that miso.yml replaces is locked as any other
  let bundled = [ bName b | b <- tool, bName b `notElem` map depName replaced ]
  home <- misoHome
  md <- misoDefault
  state <- newIORef ([], [])            -- the packages seen, and the lock (reversed)
  let pn = patchedName env
      roots = allDeps cfg
      -- the dependencies of miso.yml by that name; miso, when a package
      -- needs it, is the release's
      rootsFor n = case [ d | d <- roots, pn (depName d) == n ] of
        [] | n == "miso", Just m <- md -> [defaultMisoDep m Nothing]
        ds -> ds
      flagsOf d = maybe [] MC.decodeCabalFlags (depFlags d)
      emit l = modifyIORef state (\(seen, out) -> (seen, l : out))
      -- why a package is needed: "(needed by fail, needed by transformers-compat, ...)"
      why chain = if null chain then "" else " (needed by " ++ intercalate ", needed by " chain ++ ")"
      -- a package, by mcabal's name: one of miso.yml, or one a package needs
      -- (chain: the packages that need it, nearest first)
      visit chain n = do
        (seen, _) <- readIORef state
        unless (n `elem` seen) $ do
          modifyIORef state (\(s, out) -> (n : s, out))
          case rootsFor n of
            d : _ | Just dir <- depPath d -> do
              ok <- doesDirectoryExist dir
              unless ok $ failWith ("dependency " ++ depName d ++ ": no directory " ++ dir)
              packageDeps env (flagsOf d) (why chain) dir >>= mapM_ (visit (n : chain))
            d : _ | isGit d -> do
              let resolve = case md of
                    -- the release's miso, at its commit
                    Just m | isDefaultMiso m d -> pure (LockedGit "miso" (mdRepo m) Nothing (mdCommit m) (Just (mdVersion m)))
                    _ -> resolveGit d
              l <- maybe resolve pure (if keep then lockedFor pn old d else Nothing)
              dir <- checkoutAt (lkName l) (lkRepo l) (lkCommit l)
              packageDeps env (flagsOf d) (why chain) dir >>= mapM_ (visit (n : chain))
              emit l
            mroot -> do
              -- (a package of the tool is available, but miso.yml replaces it)
              need <- if n `elem` map (pn . depName) replaced then pure True else needsInstall env bundled n
              if not need then pure () else do
                l <- lockHackage snapshot chain n (depVersion =<< listToMaybe mroot)
                let LockedHackage _ v h = l
                    dir = home ++ "/src/hackage/" ++ n ++ "-" ++ v
                unpacked <- doesDirectoryExist dir
                unless unpacked $ do
                  tarball <- hackageTarball n v
                  h' <- sha256File tarball
                  unless (h' == h) $ failWith (n ++ "-" ++ v ++ ": the sha256 of the tarball is " ++ h' ++ ", " ++ lockFile ++ " has " ++ h)
                  _ <- unpackHackage tarball (n ++ "-" ++ v)
                  pure ()
                packageDeps env (concatMap flagsOf (take 1 mroot)) (why chain) dir >>= mapM_ (visit (n : chain))
                emit l
      -- a Hackage package: kept from miso.lock, or at the package list's
      -- version, or at the version of miso.yml
      lockHackage snapshot chain n forced = do
        let prev = [ l | l@(LockedHackage m v _) <- old, m == n, maybe True (== v) forced ]
        case prev of
          l : _ | keep || forced /= Nothing -> pure l
          _ -> do
            v <- case (forced, lookup n snapshot) of
                   (Just v, _) -> pure v
                   (_, Just v) -> pure v
                   _ -> do
                     step (n ++ " is not in mcabal's package list: Hackage's newest" ++ why chain)
                     newestVersion n
            case [ l | l@(LockedHackage _ v' _) <- prev, v' == v ] of
              l : _ -> pure l
              [] -> do
                f <- hackageTarball n v
                LockedHackage n v <$> sha256File f
  forM_ roots $ \d -> visit [] (pn (depName d))
  reverse . snd <$> readIORef state
