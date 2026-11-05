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
--
-- The versions the build-depends take are checked: a package from Hackage
-- gets a version that the packages that need it take (the package list's,
-- when they take it, or else Hackage's newest that they do), and when the
-- tree is resolved, a version that a package does not take (one of miso.yml
-- or miso.lock, one that comes with the tool, or one that a package needs
-- after its version was chosen) is an error, before anything is built.
-- When one does not fit, the tree is resolved again, with the ranges the
-- pass found choosing the versions from the start (a package read later
-- may have needed an older version of one chosen before it), a few times
-- at most.  That is not a search: no other versions are tried when the
-- ranges known do not fit.
-- The packages built into MicroHs (base, ...) are not checked, as their
-- versions are MicroHs's.
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
import Data.Maybe (isJust, listToMaybe)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (intercalate, isPrefixOf, nub, nubBy, sort)
import Data.Version (showVersion, versionBranch)
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
packageDeps env fl why dir = nub . map fst . snd <$> packageNeeds env fl why dir

-- | A package's version (of its .cabal file), and the packages it needs
-- (as 'packageDeps'), each with the versions it takes (Nothing: any).
packageNeeds :: MC.Env -> [(String, Bool)] -> String -> FilePath -> IO (Maybe String, [(String, Maybe MC.VersionRange)])
packageNeeds env fl why dir0 = do
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
      deps = [ (p, r) | sect@(MC.Section t _ flds) <- sects', t `elem` ["library", "foreign-library", "executable"]
                      , MC.isBuildable sect, (p, _, r) <- MC.getBuildDepends flds ]
      ver = listToMaybe [ showVersion v | MC.Section "global" _ flds <- sects, MC.Field "version" (MC.VVersion v) <- flds ]
      sameNeed (p, r) (q, r') = p == q && show r == show r'
  -- mcabal's parser fails with error: say which package, and why it is needed
  r <- try (evaluate (forceList (nubBy sameNeed [ d | d@(p, _) <- deps, p `notElem` own ])))
  case r of
    Right ps -> pure (ver, ps)
    Left e -> failWith ("could not read " ++ dir ++ "/" ++ f ++ why ++ ": " ++ show (e :: SomeException))
  where forceList xs = sum (map (length . show) xs) `seq` xs

-- | Lock the dependencies of miso.yml and what they depend on, in the order
-- they install in (each package after the packages it needs).  With keep (miso
-- lock), a package miso.lock has stays at its version; otherwise (miso
-- update) it moves to the version of the package list.  Local dependencies
-- are not locked, but what they need is.
resolveTree :: Bool -> [Locked] -> Config -> IO [Locked]
resolveTree keep old cfg = do
  env <- mcabalEnv
  -- mcabal's package list, which every project shares (it runs mcabal
  -- update if there is none).  Read when a Hackage package needs a version
  -- from it, not before: a project of the tool's packages (miso,
  -- containers, ...) locks and builds without the network.
  snapRef <- newIORef Nothing
  let getSnapshot = readIORef snapRef >>= \ms -> case ms of
        Just snap -> pure snap
        Nothing -> do
          snap <- withGlobalCabal $ do
            -- (not mcabalEnv, whose setupEnv is the project's $CABALDIR)
            genv <- (\e -> e { MC.verbose = -1 }) <$> MC.setupEnv
            map (\p -> (MC.stName p, showVersion (MC.stVersion p))) <$> MC.getPackageList genv
          writeIORef snapRef (Just snap)
          pure snap
  tool <- bundledPackages
  replaced <- filterM (isReplacement tool) (allDeps cfg)
  -- a package of the tool that miso.yml replaces is locked as any other
  let bundled = [ bName b | b <- tool, bName b `notElem` map depName replaced ]
  home <- misoHome
  md <- misoDefault
  -- One pass of the resolution: the lock, the ranges the chosen packages
  -- need, and the ones that do not fit.  seeds: the ranges a pass before
  -- found, which choose the versions here too (a package read later in
  -- that pass may have needed an older version of one chosen before it).
  let pass seeds = do
        state <- newIORef ([], [])            -- the packages seen, and the lock (reversed)
        needsRef <- newIORef []               -- (package, versions taken, by which package-version)
        chosenRef <- newIORef []              -- (package, (its version, where it comes from))
        let pn = patchedName env
            roots = allDeps cfg
            -- the dependencies of miso.yml by that name; miso, when a package
            -- needs it, is the release's
            rootsFor n = case [ d | d <- roots, pn (depName d) == n ] of
              [] | n == "miso", Just m <- md -> [defaultMisoDep m Nothing]
              ds -> ds
            flagsOf d = maybe [] MC.decodeCabalFlags (depFlags d)
            emit l = modifyIORef state (\(seen, out) -> (seen, l : out))
            -- the version of a package, and where it comes from; and what a
            -- package (who) needs, with the versions it takes, then visited
            chooses n v from = modifyIORef chosenRef ((n, (v, from)) :)
            needs who chain ps = do
              modifyIORef needsRef ([ (p, r, who) | (p, Just r) <- ps ] ++)
              mapM_ (visit chain) (nub (map fst ps))
            named n = maybe n ((n ++ "-") ++)
            -- (with the ranges of the pass before: see go)
            rangesFor n = (\ns -> nubBy (\(r, w) (r', w') -> w == w' && showRange r == showRange r')
                                     [ (r, who) | (p, r, who) <- ns ++ seeds, p == n ]) <$> readIORef needsRef
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
                    (v, ps) <- packageNeeds env (flagsOf d) (why chain) dir
                    maybe (pure ()) (\x -> chooses n x (" (path: " ++ dir ++ ")")) v
                    needs (named n v) (n : chain) ps
                  d : _ | isGit d -> do
                    let resolve = case md of
                          -- the release's miso, at its commit
                          Just m | isDefaultMiso m d -> pure (LockedGit "miso" (mdRepo m) Nothing (mdCommit m) (Just (mdVersion m)))
                          _ -> resolveGit d
                    l <- maybe resolve pure (if keep then lockedFor pn old d else Nothing)
                    dir <- checkoutAt (lkName l) (lkRepo l) (lkCommit l)
                    (v0, ps) <- packageNeeds env (flagsOf d) (why chain) dir
                    let v = maybe v0 Just (lkGitVersion l)
                    maybe (pure ()) (\x -> chooses n x (" (git: " ++ lkRepo l ++ " at " ++ take 7 (lkCommit l) ++ ")")) v
                    needs (named n v) (n : chain) ps
                    emit l
                  mroot -> do
                    -- (a package of the tool is available, but miso.yml replaces it)
                    need <- if n `elem` map (pn . depName) replaced then pure True else needsInstall env bundled n
                    -- one of the tool's has its version; one built into MicroHs is not checked
                    unless need $
                      forM_ [ bVersion b | b <- tool, bName b == n, n `elem` bundled ] $ \v -> chooses n v " (it comes with miso)"
                    if not need then pure () else do
                      l <- lockHackage chain n (depVersion =<< listToMaybe mroot)
                      let LockedHackage _ v h = l
                          dir = home ++ "/src/hackage/" ++ n ++ "-" ++ v
                      unpacked <- doesDirectoryExist dir
                      unless unpacked $ do
                        tarball <- hackageTarball n v
                        h' <- sha256File tarball
                        unless (h' == h) $ failWith (n ++ "-" ++ v ++ ": the sha256 of the tarball is " ++ h' ++ ", " ++ lockFile ++ " has " ++ h)
                        _ <- unpackHackage tarball (n ++ "-" ++ v)
                        pure ()
                      chooses n v (if isJust (depVersion =<< listToMaybe mroot) then " (version: of miso.yml)" else "")
                      (_, ps) <- packageNeeds env (concatMap flagsOf (take 1 mroot)) (why chain) dir
                      needs (n ++ "-" ++ v) (n : chain) ps
                      emit l
            -- a Hackage package: kept from miso.lock, or at the package list's
            -- version, or at the version of miso.yml
            lockHackage chain n forced = do
              ranges <- rangesFor n
              let prev = [ l | l@(LockedHackage m v _) <- old, m == n, maybe True (== v) forced ]
                  fits v = all (inVersions v . fst) ranges
                  takenBy = intercalate ", " [ who ++ " takes " ++ showRange r | (r, who) <- ranges ]
              case prev of
                l : _ | forced /= Nothing || keep && fits (lkVersion l) -> pure l
                _ -> do
                  v <- case forced of
                         Just v -> pure v
                         Nothing -> do
                           snapshot <- getSnapshot
                           case lookup n snapshot of
                             Just v | fits v -> pure v
                             listed -> do
                               when (listed == Nothing) $
                                 step (n ++ " is not in mcabal's package list: Hackage's newest" ++ (if null ranges then "" else " that fits") ++ why chain)
                               vs <- if null ranges then (: []) <$> newestVersion n else hackageVersions n
                               case filter fits vs of
                                 -- none fits: the check at the end says which packages disagree
                                 [] -> pure (maybe (maxVersion vs) id listed)
                                 ok -> do
                                   let v = maxVersion ok
                                   maybe (pure ()) (\lv ->
                                     step (n ++ " " ++ v ++ ", not " ++ lv ++ " of mcabal's package list: " ++ takenBy)) listed
                                   pure v
                  case [ l | l@(LockedHackage _ v' _) <- prev, v' == v ] of
                    l : _ -> pure l
                    [] -> do
                      f <- hackageTarball n v
                      LockedHackage n v <$> sha256File f
        forM_ roots $ \d -> visit [] (pn (depName d))
        -- every version a package needs, of the versions chosen
        ns <- reverse <$> readIORef needsRef
        chosen <- readIORef chosenRef
        let conflicts = nub [ who ++ " needs " ++ p ++ " " ++ showRange r ++ ", but " ++ p ++ " is " ++ v ++ from
                            | (p, r, who) <- ns, Just (v, from) <- [lookup p chosen], not (inVersions v r) ]
        lock <- reverse . snd <$> readIORef state
        pure (ns, conflicts, lock)
      -- Pass again, with the ranges found, while that finds versions that
      -- do not fit and new ranges (a package at another version needs other
      -- versions); at most a few times.  Only the ranges of the versions a
      -- pass chose go to the next, and only they are checked.  This is not a
      -- search: no other versions are tried when one does not fit.
      go n seeds = do
        (ns, conflicts, lock) <- pass seeds
        let key = sort . nub . map (\(p, r, who) -> (p, showRange r, who))
        if null conflicts then pure lock
          else if n >= (4 :: Int) || key ns == key seeds then
            failWith ("versions that do not fit:\n" ++ intercalate "\n" (map ("  " ++) conflicts)
                      ++ "\n(a version: in miso.yml, or miso update for the newest versions, may help)")
          else do
            step "versions that do not fit: resolving again, with the versions the packages take"
            go (n + 1) ns
  go 1 []

-----------------------------------------------------------------------------
-- The versions build-depends take

-- | Is a version (1.2.3) one of a range of build-depends?  An unreadable
-- version is not checked.
inVersions :: String -> MC.VersionRange -> Bool
inVersions v r = maybe True (\x -> inRange (versionBranch x) r) (MC.readVersionM v)

-- | ^>= 1.2.3 is >= 1.2.3 && < 1.3, and == 1.2.* a version that starts with 1.2.
inRange :: [Int] -> MC.VersionRange -> Bool
inRange v r = case r of
  MC.VEQ w -> v == vb w
  MC.VGT w -> v > vb w
  MC.VLT w -> v < vb w
  MC.VLE w -> v <= vb w
  MC.VGE w -> v >= vb w
  MC.VGEHat w -> v >= vb w && v < majorAfter (vb w)
  MC.VEQWild w -> take (length (vb w)) v == vb w
  MC.VOr a b -> inRange v a || inRange v b
  MC.VAnd a b -> inRange v a && inRange v b
  MC.VEQSet ws -> any ((== v) . vb) ws
  MC.VGEHatSet ws -> any (inRange v . MC.VGEHat) ws
  where
    vb = versionBranch
    majorAfter w = case w of
      a : b : _ -> [a, b + 1]
      [a] -> [a, 1]
      [] -> [0, 1]

-- | A range as build-depends has it.
showRange :: MC.VersionRange -> String
showRange r = case r of
  MC.VEQ w -> "==" ++ showVersion w
  MC.VGT w -> ">" ++ showVersion w
  MC.VLT w -> "<" ++ showVersion w
  MC.VLE w -> "<=" ++ showVersion w
  MC.VGE w -> ">=" ++ showVersion w
  MC.VGEHat w -> "^>=" ++ showVersion w
  MC.VEQWild w -> "==" ++ showVersion w ++ ".*"
  MC.VOr a b -> showRange a ++ " || " ++ showRange b
  MC.VAnd a b -> inner a ++ " && " ++ inner b
  MC.VEQSet ws -> "==" ++ set ws
  MC.VGEHatSet ws -> "^>=" ++ set ws
  where
    inner x@(MC.VOr _ _) = "(" ++ showRange x ++ ")"
    inner x = showRange x
    set ws = "{" ++ intercalate ", " (map showVersion ws) ++ "}"
