-- | Dependencies: installing them, and miso add/remove/list/update.
module Miso.CLI.Deps
  ( installDeps
  , addDeps
  , removeDeps
  , listPackages
  , lockDeps
  , updateDeps
  , vendorDeps
  , addToYml
  , removeFromYml
  ) where

import Control.Monad (filterM, foldM, forM, forM_, unless, when)
import Data.Char (isSpace)
import Data.List (intercalate, isPrefixOf, nub, sort, sortBy)
import Data.Ord (comparing)
import Data.Maybe (isJust)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)

import qualified MicroCabal.Main as MC
import MicroHs.Compile (mhsVersion)

import Miso.CLI.Cabalize (cabalized)
import Miso.CLI.Color
import Miso.CLI.Config
import Miso.CLI.Lock
import Miso.CLI.Resolve
import Miso.CLI.Store
import Miso.CLI.Util

-- | The record of what miso install installed in the project: a line per
-- package (its store ID, or for a local one its directory and files), kept
-- in the project's package database, so removing .miso/ installs again.  It
-- is per release, so a new one installs them again.
stampFile :: FilePath -> FilePath
stampFile db = db ++ "/miso-installed-" ++ version

-- | What the dependencies were when they were last installed (miso.yml's,
-- miso.lock, the tool's packages, vendor/): with no local dependency, if
-- that is the same nothing needs installing.
planFile :: FilePath -> FilePath
planFile db = db ++ "/miso-plan-" ++ version

readStamp :: FilePath -> IO [String]
readStamp db = do
  let f = stampFile db
  ok <- doesFileExist f
  if ok then lines <$> readFile' f else pure []

-- | mcabal install, with the cabal flags of a dependency.  Recursive, it
-- installs what the package needs that is not installed yet, by name, from
-- mcabal's package list; otherwise that is an error (a locked package is
-- installed after the packages it needs, which the lock has too).
mcabalInstall :: Bool -> Dep -> IO String
mcabalInstall recursive d = do
  exe <- self
  pure (quote exe ++ " mcabal" ++ (if recursive then " -r" else "")
        ++ maybe "" (\f -> " -f" ++ quote f) (depFlags d) ++ " install")

-- | A package to install in the project.
data Unit = Unit
  { uDep    :: Dep              -- ^ as miso.yml has it (or the lock, for what they need)
  , uLocked :: Maybe Locked     -- ^ its entry in miso.lock
  , uTool   :: Maybe Bundled    -- ^ a library of the tool, built again from its source
  , uDir    :: Maybe FilePath   -- ^ its source (Nothing: mcabal -r installs it by name)
  , uWhere  :: Maybe String     -- ^ where it comes from, to say so
  }

-- | Install the dependencies that are not installed yet, in the project's
-- .miso/.  A local checkout is installed again when its files change; the
-- others once.  With a miso.lock, its packages (those of miso.yml, and what
-- they need) are installed at its versions and commits; then the local
-- dependencies, and those it does not lock (which mcabal installs with what
-- they need).  Each package is installed after the ones it needs.  In a
-- vendored project (vendor/.miso-vendor) the locked packages are installed
-- from vendor/, and one that is not there as miso vendor copied it is an
-- error, not a download.
--
-- A dependency that replaces a package of the tool is installed in the
-- project, and the tool's libraries that need it are built again there,
-- from their source, against it.  A package built from a known source (not
-- a local directory, nor something built against one) is kept in the store
-- (Miso.CLI.Store), and copied from there when another project needs it.
installDeps :: Config -> IO ()
installDeps cfg0 = do
  cfg <- withMiso cfg0
  setupEnv
  root <- projectRoot >>= maybe (failWith ("no " ++ configFile ++ " here")) pure
  let db = projectDb root
  lock <- readLock
  tool <- bundledPackages
  replaced <- filterM (isReplacement tool) (allDeps cfg)
  let replacedNames = map depName replaced
      -- the packages of the tool the project uses (not those it replaces)
      bundled = [ bName b | b <- tool, bName b `notElem` replacedNames ]
  vend <- readVendor
  -- the plan: if nothing changed since the last install, nothing to do
  lockText <- readIfThere lockFile
  vendText <- readIfThere vendorFile
  let planKey = unlines ([ "miso " ++ version, "mhs " ++ mhsVersion, lockText, vendText ]
                         ++ map show (allDeps cfg) ++ map bFile tool)
      hasLocal = any isLocal (allDeps cfg)
  oldPlan <- readIfThere (planFile db)
  if not hasLocal && oldPlan == planKey && not (null oldPlan)
    then step "dependencies are installed"
    else do
      checkBundledVersions tool replacedNames
      installPlan False cfg db lock tool replaced bundled vend
      writeFile (planFile db) planKey
  where
    readIfThere f = do
      ok <- doesFileExist f
      if ok then readFile' f else pure ""

-- | Warn when the tool's libraries are not the versions miso.lock was
-- written with (another release of the tool).
checkBundledVersions :: [Bundled] -> [String] -> IO ()
checkBundledVersions tool replacedNames = do
  locked <- readLockBundled
  let now = [ bName b ++ "-" ++ bVersion b | b <- tool, bSource b /= Nothing, bName b `notElem` replacedNames ]
  case locked of
    Just was | sort was /= sort now -> do
      let changed = [ n ++ ": " ++ maybe "-" id (lookup n (pairs was)) ++ " -> " ++ maybe "-" id (lookup n (pairs now))
                    | n <- nub (map fst (pairs was) ++ map fst (pairs now))
                    , lookup n (pairs was) /= lookup n (pairs now) ]
      warn ("this miso comes with other versions of its libraries than " ++ lockFile ++ " was written with ("
            ++ intercalate ", " changed ++ "); the project builds with this miso's.  miso lock records them; "
            ++ "to keep one, add it to miso.yml with its version: (- name: miso, version: ...)")
    _ -> pure ()
  where
    pairs = map (\nv -> let (rv, rn) = break (== '-') (reverse nv) in (reverse (drop 1 rn), reverse rv))

-- | Install with a lock: miso.lock, or (resolved) one made for this
-- install, with the dependencies miso.lock does not lock too.
installPlan :: Bool -> Config -> FilePath -> Maybe [Locked] -> [Bundled] -> [Dep] -> [String] -> Maybe [(String, (String, String))] -> IO ()
installPlan resolved cfg db lock tool replaced bundled vend = do
  -- mcabal's names (random is locked as random-mhs), asked for only when a
  -- dependency of miso.yml is not in the lock by its own name
  let replacedNames = map depName replaced
      unmatched = [ d | Just l <- [lock], d <- allDeps cfg, not (isLocal d)
                      , depName d `notElem` bundled, depName d `notElem` map lockedName l ]
  pn <- if null unmatched then pure id else mcabalNames
  let locked d = maybe Nothing (\l -> lockedFor pn l d) lock
      -- a package of the lock: a dependency of miso.yml, or one they need
      depOf l = case [ d | d <- allDeps cfg, locked d == Just l ] of
                  d : _ -> d
                  [] -> Dep { depName = lockedName l, depGit = Nothing, depPath = Nothing, depRev = Nothing, depFlags = Nothing, depVersion = Nothing }
  -- a dependency of miso.yml that MicroHs has built in (text, bytestring,
  -- ...) is neither locked nor installed; mcabal is asked which, for those
  -- the lock lacks
  let maybeBuiltIn = [ depName d | d <- allDeps cfg, isHackage d, depName d `notElem` bundled
                                 , depName d `notElem` replacedNames, locked d == Nothing ]
  builtIn <- if null maybeBuiltIn then pure [] else builtInNames maybeBuiltIn
  let unlocked = [ d | d <- allDeps cfg, locked d == Nothing, isLocal d || depName d `notElem` (bundled ++ builtIn) ]
      -- an entry of the lock for a dependency of miso.yml that has changed
      -- (another git rev, or now local) is not installed: the dependency is,
      -- as unlocked, until miso lock; nor one for a package of the tool that
      -- miso.yml no longer replaces
      stale l = or [ locked d /= Just l | d <- allDeps cfg, lockedName l `elem` [depName d, pn (depName d)] ]
                || lockedName l `elem` bundled
      items = [ (depOf l, Just l) | l <- maybe [] id lock, not (stale l) ] ++ [ (d, Nothing) | d <- unlocked ]
  -- a dependency the lock does not have: a warning, or in a vendored
  -- project an error, before anything is installed
  let unlockedRemote = [ d | d <- unlocked, not (isLocal d) ]
  unless resolved $ case (lock, vend) of
    (Nothing, Just _) | not (null unlockedRemote) ->
      failWith ("this project is vendored, but has no " ++ lockFile ++ ": miso vendor locks and vendors its dependencies")
    (_, Just _) ->
      forM_ unlockedRemote $ \d ->
        failWith (depName d ++ " is not locked in " ++ lockFile ++ ", and this project is vendored: miso vendor locks and vendors it")
    (Just _, Nothing) ->
      forM_ unlockedRemote $ \d ->
        warn (depName d ++ " is not locked in " ++ lockFile ++ " (new, or changed in miso.yml); miso lock locks it")
    _ -> pure ()
  -- what miso.lock does not lock is locked for this install, as miso lock
  -- would (without writing miso.lock): its versions and what they need are
  -- then known, so each is installed after what it needs, and kept in the
  -- store
  if not resolved && not (null unlockedRemote)
    then do
      step ("resolving " ++ intercalate ", " (map depName unlockedRemote) ++ " (miso lock writes " ++ lockFile ++ ")")
      new <- resolveTree True (maybe [] id lock) cfg
      installPlan True cfg db (Just new) tool replaced bundled vend
    else installLocked cfg db lock tool replaced items pn

installLocked :: Config -> FilePath -> Maybe [Locked] -> [Bundled] -> [Dep] -> [(Dep, Maybe Locked)] -> (String -> String) -> IO ()
installLocked _ db _ tool replaced items _ = do
  let replacedNames = map depName replaced
  vend <- readVendor
  root <- projectRoot >>= maybe (failWith ("no " ++ configFile ++ " here")) pure
  -- the libraries of the tool that need a package miso.yml replaces
  rebuilt <- if null replaced then pure []
             else dependentsOf [ b | b <- tool, bSource b /= Nothing ] replacedNames
  -- the tool's packages, without the ones the project replaces or builds
  -- again (each package is in one database of mhs's path)
  toolView root tool (replacedNames ++ map bName rebuilt)
  -- where each one's source is (fetched now: what it needs, and its ID,
  -- come from it)
  units0 <- forM items $ \(d, ml) -> do
    (dir, from) <- sourceOf vend d ml
    pure (Unit d ml Nothing dir from)
  units1 <- forM rebuilt $ \b -> do
    dir <- bundledSourceDir b
    let d = Dep (bName b) Nothing Nothing Nothing (bFlags b) Nothing
    pure (Unit d Nothing (Just b) (Just dir) Nothing)
  env <- mcabalEnv
  pnEnv <- mcabalNames
  needs <- forM (units0 ++ units1) $ \u -> do
    ds <- case uDir u of
      Just dir -> packageDeps env (maybe [] MC.decodeCabalFlags (depFlags (uDep u))) "" dir
      Nothing -> pure []
    pure (u, ds)
  let nameOf u = maybe (pnEnv (depName (uDep u))) lockedName (uLocked u)
      ordered = order [] needs
      -- each after the ones it needs (among these); the rest in their order
      order done [] = reverse done
      order done todo =
        case [ x | x@(_, ds) <- todo, all (\n -> n `notElem` map (nameOf . fst) todo) ds ] of
          x : _ -> order (x : done) [ y | y <- todo, nameOf (fst y) /= nameOf (fst x) ]
          [] -> reverse done ++ todo
  -- the IDs: what each package is built from, with the IDs of what it needs
  toolIds <- forM tool $ \b -> (,) (bName b) <$> toolPkgId b
  ids <- foldM (\acc (u, ds) -> do
                  k <- unitKey acc toolIds u ds
                  pure (acc ++ [(nameOf u, k)])) [] ordered
  createDirectoryIfMissing True db
  -- a package of the tool the project no longer replaces or builds again
  -- (miso.yml changed) goes, and with it what says it was installed
  let mine = replacedNames ++ map bName rebuilt
      dropped = [ bName b | b <- tool, bName b `notElem` mine ]
  forM_ dropped $ \n -> removeOtherVersions db n ""
  done0 <- readStamp db
  let done = [ k | k <- done0, not (any (\n -> (n ++ "-") `isPrefixOf` k) dropped) ]
  -- (a local package is installed again when its files change)
  let todo = [ (u, ds, k) | ((u, ds), (_, k)) <- zip ordered ids, keyStamp k `notElem` done ]
      planned = map (nameOf . fst) ordered
  if null todo
    then step "dependencies are installed"
    else forM_ todo $ \(u, ds, k) -> do
      let d = uDep u
          ver = case (uLocked u, uTool u) of
                  (Just l, _) -> lockedAt l
                  (_, Just b) -> " " ++ bVersion b
                  _ -> maybe "" (" " ++) (depVersion d)
          what = depName d ++ ver
      fromCache <- case k of
        Stored sid keyLines -> do
          maybe (pure ()) (\v -> removeOtherVersions db (nameOf u) v) (unitVersion u)
          got <- fromStore db sid
          when got $ info ("Installing " ++ what ++ " (built already, from the store)")
          -- the release's miso, built against the tool's packages
          pre <- case uLocked u of
            Just (LockedGit "miso" repo _ c _) | not got, all (`notElem` planned) ds ->
              fromPrebuilt db repo c (maybe "" id (depFlags d))
            _ -> pure False
          when pre $ do
            info ("Installing " ++ what ++ " (built already, as it came with this miso)")
            toStore db sid (depName d ++ "-" ++ maybe "" id (unitVersion u) ++ ".pkg") keyLines
          pure (got || pre)
        _ -> pure False
      unless fromCache $ do
        info $ case uTool u of
          Just _ -> "Building " ++ what ++ " again from its source, against " ++ intercalate ", " replacedNames ++ " of miso.yml"
          Nothing -> "Installing " ++ depName d ++ maybe "" (" from " ++) (uWhere u) ++ ver
        maybe (pure ()) (\v -> removeOtherVersions db (nameOf u) v) (unitVersion u)
        before <- pkgSums db
        installUnit u
        after <- pkgSums db
        case k of
          Stored sid keyLines ->
            case [ f | (f, c) <- after, lookup f before /= Just c, (nameOf u ++ "-") `isPrefixOf` f ] of
              f : _ -> toStore db sid f keyLines
              [] -> pure ()
          _ -> pure ()
      appendFile (stampFile db) (keyStamp k ++ "\n")
  -- what is installed now: the packages of this plan
  writeFile (stampFile db) (unlines (map (keyStamp . snd) ids))
  where
    lockedAt (LockedHackage _ v _) = " " ++ v
    lockedAt (LockedGit _ _ _ c v) = " at " ++ take 7 c ++ maybe "" (\x -> " (" ++ x ++ ")") v

-- | What a package is built from: in the store (its ID, and what the ID is
-- of), or not (it is local, or built against a local one, or mcabal installs
-- it by name).
data Key = Stored String [String] | NotStored String

keyStamp :: Key -> String
keyStamp (Stored sid _) = sid
keyStamp (NotStored s) = s

-- | The version a package will be installed at, if known before.
unitVersion :: Unit -> Maybe String
unitVersion u = case (uLocked u, uTool u) of
  (Just (LockedHackage _ v _), _) -> Just v
  (Just (LockedGit _ _ _ _ v), _) -> v
  (_, Just b) -> Just (bVersion b)
  _ -> depVersion (uDep u)

-- | The key of a package: its source, flags and compiler, and the keys of
-- the packages it needs (those of the plan, the tool's, or MicroHs's).
unitKey :: [(String, Key)] -> [(String, String)] -> Unit -> [String] -> IO Key
unitKey planned toolIds u ds = do
  let d = uDep u
      depKey n = case lookup n planned of
        Just k -> Right (keyStamp k)
        Nothing -> case lookup n toolIds of
          Just t -> Right t
          Nothing -> Right ("mhs:" ++ n)
      local = [ n | n <- ds, Just (NotStored _) <- [lookup n planned] ]
      common = [ "flags: " ++ maybe "" id (depFlags d), "mhs: " ++ mhsVersion ]
               ++ [ "needs: " ++ n ++ " " ++ k | n <- sort ds, Right k <- [depKey n] ]
  case (uLocked u, uTool u, uDir u) of
    _ | isLocal d -> do
          let dir = maybe "" id (depPath d)
          h <- sha256Tree dir
          pure (NotStored ("path " ++ dir ++ " " ++ h))
    -- built against a local package: again when that changes
    _ | not (null local) ->
          pure (NotStored (unwords (depName d : "against" : [ keyStamp k | n <- local, Just k <- [lookup n planned] ])))
    (Just (LockedHackage n v h), _, _) -> stored n v (["hackage: " ++ n ++ " " ++ v ++ " " ++ h] ++ common)
    (Just (LockedGit n repo _ c v), _, _) -> stored n (maybe (take 7 c) id v) (["git: " ++ n ++ " " ++ repo ++ " " ++ c] ++ common)
    (_, Just b, _) -> do
      h <- maybe (pure "") sha256File (bSource b)
      stored (bName b) (bVersion b) (["tool-source: " ++ bName b ++ " " ++ bVersion b ++ " " ++ h] ++ common)
    (_, _, Just dir) | Just v <- depVersion d -> do
      h <- sha256Tree dir
      stored (depName d) v (["hackage-source: " ++ depName d ++ " " ++ v ++ " " ++ h] ++ common)
    _ -> pure (NotStored (show d))
  where
    stored n v keyLines = (\sid -> Stored sid keyLines) <$> storeId n v keyLines

-- | Where the source of a package is, fetched if needed: Nothing when mcabal
-- installs it by name (mcabal -r).
sourceOf :: Maybe [(String, (String, String))] -> Dep -> Maybe Locked -> IO (Maybe FilePath, Maybe String)
sourceOf vend d ml = do
  (mdir, from) <- sourceOf' vend d ml
  -- a miso project (miso.yml, no .cabal file): built from a copy with the
  -- .cabal file of its library
  case mdir of
    Just dir -> do
      dir' <- cabalized (Just (maybe (depName d) lockedName ml)) dir
      pure (Just dir', from)
    Nothing -> pure (Nothing, from)

sourceOf' :: Maybe [(String, (String, String))] -> Dep -> Maybe Locked -> IO (Maybe FilePath, Maybe String)
sourceOf' vend d ml =
  case (vend, ml) of
    (Just vs, _) | not (isLocal d) -> do
      dir <- vendoredDir vs ml d
      pure (Just dir, Just dir)
    (_, Just (LockedHackage n v h)) -> do
      tarball <- checkedTarball n v h
      dir <- unpackHackage tarball (n ++ "-" ++ v)
      pure (Just dir, Nothing)
    (_, Just (LockedGit n repo _ c _)) -> do
      dir <- checkoutAt n repo c
      pure (Just dir, Nothing)
    (_, Nothing) -> case (depGit d, depPath d, depVersion d) of
      (_, Just dir, _) -> do
        ok <- doesDirectoryExist dir
        unless ok $ failWith ("dependency " ++ depName d ++ ": no directory " ++ dir)
        pure (Just dir, Just dir)
      (Just repo, _, _) -> do
        dir <- gitDir d
        cloned <- doesDirectoryExist dir
        unless cloned $ cloneAt repo (depRev d) dir
        pure (Just dir, Nothing)
      (_, _, Just v) -> do
        -- a version of miso.yml that the lock does not have yet
        tarball <- hackageTarball (depName d) v
        dir <- unpackHackage tarball (depName d ++ "-" ++ v)
        pure (Just dir, Nothing)
      _ -> pure (Nothing, Nothing)

-- | Install a package in the project's package database ($CABALDIR): a
-- locked one or a library of the tool without what it needs (installed
-- before it); otherwise, with what it needs that is not installed (mcabal
-- -r, by name from the package list).
installUnit :: Unit -> IO ()
installUnit u = do
  let d = uDep u
  case (uLocked u, uTool u, uDir u) of
    (Just _, _, Just dir) -> mcabalInstall False d >>= \mcabal -> runLocked d ("cd " ++ quote dir ++ " && " ++ mcabal)
    (_, Just _, Just dir) -> mcabalInstall False d >>= \mcabal -> run ("cd " ++ quote dir ++ " && " ++ mcabal)
    (_, _, Just dir) -> mcabalInstall True d >>= \mcabal -> run ("cd " ++ quote dir ++ " && " ++ mcabal)
    _ -> mcabalInstall True d >>= \mcabal -> run (mcabal ++ " " ++ quote (depName d))

gitDir :: Dep -> IO FilePath
gitDir d = do
  home <- misoHome
  pure (home ++ "/src/" ++ depName d ++ "-" ++ maybe "HEAD" (map (\c -> if c == '/' then '-' else c)) (depRev d))

-- | mcabal install of a locked package, which installs nothing the package
-- needs: a miso.lock from before miso locked those lacks them.
runLocked :: Dep -> String -> IO ()
runLocked d c = do
  ok <- tryRun c
  unless ok $
    failWith ("installing " ++ depName d ++ " failed (see above); if mcabal says a dependency is not installed, "
              ++ lockFile ++ " lacks it: miso lock adds it")

-- | The tarball of a locked Hackage version, checked against its sha256.
checkedTarball :: String -> String -> String -> IO FilePath
checkedTarball n v h = do
  tarball <- hackageTarball n v
  h' <- sha256File tarball
  when (h' /= h) $ do
    run ("rm -f " ++ quote tarball)
    failWith (n ++ "-" ++ v ++ ": the sha256 of the tarball is " ++ h' ++ ", " ++ lockFile ++ " has " ++ h)
  pure tarball

-----------------------------------------------------------------------------
-- miso add / remove: edit miso.yml (keeping its comments and layout)

addDeps :: [String] -> IO ()
addDeps names = do
  cfg <- withConfig pure
  let new = [ n | n <- names, n `notElem` map depName (cfgDeps cfg) ]
  forM_ [ n | n <- names, n `notElem` new ] $ \n -> step (n ++ " is already a dependency")
  unless (null new) $ do
    s <- readFile' configFile
    let s' = foldl addToYml s new
    either (\e -> failWith ("could not edit miso.yml: " ++ e)) (const (pure ())) (parseConfig s')
    writeFile configFile s'
    forM_ new $ \n -> success ("Added " ++ n)
    withConfig installDeps

removeDeps :: [String] -> IO ()
removeDeps names = do
  cfg <- withConfig pure
  forM_ [ n | n <- names, n `notElem` map depName (cfgDeps cfg) ] $ \n -> warn (n ++ " is not a dependency")
  s <- readFile' configFile
  let s' = foldl removeFromYml s names
  either (\e -> failWith ("could not edit miso.yml: " ++ e)) (const (pure ())) (parseConfig s')
  when (s' /= s) $ do
    writeFile configFile s'
    forM_ [ n | n <- names, n `elem` map depName (cfgDeps cfg) ] $ \n -> success ("Removed " ++ n)
    step "(the packages stay in .miso/ until miso clean)"

indentOf :: String -> Int
indentOf = length . takeWhile (== ' ')

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- The line of the dependencies field, and its value after the colon.
depsLine :: [String] -> Maybe (Int, String)
depsLine ls = case [ (i, drop (length "dependencies:") l) | (i, l) <- zip [0 ..] ls, "dependencies:" `isPrefixOf` l ] of
                x : _ -> Just x
                [] -> Nothing

-- The lines of the block under line i (more indented, or blank).
blockEnd :: [String] -> Int -> Int
blockEnd ls i =
  let body = takeWhile (\l -> all isSpace l || indentOf l > 0) (drop (i + 1) ls)
      -- trailing blank lines do not belong to the block
      n = length (reverse (dropWhile (all isSpace) (reverse body)))
  in  i + 1 + n

inlineItems :: String -> Maybe [String]
inlineItems v =
  case trim (takeWhile (/= '#') v) of
    '[' : r | not (null r) && last r == ']' -> Just [ trim x | x <- splitComma (init r), not (null (trim x)) ]
    _ -> Nothing
  where splitComma s = case break (== ',') s of
                         (a, _ : b) -> a : splitComma b
                         (a, []) -> [a]

-- | Add a (Hackage) dependency to the text of miso.yml.
addToYml :: String -> String -> String
addToYml s n =
  let ls = lines s in
  case depsLine ls of
    Nothing -> unlines (ls ++ ["dependencies:", "  - " ++ n])
    Just (i, v)
      | Just items <- inlineItems v ->
          unlines (take i ls ++ ["dependencies: [" ++ commaSep (items ++ [n]) ++ "]"] ++ drop (i + 1) ls)
      | otherwise ->
          let end = blockEnd ls i
              items = [ l | l <- take (end - i - 1) (drop (i + 1) ls), "- " `isPrefixOf` dropWhile (== ' ') l ]
              ind = case items of { l : _ -> indentOf l; [] -> 2 }
          in  unlines (take end ls ++ [replicate ind ' ' ++ "- " ++ n] ++ drop end ls)
  where commaSep = foldr1 (\a b -> a ++ ", " ++ b)

-- | Remove a dependency (a name, or a "- name: ..." item) from miso.yml.
removeFromYml :: String -> String -> String
removeFromYml s n =
  let ls = lines s in
  case depsLine ls of
    Nothing -> s
    Just (i, v)
      | Just items <- inlineItems v ->
          unlines (take i ls ++ ["dependencies: [" ++ commaSep [ x | x <- items, unq x /= n ] ++ "]"] ++ drop (i + 1) ls)
      | otherwise ->
          let end = blockEnd ls i
          in  unlines (take (i + 1) ls ++ go (take (end - i - 1) (drop (i + 1) ls)) ++ drop end ls)
  where
    commaSep [] = ""
    commaSep xs = foldr1 (\a b -> a ++ ", " ++ b) xs
    unq = filter (`notElem` "\"'")
    go [] = []
    go (l : rest)
      | Just item <- itemOf l =
          let (cont, rest') = span (\x -> all isSpace x || indentOf x > indentOf l) rest
          in  if isNamed item then go rest' else l : cont ++ go rest'
      | otherwise = l : go rest
    itemOf l = case dropWhile (== ' ') l of
                 '-' : ' ' : item -> Just (trim (takeWhile (/= '#') item))
                 _ -> Nothing
    isNamed item = unq item == n || (case break (== ':') item of
                                       ("name", ':' : v) -> unq (trim v) == n
                                       _ -> False)

-----------------------------------------------------------------------------
-- miso list

-- | The packages: those that come with the tool (its compiler's, and its
-- libraries), and those the project installed in its .miso/ (which it uses
-- instead of the tool's of the same name).
listPackages :: IO ()
listPackages = do
  db <- toolDb
  ok <- doesDirectoryExist (db ++ "/packages")
  unless ok $ failWith ("no package database at " ++ db)
  tool <- bundledPackages
  mpd <- pkgDir
  installed <- case mpd of
    Just pd -> do
      has <- doesDirectoryExist (pd ++ "/packages")
      if has then sort . filter (".pkg" `isSuffixOf'`) <$> listDirectory (pd ++ "/packages") else pure []
    Nothing -> pure []
  let nameVersion p =
        let (rv, rn) = break (== '-') (reverse (take (length p - 4) p))
        in  (reverse (drop 1 rn), reverse rv)
      mine = map (fst . nameVersion) installed
      showPkg n v note = do
        n' <- colored 1 [Bold] n
        v' <- colored 1 [Dim] v
        note' <- if null note then pure "" else (" " ++) <$> colored 1 [Dim] note
        putStrLn ("  " ++ n' ++ replicate (24 - length n) ' ' ++ v' ++ note')
  info "Packages that come with miso"
  forM_ (sortBy (comparing bName) tool) $ \b ->
    showPkg (bName b) (bVersion b)
      (if bName b `elem` mine then "(replaced in this project)"
       else if bSource b == Nothing then "(part of the compiler)" else "")
  case mpd of
    Nothing -> step "(not in a project: miso list in a project shows its packages too)"
    Just pd -> do
      info ("Packages of this project (" ++ pd ++ ")")
      if null installed then colored 1 [Dim] "  (none)" >>= putStrLn
        else forM_ installed $ \p -> let (n, v) = nameVersion p in
               showPkg n v (if n `elem` map bName tool then "(instead of the one that comes with miso)" else "")

-----------------------------------------------------------------------------
-- miso update

-- | miso lock: lock the dependencies of miso.yml and the packages they
-- need (Miso.CLI.Resolve), keeping the packages miso.lock has at their
-- versions, and dropping those no longer needed.  miso update moves all of
-- them to the versions of a new package list.
lockDeps :: Config -> IO ()
lockDeps cfg0 = do
  cfg <- withMiso cfg0
  setupEnv
  old <- maybe [] id <$> readLock
  info ("Locking the dependencies in " ++ lockFile)
  new <- resolveTree True old cfg
  toolLibs <- usedToolLibraries cfg
  writeLock toolLibs new
  case describeChanges old new of
    [] -> success (lockFile ++ " is up to date")
    changes -> do
      mapM_ step changes
      success ("Wrote " ++ lockFile ++ " (miso install installs it)")

-- | The libraries of the tool the project uses (those it does not
-- replace), as name-version, for miso.lock.
usedToolLibraries :: Config -> IO [String]
usedToolLibraries cfg0 = do
  cfg <- withMiso cfg0
  tool <- bundledPackages
  replaced <- filterM (isReplacement tool) (allDeps cfg)
  pure [ bName b ++ "-" ++ bVersion b | b <- tool, bSource b /= Nothing, bName b `notElem` map depName replaced ]

-- | miso update: a new package list for mcabal (the newest Stackage
-- snapshot), and every package locked again at its version, git
-- dependencies at the commit of their rev now; then install them.
updateDeps :: Config -> IO ()
updateDeps cfg0 = do
  cfg <- withMiso cfg0
  setupEnv
  info "Updating the package list"
  withGlobalCabal (runSelf "mcabal update")
  old <- maybe [] id <$> readLock
  info ("Locking the dependencies in " ++ lockFile)
  new <- resolveTree False old cfg
  toolLibs <- usedToolLibraries cfg
  writeLock toolLibs new
  case describeChanges old new of
    [] -> success (lockFile ++ " is up to date")
    changes -> do
      mapM_ step changes
      success ("Wrote " ++ lockFile)
  installDeps cfg

-----------------------------------------------------------------------------
-- miso vendor

vendorDir :: FilePath
vendorDir = "vendor"

-- | The list of what miso vendor copied: a project with one is vendored.
vendorFile :: FilePath
vendorFile = vendorDir ++ "/.miso-vendor"

-- | What a package in vendorFile is: the source, and the version or commit.
stampOf :: Locked -> String
stampOf (LockedHackage n v h) = unwords ["hackage", n, v, h]
stampOf (LockedGit n repo _ c _) = unwords ["git", n, repo, c]

-- | The packages of vendorFile, by name: their stamp, and the sha256 of
-- their files.  Nothing if the project is not vendored.
readVendor :: IO (Maybe [(String, (String, String))])
readVendor = do
  ok <- doesFileExist vendorFile
  if not ok then pure Nothing else do
    s <- readFile' vendorFile
    pure (Just [ (n, (unwords (init ws), drop 6 (last ws)))
               | l <- lines s, take 1 l /= "#", let ws = words l
               , length ws >= 3, _ : n : _ <- [ws], "files:" `isPrefixOf` last ws ])

writeVendor :: [(String, (String, String))] -> IO ()
writeVendor vs =
  writeFile vendorFile $ unlines $
    "# vendor/.miso-vendor: written by miso vendor.  Commit it; do not edit it."
    : [ s ++ " files:" ++ h | (_, (s, h)) <- vs ]

-- | The directory of a dependency in a vendored project, checked: it must be
-- locked, and in vendor/ as miso vendor copied it.
vendoredDir :: [(String, (String, String))] -> Maybe Locked -> Dep -> IO FilePath
vendoredDir vs ml d = do
  l <- maybe (failWith (depName d ++ " is not locked in " ++ lockFile ++ ", and this project is vendored: miso vendor locks and vendors it")) pure ml
  let dir = vendorDir ++ "/" ++ lockedName l
      again = "; miso vendor copies it again"
  case lookup (lockedName l) vs of
    Nothing -> failWith (dir ++ " is not vendored (it is not in " ++ vendorFile ++ "): miso vendor vendors it")
    Just (s, h) -> do
      when (s /= stampOf l) $ failWith (dir ++ " is not the version in " ++ lockFile ++ again)
      there <- doesDirectoryExist dir
      unless there $ failWith (dir ++ " is missing" ++ again)
      h' <- sha256Tree dir
      when (h' /= h) $
        failWith (dir ++ " has changed since miso vendor copied it" ++ again ++ " (for a patched copy, use a path: dependency)")
      pure dir

-- | miso vendor: lock the dependencies (as miso lock does), and copy the
-- source of each Hackage and git dependency to vendor/NAME, listed with the
-- sha256 of its files in vendor/.miso-vendor.  miso install then installs
-- from there, without Hackage or git, and fails rather than download.
-- Packages vendored before that are no longer dependencies are removed.
-- Every package of the lock is vendored: the dependencies of miso.yml and
-- the packages they need.  Local dependencies, and the packages that come
-- with the tool or with MicroHs, are not (they are not locked).
vendorDeps :: Config -> IO ()
vendorDeps cfg0 = do
  cfg <- withMiso cfg0
  lockDeps cfg
  lock <- maybe [] id <$> readLock
  old <- maybe [] id <$> readVendor
  info ("Vendoring the dependencies in " ++ vendorDir ++ "/")
  createDirectoryIfMissing True vendorDir
  -- mcabal builds in the vendored directories; and git must not change
  -- their line endings, or their sha256 changes
  writeFile (vendorDir ++ "/.gitignore") "dist-mcabal/\n"
  writeFile (vendorDir ++ "/.gitattributes") "* -text\n"
  new <- forM lock $ \l -> do
    let dir = vendorDir ++ "/" ++ lockedName l
    kept <- case lookup (lockedName l) old of
      Just (s, h) | s == stampOf l -> do
        there <- doesDirectoryExist dir
        same <- if there then (== h) <$> sha256Tree dir else pure False
        unless same $ warn (dir ++ " has changed since miso vendor copied it; copying it again")
        pure (if same then Just h else Nothing)
      _ -> pure Nothing
    h <- case kept of
      Just h -> step (dir ++ " (kept)") >> pure h
      Nothing -> vendorOne l >> sha256Tree dir
    pure (lockedName l, (stampOf l, h))
  -- what miso vendor put there before, for dependencies that are gone
  forM_ [ n | (n, _) <- old, n `notElem` map lockedName lock ] $ \n -> do
    let dir = vendorDir ++ "/" ++ n
    there <- doesDirectoryExist dir
    when there $ run ("rm -rf " ++ quote dir)
    step ("- " ++ dir)
  writeVendor new
  if null lock
    then success "No Hackage or git dependencies to vendor"
    else success ("Vendored " ++ show (length lock) ++ " package" ++ (if length lock == 1 then "" else "s")
                  ++ " in " ++ vendorDir ++ "/ (commit it; miso install installs from it)")
  where
    vendorOne l = do
      let dir = vendorDir ++ "/" ++ lockedName l
          tmp = vendorDir ++ "/.tmp-" ++ lockedName l
      run ("rm -rf " ++ quote dir ++ " " ++ quote tmp)
      createDirectoryIfMissing True tmp
      case l of
        LockedHackage n v h -> do
          tarball <- checkedTarball n v h
          run ("tar -xzf " ++ quote tarball ++ " -C " ++ quote tmp)
          run ("mv " ++ quote (tmp ++ "/" ++ n ++ "-" ++ v) ++ " " ++ quote dir)
        LockedGit n repo _ c _ -> do
          -- the files of the commit, without .git and what builds left there
          -- (the release's miso is its source, not a checkout)
          src <- checkoutAt n repo c
          isRepo <- doesDirectoryExist (src ++ "/.git")
          if isRepo
            then run ("git -C " ++ quote src ++ " archive --format=tar " ++ quote c ++ " | tar -xf - -C " ++ quote tmp)
            else run ("cd " ++ quote src ++ " && tar -cf - --exclude=dist-mcabal . | tar -xf - -C " ++ quote tmp)
          run ("mv " ++ quote tmp ++ " " ++ quote dir)
      run ("rm -rf " ++ quote tmp)
      step ("+ " ++ dir ++ " " ++ describe l)
    describe (LockedHackage _ v _) = v
    describe (LockedGit _ _ rev c _) = maybe "" (++ " ") rev ++ "(" ++ take 7 c ++ ")"
