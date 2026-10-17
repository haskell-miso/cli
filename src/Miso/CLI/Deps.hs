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

import Control.Monad (forM, forM_, unless, when)
import Data.Char (isSpace)
import Data.List (isPrefixOf, sort)
import Data.Maybe (isJust)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)

import Miso.CLI.Color
import Miso.CLI.Config
import Miso.CLI.Lock
import Miso.CLI.Util

-- | The record of what miso install installed, kept in the package
-- database (a new release replaces both).
stampFile :: IO FilePath
stampFile = (++ "/miso-installed") <$> toolDb

readStamp :: IO [String]
readStamp = do
  f <- stampFile
  ok <- doesFileExist f
  if ok then lines <$> readFile' f else pure []

-- | mcabal install, with the cabal flags of a dependency.
mcabalInstall :: Dep -> IO String
mcabalInstall d = do
  exe <- self
  pure (quote exe ++ " mcabal" ++ maybe "" (\f -> " -f" ++ quote f) (depFlags d) ++ " install")

-- | Install the dependencies that are not installed yet.  A local checkout
-- is installed on every build, as it changes; the others once.  With a
-- miso.lock, Hackage and git dependencies are installed at the versions and
-- commits it has.  In a vendored project (vendor/.miso-vendor) they are
-- installed from vendor/, and one that is not there as miso vendor copied
-- it is an error, not a download.
installDeps :: Config -> IO ()
installDeps cfg = do
  setupEnv
  lock <- readLock
  bundled <- bundledNames
  vend <- readVendor
  let locked d = maybe Nothing (\l -> lockedFor l d) lock
  -- (in a vendored project that is an error, below)
  case lock of
    Just _ | vend == Nothing ->
      forM_ [ d | d <- allDeps cfg, not (isLocal d), depName d `notElem` bundled, locked d == Nothing ] $ \d ->
        warn (depName d ++ " is not locked in " ++ lockFile ++ " (new, or changed in miso.yml); miso update locks it")
    _ -> pure ()
  done <- readStamp
  stamp <- stampFile
  -- the stamp of a locked dependency has its version or commit: a new lock installs it again
  let key d = show d ++ maybe "" (\l -> " " ++ show l) (locked d)
      todo = [ d | d <- allDeps cfg, isLocal d || key d `notElem` done ]
  if null todo
    then step "dependencies are installed"
    else forM_ todo $ \d -> do
      v <- case vend of
        Just vs | not (isLocal d), depName d `notElem` bundled -> Just <$> vendoredDir vs (locked d) d
        _ -> pure Nothing
      info ("Installing " ++ depName d ++ maybe "" (" from " ++) (maybe (depPath d) Just v) ++ maybe "" lockedAt (locked d))
      case v of
        Just dir -> mcabalInstall d >>= \mcabal -> run ("cd " ++ quote dir ++ " && " ++ mcabal)
        Nothing -> installDep (locked d) d
      unless (isLocal d) $ appendFile stamp (key d ++ "\n")
  where
    lockedAt (LockedHackage _ v _) = " " ++ v
    lockedAt (LockedGit _ _ _ c v) = " at " ++ take 7 c ++ maybe "" (\x -> " (" ++ x ++ ")") v

-- | The names of the packages that come with the tool.
bundledNames :: IO [String]
bundledNames = do
  db <- toolDb
  let listed = db ++ "/miso-bundled"
      dir = db ++ "/packages"
  hasList <- doesFileExist listed
  hasDir <- doesDirectoryExist dir
  pkgs <- if hasList then lines <$> readFile' listed
          else if hasDir then filter (".pkg" `isSuffixOf'`) <$> listDirectory dir
          else pure []
  pure [ nameOf p | p <- pkgs, ".pkg" `isSuffixOf'` p ]
  where
    -- name-1.2.3.pkg
    nameOf p = reverse (drop 1 (dropWhile (/= '-') (reverse (take (length p - 4) p))))

gitDir :: Dep -> IO FilePath
gitDir d = do
  home <- misoHome
  pure (home ++ "/src/" ++ depName d ++ "-" ++ maybe "HEAD" (map (\c -> if c == '/' then '-' else c)) (depRev d))

installDep :: Maybe Locked -> Dep -> IO ()
installDep (Just (LockedHackage n v h)) d = do
  -- the version of the lock, from Hackage, checked against its sha256
  home <- misoHome
  tarball <- checkedTarball n v h
  let dir = home ++ "/src/hackage/" ++ n ++ "-" ++ v
  unpacked <- doesDirectoryExist dir
  unless unpacked $ run ("tar -xzf " ++ quote tarball ++ " -C " ++ quote (home ++ "/src/hackage"))
  mcabal <- mcabalInstall d
  run ("cd " ++ quote dir ++ " && " ++ mcabal)
installDep (Just (LockedGit n repo _ c _)) d = do
  -- the commit of the lock
  dir <- gitCheckout n repo c
  mcabal <- mcabalInstall d
  run ("cd " ++ quote dir ++ " && " ++ mcabal)
installDep Nothing d = do
  mcabal <- mcabalInstall d
  case (depGit d, depPath d) of
    (_, Just dir) -> do
      ok <- doesDirectoryExist dir
      unless ok $ failWith ("dependency " ++ depName d ++ ": no directory " ++ dir)
      run ("cd " ++ quote dir ++ " && " ++ mcabal)
    (Nothing, Nothing) -> run (mcabal ++ " " ++ quote (depName d))
    (Just repo, Nothing) -> do
      dir <- gitDir d
      cloned <- doesDirectoryExist dir
      unless cloned $ do
        step ("cloning " ++ repo)
        run ("git clone --quiet " ++ quote repo ++ " " ++ quote dir)
        maybe (pure ()) (\r -> run ("git -C " ++ quote dir ++ " checkout --quiet " ++ quote r)) (depRev d)
      run ("cd " ++ quote dir ++ " && " ++ mcabal)

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
    step "(the packages stay installed, other projects may use them)"

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

-- | The packages: those that come with the tool, and those miso installed.
listPackages :: IO ()
listPackages = do
  db <- toolDb
  let dir = db ++ "/packages"
  ok <- doesDirectoryExist dir
  unless ok $ failWith ("no package database at " ++ db)
  pkgs <- sort . filter (".pkg" `isSuffixOf'`) <$> listDirectory dir
  bundledFile <- pure (db ++ "/miso-bundled")
  hasBundled <- doesFileExist bundledFile
  bundled <- if hasBundled then lines <$> readFile' bundledFile else pure pkgs
  let (mine, user) = (filter (`elem` bundled) pkgs, filter (`notElem` bundled) pkgs)
      showPkg p = do
        let nv = take (length p - 4) p
            (rv, rn) = break (== '-') (reverse nv)
            n = reverse (drop 1 rn)
        n' <- colored 1 [Bold] n
        v' <- colored 1 [Dim] (reverse rv)
        putStrLn ("  " ++ n' ++ replicate (24 - length n) ' ' ++ v')
  info "Packages that come with miso"
  mapM_ showPkg mine
  info "Packages installed by miso install"
  if null user then colored 1 [Dim] "  (none)" >>= putStrLn else mapM_ showPkg user

-----------------------------------------------------------------------------
-- miso update

-- | Lock the Hackage and git dependencies in miso.lock: Hackage packages at
-- their newest version (with the sha256 of the tarball), git dependencies at
-- the commit their rev (or HEAD) is now.  Local dependencies, and the packages
-- that come with the tool, are not locked.  Then install what changed.
-- | miso lock: lock the dependencies of miso.yml that are not locked yet
-- (new, or changed in miso.yml), keeping the entries that still match, and
-- drop the entries of dependencies that are gone.  miso update moves all of
-- them to the newest versions.
lockDeps :: Config -> IO ()
lockDeps cfg = do
  setupEnv
  old <- maybe [] id <$> readLock
  bundled <- bundledNames
  info ("Locking the dependencies in " ++ lockFile)
  new <- fmap concat $ forM (allDeps cfg) $ \d ->
    if isLocal d || (isHackage d && depName d `elem` bundled)
      then pure []
      else case lockedFor old d of
        Just l -> pure [l]
        Nothing -> (: []) <$> resolveDep old d
  writeLock new
  case describeChanges old new of
    [] -> success (lockFile ++ " is up to date")
    changes -> do
      mapM_ step changes
      success ("Wrote " ++ lockFile ++ " (miso install installs it)")

updateDeps :: Config -> IO ()
updateDeps cfg = do
  setupEnv
  -- mcabal installs the dependencies of the dependencies from its package list
  info "Updating the package list"
  runSelf "mcabal update"
  old <- maybe [] id <$> readLock
  bundled <- bundledNames
  info ("Locking the dependencies in " ++ lockFile)
  new <- fmap concat $ mapM (lockDep old bundled) (allDeps cfg)
  writeLock new
  case describeChanges old new of
    [] -> success (lockFile ++ " is up to date")
    changes -> do
      mapM_ step changes
      success ("Wrote " ++ lockFile)
  installDeps cfg
  where
    lockDep old bundled d
      | isLocal d = do
          step (depName d ++ ": local, not locked")
          pure []
      | isHackage d && depName d `elem` bundled = do
          step (depName d ++ ": comes with miso, not locked")
          pure []
      | otherwise = (: []) <$> resolveDep old d

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
  let dir = vendorDir ++ "/" ++ depName d
      again = "; miso vendor copies it again"
  l <- maybe (failWith (depName d ++ " is not locked in " ++ lockFile ++ ", and this project is vendored: miso vendor locks and vendors it")) pure ml
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
-- Local dependencies, and the packages that come with the tool, are not
-- vendored (they are not locked); nor are the packages the dependencies
-- depend on, which mcabal still installs from its package list.
vendorDeps :: Config -> IO ()
vendorDeps cfg = do
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
          src <- gitCheckout n repo c
          run ("git -C " ++ quote src ++ " archive --format=tar " ++ quote c ++ " | tar -xf - -C " ++ quote tmp)
          run ("mv " ++ quote tmp ++ " " ++ quote dir)
      run ("rm -rf " ++ quote tmp)
      step ("+ " ++ dir ++ " " ++ describe l)
    describe (LockedHackage _ v _) = v
    describe (LockedGit _ _ rev c _) = maybe "" (++ " ") rev ++ "(" ++ take 7 c ++ ")"
