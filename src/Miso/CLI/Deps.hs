-- | Dependencies: installing them, and miso add/remove/list/update.
module Miso.CLI.Deps
  ( installDeps
  , addDeps
  , removeDeps
  , listPackages
  , updateDeps
  , addToYml
  , removeFromYml
  ) where

import Control.Monad (forM_, unless, when)
import Data.Char (isSpace)
import Data.List (isPrefixOf, sort)
import Data.Maybe (isJust)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)

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
-- commits it has.
installDeps :: Config -> IO ()
installDeps cfg = do
  setupEnv
  lock <- readLock
  bundled <- bundledNames
  let locked d = maybe Nothing (\l -> lockedFor l d) lock
  case lock of
    Nothing -> pure ()
    Just _ ->
      forM_ [ d | d <- cfgDeps cfg, not (isLocal d), depName d `notElem` bundled, locked d == Nothing ] $ \d ->
        warn (depName d ++ " is not locked in " ++ lockFile ++ " (new, or changed in miso.yml); miso update locks it")
  done <- readStamp
  stamp <- stampFile
  -- the stamp of a locked dependency has its version or commit: a new lock installs it again
  let key d = show d ++ maybe "" (\l -> " " ++ show l) (locked d)
      todo = [ d | d <- cfgDeps cfg, isLocal d || key d `notElem` done ]
  if null todo
    then step "dependencies are installed"
    else forM_ todo $ \d -> do
      info ("Installing " ++ depName d ++ maybe "" (" from " ++) (depPath d) ++ maybe "" lockedAt (locked d))
      installDep (locked d) d
      unless (isLocal d) $ appendFile stamp (key d ++ "\n")
  where
    lockedAt (LockedHackage _ v _) = " " ++ v
    lockedAt (LockedGit _ _ _ c) = " at " ++ take 7 c

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
  tarball <- hackageTarball n v
  h' <- sha256File tarball
  when (h' /= h) $ do
    run ("rm -f " ++ quote tarball)
    failWith (n ++ "-" ++ v ++ ": the sha256 of the tarball is " ++ h' ++ ", " ++ lockFile ++ " has " ++ h)
  let dir = home ++ "/src/hackage/" ++ n ++ "-" ++ v
  unpacked <- doesDirectoryExist dir
  unless unpacked $ run ("tar -xzf " ++ quote tarball ++ " -C " ++ quote (home ++ "/src/hackage"))
  mcabal <- mcabalInstall d
  run ("cd " ++ quote dir ++ " && " ++ mcabal)
installDep (Just (LockedGit n repo _ c)) d = do
  -- the commit of the lock
  home <- misoHome
  let dir = home ++ "/src/" ++ n ++ "-" ++ c
  cloned <- doesDirectoryExist dir
  unless cloned $ do
    step ("cloning " ++ repo)
    run ("git clone --quiet " ++ quote repo ++ " " ++ quote dir)
    run ("git -C " ++ quote dir ++ " checkout --quiet " ++ quote c)
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
updateDeps :: Config -> IO ()
updateDeps cfg = do
  setupEnv
  -- mcabal installs the dependencies of the dependencies from its package list
  info "Updating the package list"
  runSelf "mcabal update"
  old <- maybe [] id <$> readLock
  bundled <- bundledNames
  info ("Locking the dependencies in " ++ lockFile)
  new <- fmap concat $ mapM (lockDep old bundled) (cfgDeps cfg)
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
