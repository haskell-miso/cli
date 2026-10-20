-- | Paths, commands, and the environment of the tools.
module Miso.CLI.Util
  ( version
  , self
  , windows
  , exeSuffix
  , shell
  , quote
  , run
  , tryRun
  , runExitCode
  , readCommand
  , runSelf
  , captureSelf
  , tryRunSelf
  , have
  , parent
  , readFile'
  , misoHome
  , globalCabalDir
  , projectRoot
  , localDir
  , projectCabalDir
  , projectDb
  , pkgDir
  , packagePath
  , toolHome
  , toolDb
  , setupCpphs
  , setupEnv
  , withGlobalCabal
  , copyPackageList
  , withConfig
  , isSuffixOf'
  ) where

import Control.Monad (when)
import Data.List (isPrefixOf)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, getHomeDirectory, getTemporaryDirectory, removeFile)
import System.Environment (getExecutablePath, lookupEnv, setEnv)
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)
import System.Process (system)
import qualified System.Info as Info

import MicroHs.Compile (mhsVersion)

import Miso.CLI.Color
import Miso.CLI.Config

version :: String
version = "0.1.0"

-- | The path of the executable, with / (it goes in sh commands).
self :: IO String
self = map (\c -> if c == '\\' then '/' else c) <$> getExecutablePath

windows :: Bool
windows = Info.os == "windows"

exeSuffix :: String
exeSuffix = if windows then ".exe" else ""

-- | A command for system: on Windows system runs cmd.exe, so run sh
-- (from MSYS2) instead.
shell :: String -> String
shell c
  | windows = "sh -c \"" ++ concatMap (\x -> if x == '"' then "\\\"" else [x]) c ++ "\""
  | otherwise = c

quote :: String -> String
quote s = "'" ++ concatMap (\c -> if c == '\'' then "'\\''" else [c]) s ++ "'"

-- | Run a command; did it succeed?
tryRun :: String -> IO Bool
tryRun c = (== ExitSuccess) <$> system (shell c)

-- | Run a command, and stop if it fails.
run :: String -> IO ()
run c = do
  ok <- tryRun c
  when (not ok) $ failWith ("command failed: " ++ c)

-- | Run a command, with its exit code.  MicroHs's system returns the wait
-- status of the C system() (an exit code of 1 is 256), which exitWith
-- would turn into 0; this is the exit code (or 128 + the signal).
runExitCode :: String -> IO ExitCode
runExitCode c = do
  r <- system (shell c)
  pure $ case r of
    ExitFailure s
      | s `mod` 256 == 0 -> ExitFailure (s `div` 256)
      | otherwise -> ExitFailure (128 + s `mod` 128)
    ok -> ok

-- | Run a command and read what it prints; Nothing if it fails.
readCommand :: String -> IO (Maybe String)
readCommand c = do
  tmp <- getTemporaryDirectory
  (f, h) <- openTempFile tmp "miso.out"
  hClose h
  ok <- tryRun (c ++ " > " ++ quote (map (\x -> if x == '\\' then '/' else x) f))
  s <- readFile' f
  removeFile f
  pure (if ok then Just s else Nothing)

-- | Run ourselves with the arguments.
runSelf :: String -> IO ()
runSelf as = do
  exe <- self
  run (quote exe ++ " " ++ as)

-- | Run ourselves with the arguments; did it work, and what it printed
-- (stdout and stderr).
captureSelf :: String -> IO (Bool, String)
captureSelf as = do
  exe <- self
  tmp <- getTemporaryDirectory
  (f, h) <- openTempFile tmp "miso.out"
  hClose h
  ok <- tryRun (quote exe ++ " " ++ as ++ " > " ++ quote (map (\x -> if x == '\\' then '/' else x) f) ++ " 2>&1")
  s <- readFile' f
  removeFile f
  pure (ok, s)

tryRunSelf :: String -> IO Bool
tryRunSelf as = do
  exe <- self
  tryRun (quote exe ++ " " ++ as)

-- | Is the command on the PATH?
have :: String -> IO Bool
have c = tryRun ("command -v " ++ quote c ++ " >/dev/null 2>&1")

-- | The directory of a path.
parent :: FilePath -> FilePath
parent = reverse . drop 1 . dropWhile (`notElem` "/\\") . reverse

readFile' :: FilePath -> IO String
readFile' f = do
  s <- readFile f
  length s `seq` pure s

isSuffixOf' :: String -> String -> Bool
isSuffixOf' s x = reverse s `isPrefixOf` reverse x

-- | Where miso keeps its files (downloads, git checkouts, mcabal's package
-- list, the store of built packages).
misoHome :: IO FilePath
misoHome = do
  m <- lookupEnv "MISO_HOME"
  case m of
    Just d -> pure d
    Nothing -> (++ "/.miso") <$> getHomeDirectory

-- | Where mcabal keeps its package list (the Stackage snapshot), for every
-- project.  (It installs nothing there.)
globalCabalDir :: IO FilePath
globalCabalDir = (++ "/mcabal") <$> misoHome

-- | The project: the directory of its miso.yml, as an absolute path.
-- MISO_PROJECT is set for what miso runs (mcabal runs miso mhs in a
-- package's directory, not the project's).
projectRoot :: IO (Maybe FilePath)
projectRoot = do
  m <- lookupEnv "MISO_PROJECT"
  case m of
    Just d | not (null d) -> pure (Just d)
    _ -> do
      here <- doesFileExist configFile
      if here then Just <$> getCurrentDirectory else pure Nothing

-- | A project's own files: .miso/ next to its miso.yml.
localDir :: FilePath -> FilePath
localDir root = root ++ "/.miso"

-- | Where mcabal installs a project's packages ($CABALDIR), and the package
-- database there.
projectCabalDir, projectDb :: FilePath -> FilePath
projectCabalDir root = localDir root ++ "/mcabal"
projectDb root = projectCabalDir root ++ "/mhs-" ++ mhsVersion

-- | The package database of the project here, if there is one.
pkgDir :: IO (Maybe FilePath)
pkgDir = fmap projectDb <$> projectRoot

-- | The installation: bin/miso, and the package database next to it, with
-- the packages that come with the tool.
toolHome, toolDb :: IO FilePath
toolHome = parent . parent <$> self
toolDb = (\h -> h ++ "/mhs-" ++ mhsVersion) <$> toolHome

-- | Where mhs looks for packages: the project's database, then the tool's,
-- and nothing else (not the package path of mhs.conf).  The first one that
-- has a module wins, so a package the project installs is used instead of
-- the tool's.
packagePath :: IO [String]
packagePath = do
  mroot <- projectRoot
  tool <- toolDb
  -- a project that replaces packages of the tool has a view of the tool's
  -- database without them (Miso.CLI.Store.toolView)
  let view = fmap (\root -> localDir root ++ "/tool/mhs-" ++ mhsVersion) mroot
  hasView <- maybe (pure False) (\v -> doesDirectoryExist (v ++ "/packages")) view
  let db = if hasView then maybe tool id view else tool
  pure ("-a" : [ "-a" ++ projectDb root | Just root <- [mroot] ] ++ ["-a" ++ db])

-- | mhs runs cpphs on modules with CPP: use the one next to us.
setupCpphs :: IO ()
setupCpphs = do
  set <- lookupEnv "MHSCPPHS"
  case set of
    Just _ -> pure ()
    Nothing -> do
      exe <- self
      let cpphs = parent exe ++ "/cpphs" ++ exeSuffix
      ok <- doesFileExist cpphs
      when ok $ setEnv "MHSCPPHS" cpphs

-- | Make mcabal compile with us, and install in the project's .miso/ (with
-- a copy of the package list, which is kept in $MISO_HOME for every
-- project).  Outside a project, mcabal keeps its files in $MISO_HOME.
setupEnv :: IO ()
setupEnv = do
  setupCpphs
  exe <- self
  setEnv "MHS" (quote exe ++ " mhs")
  global <- globalCabalDir
  createDirectoryIfMissing True global
  mroot <- projectRoot
  case mroot of
    Nothing -> setEnv "CABALDIR" global
    Just root -> do
      let cd = projectCabalDir root
      createDirectoryIfMissing True (projectDb root ++ "/packages")
      setEnv "MISO_PROJECT" root
      setEnv "CABALDIR" cd
      copyPackageList

-- | Run with mcabal keeping its files in $MISO_HOME: for mcabal update, and
-- for reading the package list, which every project shares.
withGlobalCabal :: IO a -> IO a
withGlobalCabal act = do
  setupEnv
  before <- lookupEnv "CABALDIR"
  global <- globalCabalDir
  setEnv "CABALDIR" global
  r <- act
  maybe (pure ()) (setEnv "CABALDIR") before
  copyPackageList
  pure r

-- | The package list in the project's .miso/, as in $MISO_HOME (mcabal
-- reads it there for mcabal -r install).
copyPackageList :: IO ()
copyPackageList = do
  mroot <- projectRoot
  global <- globalCabalDir
  let list = global ++ "/packages.txt"
  has <- doesFileExist list
  case mroot of
    Just root | has -> do
      let local = projectCabalDir root ++ "/packages.txt"
      same <- tryRun ("cmp -s " ++ quote list ++ " " ++ quote local)
      when (not same) $ run ("cp " ++ quote list ++ " " ++ quote local)
    _ -> pure ()

withConfig :: (Config -> IO a) -> IO a
withConfig k = readConfig >>= either failWith k
