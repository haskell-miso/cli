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
  , runSelf
  , tryRunSelf
  , have
  , parent
  , readFile'
  , misoHome
  , cabalDir
  , pkgDir
  , toolHome
  , toolDb
  , setupCpphs
  , setupEnv
  , withConfig
  , isSuffixOf'
  ) where

import Control.Monad (when)
import Data.List (isPrefixOf)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.Environment (getExecutablePath, lookupEnv, setEnv)
import System.Exit (ExitCode (..))
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

-- | Run ourselves with the arguments.
runSelf :: String -> IO ()
runSelf as = do
  exe <- self
  run (quote exe ++ " " ++ as)

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

-- | Where miso keeps its files (git checkouts, mcabal's package lists).
misoHome :: IO FilePath
misoHome = do
  m <- lookupEnv "MISO_HOME"
  case m of
    Just d -> pure d
    Nothing -> (++ "/.miso") <$> getHomeDirectory

-- | Where mcabal keeps its files, and an extra package database for mhs.
cabalDir, pkgDir :: IO FilePath
cabalDir = (++ "/mcabal") <$> misoHome
pkgDir = (\d -> d ++ "/mhs-" ++ mhsVersion) <$> cabalDir

-- | The installation: bin/miso, and the package database next to it, with
-- the packages that come with the tool and the ones miso installs.
toolHome, toolDb :: IO FilePath
toolHome = parent . parent <$> self
toolDb = (\h -> h ++ "/mhs-" ++ mhsVersion) <$> toolHome

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

-- | Make mcabal keep its files with ours, and compile with us.
setupEnv :: IO ()
setupEnv = do
  setupCpphs
  cd <- cabalDir
  createDirectoryIfMissing True cd
  setEnv "CABALDIR" cd
  exe <- self
  setEnv "MHS" (quote exe ++ " mhs")

withConfig :: (Config -> IO a) -> IO a
withConfig k = readConfig >>= either failWith k
