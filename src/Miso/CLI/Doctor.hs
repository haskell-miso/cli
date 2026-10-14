-- | miso doctor: check what miso needs, and say how to fix what is missing.
module Miso.CLI.Doctor (doctor) where

import Control.Monad (when)
import Data.IORef
import Data.List (isPrefixOf)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)


import MicroHs.Compile (mhsVersion)

import Miso.CLI.Color
import Miso.CLI.Config
import Miso.CLI.Util

doctor :: IO ()
doctor = do
  problems <- newIORef (0 :: Int)
  let ok msg = do { t <- colored 1 [Bold, Green] "✓"; putStrLn ("  " ++ t ++ " " ++ msg) }
      bad msg fix = do
        t <- colored 1 [Bold, Red] "✗"
        f <- colored 1 [Dim] fix
        putStrLn ("  " ++ t ++ " " ++ msg ++ "\n      " ++ f)
        modifyIORef problems (+ 1)
      note msg = do { t <- colored 1 [Bold, Yellow] "!"; putStrLn ("  " ++ t ++ " " ++ msg) }
      check cond msg fix = if cond then ok msg else bad msg fix

  info ("miso " ++ version ++ " (MicroHs " ++ mhsVersion ++ ")")
  home <- toolHome
  db <- toolDb
  exe <- self

  info "The installation"
  dbOk <- doesDirectoryExist (db ++ "/packages")
  check dbOk ("package database " ++ db) "reinstall: miso upgrade (or install.sh)"
  pkgs <- if dbOk then listDirectory (db ++ "/packages") else pure []
  let hasPkg p = any ((p ++ "-") `isPrefixOf`) pkgs
  check (hasPkg "base") "base" "the package database is damaged; reinstall: miso upgrade"
  check (hasPkg "miso") "miso" "the package database is damaged; reinstall: miso upgrade"
  cpphs <- doesFileExist (parent exe ++ "/cpphs" ++ exeSuffix)
  check cpphs "cpphs (for modules with CPP)" ("missing next to " ++ exe ++ "; reinstall: miso upgrade")
  mhome <- misoHome
  createDirectoryIfMissing True mhome
  writable <- tryRun ("test -w " ++ quote mhome ++ " && test -w " ++ quote home)
  check writable ("writable: " ++ mhome) ("miso installs packages there; fix its permissions")

  info "Tools"
  let cc = if windows then "gcc" else "cc"
  hasCc <- have cc
  check hasCc (cc ++ " (mhs compiles to C)")
    (if windows then "pacman -S mingw-w64-x86_64-gcc"
     else "install the Xcode command line tools (xcode-select --install) or gcc/clang")
  hasCurl <- have "curl"
  check hasCurl "curl (downloads packages)" "install curl"
  hasTar <- have "tar"
  check hasTar "tar (unpacks packages)" "install tar"
  hasGit <- have "git"
  if hasGit then ok "git (for git dependencies)" else note "git is not installed (needed for git dependencies)"
  when windows $ do
    hasSh <- have "sh"
    check hasSh "sh (MSYS2)" "run miso in an MSYS2 shell"

  isProject <- doesFileExist configFile
  when isProject $ do
    info "This project"
    r <- readConfig
    case r of
      Left e -> bad "miso.yml" e
      Right cfg -> do
        ok ("miso.yml (" ++ cfgName cfg ++ " " ++ cfgVersion cfg ++ ")")
        dirs <- mapM doesDirectoryExist (cfgSourceDirs cfg)
        check (and dirs) ("source dirs: " ++ unwords (cfgSourceDirs cfg)) "a source dir of miso.yml does not exist"
        let browser = maybe False (`elem` ["browser", "browser_js"]) (cfgTarget cfg)
        when browser $ do
          hasEmcc <- have "emcc"
          check hasEmcc "emcc (target: browser)" "install emscripten: https://emscripten.org/docs/getting_started/downloads.html"
        locals <- mapM (\d -> maybe (pure True) doesDirectoryExist (depPath d)) (cfgDeps cfg)
        check (and locals) "local dependencies" "a path: dependency of miso.yml does not exist"
        when (any ((/= Nothing) . depGit) (cfgDeps cfg)) $
          check hasGit "git (git dependencies)" "install git"

  n <- readIORef problems
  if n == 0
    then success "Everything is fine"
    else do
      failWith (show n ++ " problem" ++ (if n == 1 then "" else "s") ++ " found")
