-- | miso doctor: check what miso needs, and say how to fix what is missing.
module Miso.CLI.Doctor (doctor) where

import Control.Monad (filterM, forM_, when)
import Data.IORef
import Data.List (isPrefixOf, nub)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.IO (hFlush, stdout)


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
  -- miso: prebuilt next to the packages (miso-prebuilt), or among them (a
  -- release from before projects depended on miso)
  hasDefault <- doesFileExist (db ++ "/miso-default")
  prebuilt <- if hasDefault then doesDirectoryExist (db ++ "/miso-prebuilt/packages") else pure False
  check (prebuilt || hasPkg "miso") "miso" "the package database is damaged; reinstall: miso upgrade"
  cpphs <- doesFileExist (parent exe ++ "/cpphs" ++ exeSuffix)
  check cpphs "cpphs (for modules with CPP)" ("missing next to " ++ exe ++ "; reinstall: miso upgrade")
  mhome <- misoHome
  createDirectoryIfMissing True mhome
  writable <- tryRun ("test -w " ++ quote mhome ++ " && test -w " ++ quote home)
  check writable ("writable: " ++ mhome) ("miso keeps downloads and built packages there; fix its permissions")
  -- packages installed where every project shared them, before each had its own .miso/
  olds <- filterM (\d -> (\fs -> any (".pkg" `isSuffixOf'`) fs) <$> listDirectory (mhome ++ "/mcabal/" ++ d ++ "/packages"))
            =<< (do e <- doesDirectoryExist (mhome ++ "/mcabal")
                    if e then filterM (\d -> doesDirectoryExist (mhome ++ "/mcabal/" ++ d ++ "/packages"))
                                 . filter ("mhs-" `isPrefixOf`) =<< listDirectory (mhome ++ "/mcabal")
                         else pure [])
  forM_ olds $ \d ->
    note (mhome ++ "/mcabal/" ++ d ++ " has packages from before each project had its own .miso/; miso no longer uses them (rm -rf it)")

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
  hasSha <- (||) <$> have "sha256sum" <*> have "shasum"
  check hasSha "sha256sum or shasum (checks downloads against miso.lock)" "install coreutils (sha256sum) or perl (shasum)"
  hasHlint <- have "hlint"
  if hasHlint then ok "hlint (for miso lint)" else note "hlint is not installed (needed for miso lint)"
  hasGit <- have "git"
  if hasGit then ok "git (for git dependencies)" else note "git is not installed (needed for git dependencies)"
  hasEmcc <- have "emcc"
  if hasEmcc then ok "emcc (for the browser targets)" else note ("emcc is not installed (needed for the browser targets): " ++ emsdk)
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
        forM_ (maybe [] (: []) (cfgLibrary cfg)) $ \lib ->
          ok ("library (" ++ unwords (libSourceDirs lib) ++ ")")
        forM_ (cfgExecutables cfg) $ \e ->
          ok ("program " ++ exeName e ++ " (" ++ exeMain e ++ " in " ++ unwords (exeSourceDirs e) ++ ", target " ++ maybe "native" id (exeTarget e) ++ ")")
        forM_ (cfgTests cfg) $ \t ->
          ok ("test " ++ exeName t ++ " (" ++ exeMain t ++ " in " ++ unwords (exeSourceDirs t) ++ ")")
        dirs <- mapM doesDirectoryExist (allSourceDirs cfg)
        check (and dirs) ("source dirs: " ++ unwords (allSourceDirs cfg)) "a source dir of miso.yml does not exist"
        forM_ (nub [ t | Just t <- map exeTarget (cfgExecutables cfg), t `elem` emccTargets ]) $ \t ->
          check hasEmcc ("emcc (target: " ++ t ++ ")") ("install emscripten: " ++ emsdk)
        mdb <- pkgDir
        case mdb of
          Just pd -> do
            has <- doesDirectoryExist (pd ++ "/packages")
            n <- if has then length . filter (".pkg" `isSuffixOf'`) <$> listDirectory (pd ++ "/packages") else pure 0
            ok (".miso/: " ++ show n ++ " package" ++ (if n == 1 then "" else "s") ++ " installed (miso list shows them)")
          Nothing -> pure ()
        locals <- mapM (\d -> maybe (pure True) doesDirectoryExist (depPath d)) (allDeps cfg)
        check (and locals) "local dependencies" "a path: dependency of miso.yml does not exist"
        when (any ((/= Nothing) . depGit) (allDeps cfg)) $
          check hasGit "git (git dependencies)" "install git"

  n <- readIORef problems
  hFlush stdout   -- the checks before the error, when stdout is not a terminal
  if n == 0
    then success "Everything is fine"
    else do
      failWith (show n ++ " problem" ++ (if n == 1 then "" else "s") ++ " found")

-- | The mhs targets that compile with emcc (see mhs.conf).
emccTargets :: [String]
emccTargets = ["browser", "browser_js", "browser_rts", "emscripten"]

emsdk :: String
emsdk = "https://emscripten.org/docs/getting_started/downloads.html"
