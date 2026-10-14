-- | The miso command line tool.
--
-- One executable: the MicroHs compiler (miso mhs ...) and MicroCabal
-- (miso mcabal ...) are linked in, and the project commands use them.
module Miso.CLI.Main (main) where

import Control.Exception (catch)
import Control.Monad (forM_, when)
import Foreign.C.Types (CInt (..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Environment (getArgs, lookupEnv, withArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hFlush, stderr, stdout)
import System.Process (system)

import qualified MicroCabal.Main as MCabal
import qualified MicroHs.Main as MHS
import MicroHs.Compile (mhsVersion)

import Miso.CLI.Color
import Miso.CLI.Config
import Miso.CLI.Deps
import Miso.CLI.Doctor (doctor)
import Miso.CLI.Serve (dev, serve)
import Miso.CLI.Templates (templateNames, templates)
import Miso.CLI.Util

-- Exit with the code of an ExitCode exception (from mhs, mcabal, or us);
-- not every runtime does that for an uncaught one.
foreign import ccall "exit" c_exit :: CInt -> IO ()

main :: IO ()
main = main' `catch` \e -> do
  hFlush stdout
  hFlush stderr
  c_exit (case e of { ExitSuccess -> 0; ExitFailure n -> fromIntegral n })

main' :: IO ()
main' = do
  args <- getArgs
  case args of
    "mhs" : as         -> runMhs as
    "mcabal" : as      -> runMcabal as
    ["new", name]      -> new "counter" name
    ["new", name, "--template", t] -> new t name
    ["new", "--template", t, name] -> new t name
    ["templates"]      -> listTemplates
    ["build"]          -> withConfig build >> pure ()
    "run" : as         -> withConfig (\cfg -> build cfg >>= runExe as)
    "dev" : as         -> port as >>= \p -> withConfig (dev p)
    "serve" : as       -> port as >>= serve
    ["clean"]          -> clean
    ["repl"]           -> repl
    ["install"]        -> withConfig installDeps
    "add" : ps@(_ : _) -> addDeps ps
    "remove" : ps@(_ : _) -> removeDeps ps
    ["list"]           -> listPackages
    ["update"]         -> withConfig updateDeps
    ["doctor"]         -> doctor
    ["upgrade"]        -> upgrade Nothing
    ["upgrade", v]     -> upgrade (Just v)
    ["version"]        -> printVersion
    ["--version"]      -> printVersion
    ["help"]           -> usage
    ["--help"]         -> usage
    []                 -> usage
    _                  -> usage >> failWith ("unknown command: miso " ++ unwords args)

usage :: IO ()
usage = do
  b <- colored 1 [Bold, Magenta] "miso"
  hdr <- colored 1 [Bold] "Usage:"
  let section t = colored 1 [Bold] t
      -- a command in cyan, padded to line up its description
      cmd c what = do
        c' <- colored 1 [Cyan] c
        pure ("  " ++ c' ++ replicate (25 - length c) ' ' ++ what)
  proj <- section "Project"
  deps <- section "Dependencies"
  tool <- section "The tool"
  ls <- sequence
    [ cmd "new NAME [--template T]" "make a new project (miso templates lists them)"
    , cmd "build" "build the project of miso.yml"
    , cmd "run [ARGS]" "build and run it (target: native)"
    , cmd "dev [--port N]" "serve it, rebuild on changes, and reload the page"
    , cmd "serve [--port N]" "serve public/"
    , cmd "repl" "mhs, interactively, with the project's sources"
    , cmd "clean" "remove what builds make (dist/, public/, caches)"
    ]
  ds <- sequence
    [ cmd "install" "install the dependencies of miso.yml"
    , cmd "add PKG..." "add dependencies to miso.yml, and install them"
    , cmd "remove PKG..." "remove dependencies from miso.yml"
    , cmd "list" "list the installed packages"
    , cmd "update" "refresh the package list and git dependencies"
    ]
  ts <- sequence
    [ cmd "doctor" "check the installation and the tools miso needs"
    , cmd "upgrade [VERSION]" "install the latest release (or VERSION)"
    , cmd "version" "show the version"
    , cmd "mhs ARGS" "run the MicroHs compiler"
    , cmd "mcabal ARGS" "run MicroCabal"
    ]
  foot <- colored 1 [Dim] "miso keeps its files in $MISO_HOME (default ~/.miso).  NO_COLOR=1 turns off colors."
  putStrLn $ unlines $
    [ b ++ " " ++ version ++ " - build miso apps with MicroHs " ++ mhsVersion
    , ""
    , hdr ++ " miso COMMAND"
    , ""
    , proj ] ++ ls ++ [ "", deps ] ++ ds ++ [ "", tool ] ++ ts ++ [ "", foot ]

printVersion :: IO ()
printVersion = putStrLn ("miso " ++ version ++ " (MicroHs " ++ mhsVersion ++ ")")

port :: [String] -> IO Int
port as =
  case as of
    [] -> pure 8000
    ["--port", n] | not (null n) && all (`elem` "0123456789") n -> pure (read n)
    _ -> failWith "expected --port N"

-----------------------------------------------------------------------------
-- The embedded tools

-- | mhs, also finding the packages in $MISO_HOME/mcabal.  (The packages of
-- the tool, and those miso install installs, are found next to the
-- executable.)  A bare -a clears the package path, so it is left alone.
runMhs :: [String] -> IO ()
runMhs as = do
  setupCpphs
  pd <- pkgDir
  createDirectoryIfMissing True pd
  let as' | "-a" `elem` as || ("-a" ++ pd) `elem` as = as
          | otherwise = ("-a" ++ pd) : as
  withArgs as' MHS.main

runMcabal :: [String] -> IO ()
runMcabal as = setupEnv >> withArgs as MCabal.main

-----------------------------------------------------------------------------
-- Project commands

-- | Build the project, and return the output file.  A browser target
-- builds public/index.js, next to a copy of static/.
build :: Config -> IO FilePath
build cfg = do
  installDeps cfg
  let native = maybe True (== "native") (cfgTarget cfg)
      outDir = if native then "dist" else "public"
      outFile = if native then "dist/" ++ cfgName cfg else "public/index.js"
      args = [ "-i" ++ d | d <- cfgSourceDirs cfg ]
          ++ (if native then [] else maybe [] (\t -> ["-t" ++ t]) (cfgTarget cfg))
          ++ cfgMhsOptions cfg
          ++ ["-o" ++ outFile, cfgMain cfg]
  createDirectoryIfMissing True outDir
  static <- doesDirectoryExist "static"
  when (static && not native) $ run "cp -R static/. public/"
  info ("Building " ++ cfgName cfg)
  runSelf ("mhs " ++ unwords (map quote args))
  success ("Built " ++ outFile)
  pure outFile

runExe :: [String] -> FilePath -> IO ()
runExe as exe = do
  when (".js" `isSuffixOf'` exe) $ failWith "miso run runs native programs; use miso dev or miso serve for a browser target"
  r <- system (shell (unwords (map quote (("./" ++ exe) : as))))
  exitWith r

clean :: IO ()
clean = do
  let dirs = ["dist", "public", ".mhscache", "dist-mcabal"]
  there <- filterM' doesDirectoryExist dirs
  if null there
    then step "nothing to clean"
    else do
      run ("rm -rf " ++ unwords (map quote there))
      success ("Removed " ++ unwords there)
  where filterM' p = fmap concat . mapM (\x -> (\b -> [ x | b ]) <$> p x)

-- | mhs, interactively, with the source dirs and options of miso.yml.
repl :: IO ()
repl = do
  isProject <- doesFileExist configFile
  args <- if isProject
            then withConfig $ \cfg -> pure ([ "-i" ++ d | d <- cfgSourceDirs cfg ] ++ cfgMhsOptions cfg)
            else pure []
  when isProject $ withConfig installDeps
  runMhs (args ++ ["--interactive"])

-- | Install a new release where this one is: run install.sh (the same
-- download and checks as a first install) with MISO_HOME set to our
-- installation.  Packages from miso install are installed again by the
-- next miso build, against the new package database.
upgrade :: Maybe String -> IO ()
upgrade mv = do
  home <- toolHome
  url <- maybe "https://haskell-miso.org/install.sh" id <$> lookupEnv "MISO_INSTALL_URL"
  info ("Upgrading miso " ++ version ++ " in " ++ home)
  run ("curl -fsSL " ++ quote url ++ " | MISO_HOME=" ++ quote home
       ++ maybe "" (\v -> " MISO_VERSION=" ++ quote v) mv ++ " sh")

-- | A new project from a template.
new :: String -> String -> IO ()
new template name = do
  files <- maybe (listTemplates >> failWith ("no template " ++ template)) pure (templates template name)
  exists <- doesDirectoryExist name
  when exists $ failWith (name ++ " already exists")
  info ("Creating " ++ name ++ " (" ++ template ++ ")")
  forM_ files $ \(path, contents) -> do
    let file = name ++ "/" ++ path
    createDirectoryIfMissing True (parent file)
    writeFile file contents
    step file
  success ("Created " ++ name ++ ": cd " ++ name ++ " && miso dev")

listTemplates :: IO ()
listTemplates = do
  info "Templates (miso new NAME --template T)"
  forM_ templateNames $ \(t, what) -> do
    t' <- colored 1 [Cyan] t
    putStrLn ("  " ++ t' ++ replicate (10 - length t) ' ' ++ what)
