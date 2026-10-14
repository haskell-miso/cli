-- | The miso command line tool.
--
-- One executable: the MicroHs compiler (miso mhs ...) and MicroCabal
-- (miso mcabal ...) are linked in, and the project commands use them.
module Miso.CLI.Main (main) where

import Control.Exception (AsyncException (..), catch, throwIO)
import Control.Monad (forM, forM_, unless, when)
import Data.List (intercalate)
import Foreign.C.Types (CInt (..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs, lookupEnv, withArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hFlush, stderr, stdout)
import System.Process (system)

import qualified MicroCabal.Main as MCabal
import qualified MicroHs.Main as MHS
import MicroHs.Compile (mhsVersion)

import Miso.CLI.Build
import Miso.CLI.Color
import Miso.CLI.Config
import Miso.CLI.Deps
import Miso.CLI.Doctor (doctor)
import Miso.CLI.Serve (dev, serve, watch)
import Miso.CLI.Templates (templateNames, templates)
import Miso.CLI.Util

-- Exit with the code of an ExitCode exception (from mhs, mcabal, or us);
-- not every runtime does that for an uncaught one.
foreign import ccall "exit" c_exit :: CInt -> IO ()
-- cbits/serve.c
foreign import ccall "serve.h miso_line_buffered" c_lineBuffered :: IO ()

main :: IO ()
main = (main' `catch` \e -> do
  hFlush stdout
  hFlush stderr
  c_exit (case e of { ExitSuccess -> 0; ExitFailure n -> fromIntegral n }))
  -- Ctrl-C (miso watch waits in Haskell, where it arrives as an exception)
  `catch` \e -> case e of
    UserInterrupt -> putStrLn "" >> hFlush stdout >> c_exit 130
    _ -> throwIO e

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
    "build" : as       -> withConfig (build ("--dev" `elem` as) (filter (/= "--dev") as))
    "run" : as         -> withConfig (runProgram as)
    "dev" : as         -> let (n, as') = named as in port as' >>= \p -> withConfig (dev p n)
    ["watch"]          -> withConfig (watch Nothing)
    ["watch", n]       -> withConfig (watch (Just n))
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
    , cmd "build [NAME...]" "build the project of miso.yml (or the programs named)"
    , cmd "run [NAME] [ARGS]" "build and run a program (target: native)"
    , cmd "dev [NAME] [--port N]" "serve a program, rebuild on changes, and reload the page"
    , cmd "serve [--port N]" "serve public/"
    , cmd "watch [NAME]" "type check on every change, and show the first error"
    , cmd "repl" "mhs, interactively, with the project's sources"
    , cmd "clean" "remove what builds make (dist/, public/, caches)"
    ]
  ds <- sequence
    [ cmd "install" "install the dependencies of miso.yml"
    , cmd "add PKG..." "add dependencies to miso.yml, and install them"
    , cmd "remove PKG..." "remove dependencies from miso.yml"
    , cmd "list" "list the installed packages"
    , cmd "update" "lock Hackage and git dependencies in miso.lock, and install them"
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

-- | A program's name, before or after the other arguments.
named :: [String] -> (Maybe String, [String])
named as =
  case as of
    n : rest | take 1 n /= "-" -> (Just n, rest)
    ["--port", p, n] -> (Just n, ["--port", p])
    _ -> (Nothing, as)

-- | --port N, or Nothing: the first free port from 8000.
port :: [String] -> IO (Maybe Int)
port as =
  case as of
    [] -> pure Nothing
    ["--port", n] | not (null n) && all (`elem` "0123456789") n -> pure (Just (read n))
    _ -> failWith "expected --port N"

-----------------------------------------------------------------------------
-- The embedded tools

-- | mhs, also finding the packages in $MISO_HOME/mcabal.  (The packages of
-- the tool, and those miso install installs, are found next to the
-- executable.)  A bare -a clears the package path, so it is left alone.
runMhs :: [String] -> IO ()
runMhs as = do
  c_lineBuffered
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

-- | Build the programs named, or the whole project: check the library
-- (with mhs -fno-code, as a library alone makes no program) and build every
-- program.
build :: Bool -> [String] -> Config -> IO ()
build devBuild names cfg = do
  exes <- mapM (either failWith pure . findProgram cfg) names
  installDeps cfg
  when (null names) $ maybe (pure ()) (checkLibrary cfg) (cfgLibrary cfg)
  forM_ (if null names then cfgExecutables cfg else exes) (buildProgram devBuild cfg)

-- | miso run [NAME] [--] [ARGS]: build a native program (the one named, or
-- the first) and run it.
runProgram :: [String] -> Config -> IO ()
runProgram as cfg = do
  (exe, args) <- case as of
    n : rest | Right e <- findProgram cfg n -> pure (e, rest)
    _ -> case filter isNative (cfgExecutables cfg) of
           e : _ -> pure (e, as)
           [] | null (cfgExecutables cfg) -> failWith (noProgram cfg "nothing to run")
              | otherwise -> failWith "miso run runs native programs; use miso dev or miso serve for a browser target"
  unless (isNative exe) $ failWith (exeName exe ++ " is for a browser; miso run runs native programs (miso dev serves it)")
  installDeps cfg
  out <- buildProgram False cfg exe
  r <- system (shell (unwords (map quote (("./" ++ out) : dropDashes args))))
  exitWith r
  where dropDashes xs = case xs of { "--" : r -> r; _ -> xs }

clean :: IO ()
clean = do
  let dirs = ["dist", "public", ".mhscache", "dist-mcabal", ".miso-build.log"]
  there <- filterM' (\p -> (||) <$> doesDirectoryExist p <*> doesFileExist p) dirs
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
            then withConfig $ \cfg -> pure ([ "-i" ++ d | d <- allSourceDirs cfg ] ++ cfgMhsOptions cfg ++ maybe [] libMhsOptions (cfgLibrary cfg))
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
