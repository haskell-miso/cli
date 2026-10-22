-- | The miso command line tool.
--
-- One executable: the MicroHs compiler (miso mhs ...) and MicroCabal
-- (miso mcabal ...) are linked in, and the project commands use them.
module Miso.CLI.Main (main) where

import Control.Exception (AsyncException (..), catch, throwIO)
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (intercalate, isInfixOf, isPrefixOf)
import Foreign.C.Types (CInt (..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, getHomeDirectory, getTemporaryDirectory, listDirectory, removeFile)
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
import Miso.CLI.Templates (licenseText, templateNames, templates)
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
    "new" : _          -> failWith "usage: miso new NAME [--template T] (miso templates lists the templates)"
    "init" : as        -> initProject as
    ["templates"]      -> listTemplates
    "build" : as       -> withConfig (build ("--dev" `elem` as) (filter (/= "--dev") as))
    "run" : as         -> withConfig (runProgram as)
    "test" : as        -> withConfig (test as)
    "dev" : as         -> let (n, as') = named as in port as' >>= \p -> withConfig (dev p n)
    ["watch"]          -> withConfig (watch Nothing)
    ["watch", n]       -> withConfig (watch (Just n))
    "serve" : as       -> port as >>= serve
    ["clean"]          -> clean
    "lint" : as        -> withConfig (lint as)
    ["repl"]           -> repl
    ["install"]        -> withConfig installDeps
    "add" : ps@(_ : _) -> addDeps ps
    "remove" : ps@(_ : _) -> removeDeps ps
    ["list"]           -> listPackages
    ["lock"]           -> withConfig lockDeps
    ["update"]         -> withConfig updateDeps
    ["vendor"]         -> withConfig vendorDeps
    ["doctor"]         -> doctor
    "uninstall" : as   -> uninstall as
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
    , cmd "init" "make this directory a project (asks for its name, license, ...)"
    , cmd "build [NAME...]" "build the project of miso.yml (or the programs named)"
    , cmd "run [NAME] [ARGS]" "build and run a program (target: native)"
    , cmd "test [NAME...] [-- ARGS]" "build and run the tests (or those named)"
    , cmd "dev [NAME] [--port N]" "serve a program, rebuild on changes, and reload the page"
    , cmd "serve [--port N]" "serve public/"
    , cmd "watch [NAME]" "type check on every change, and show the first error"
    , cmd "lint [ARGS]" "run hlint on the project's sources (.hlint.yaml configures it)"
    , cmd "repl" "mhs, interactively, with the project's sources"
    , cmd "clean" "remove what builds make (dist/, public/, .miso/, caches)"
    ]
  ds <- sequence
    [ cmd "install" "install the dependencies of miso.yml (in .miso/)"
    , cmd "add PKG..." "add dependencies to miso.yml, and install them"
    , cmd "remove PKG..." "remove dependencies from miso.yml"
    , cmd "list" "list the packages: the tool's, and the project's"
    , cmd "lock" "write miso.lock, keeping the versions locked already"
    , cmd "update" "lock Hackage and git dependencies in miso.lock, and install them"
    , cmd "vendor" "copy the locked dependencies' sources to vendor/, to build from"
    ]
  ts <- sequence
    [ cmd "doctor" "check the installation and the tools miso needs"
    , cmd "upgrade [VERSION]" "install the latest release (or VERSION)"
    , cmd "uninstall [--yes]" "remove miso, its packages and caches (asks first)"
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

-- | mhs, finding the packages of the project (in its .miso/) and then the
-- tool's (Miso.CLI.Util.packagePath).  Other -a arguments add to that (mcabal
-- adds -a.); a bare -a clears the package path, so it is left alone.
runMhs :: [String] -> IO ()
runMhs as = do
  c_lineBuffered
  setupCpphs
  path <- packagePath
  withArgs (if "-a" `elem` as then as else path ++ as) MHS.main

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
  r <- runExitCode (unwords (map quote (("./" ++ out) : dropDashes args)))
  exitWith r
  where dropDashes xs = case xs of { "--" : r -> r; _ -> xs }

-- | miso test [NAME...] [-- ARGS]: build the tests of miso.yml (or those
-- named) and run each, with ARGS.  A test passes when it exits with 0.
-- Every test runs, and miso test fails when one of them did.
test :: [String] -> Config -> IO ()
test as cfg = do
  let (names, rest) = break (== "--") as
      args = drop 1 rest
  when (null (cfgTests cfg)) $
    failWith "miso.yml has no tests (a tests: section, like executables:, of programs that exit with 0 when they pass)"
  ts <- if null names then pure (cfgTests cfg) else mapM (either failWith pure . findTest cfg) names
  installDeps cfg
  outs <- mapM (buildProgram False cfg) ts
  results <- forM (zip ts outs) $ \(t, out) -> do
    info ("Running " ++ exeName t)
    r <- runExitCode (unwords (map quote (("./" ++ out) : args)))
    case r of
      ExitSuccess -> success (exeName t ++ " passed")
      ExitFailure n -> warn (exeName t ++ " failed (exit code " ++ show n ++ ")")
    pure (exeName t, r == ExitSuccess)
  let failed = [ n | (n, False) <- results ]
      count n = show n ++ " test" ++ (if n == 1 then "" else "s")
  if null failed
    then success (count (length results) ++ " passed")
    else failWith (show (length failed) ++ " of " ++ count (length results) ++ " failed: " ++ intercalate ", " failed)

-- | Remove what builds made in the project here.  Only in a project (a
-- folder with miso.yml): elsewhere public/, dist/ and .miso/ are someone
-- else's (another kind of project, a static site, or, in the home folder,
-- ~/.miso, the miso installation).  Not .miso/ either when it is
-- $MISO_HOME or where the tool is installed, as with a project in ~.
clean :: IO ()
clean = do
  isProject <- doesFileExist configFile
  unless isProject $
    failWith ("no " ++ configFile ++ " here: miso clean removes what miso built in a project (dist/, public/, .miso/), so it runs in the project's folder")
  home <- misoHome
  tool <- toolHome
  let dirs = ["dist", "public", ".mhscache", "dist-mcabal", ".miso-build.log", ".miso"]
  there <- filterM' (\p -> (||) <$> doesDirectoryExist p <*> doesFileExist p) dirs
  ours <- filterM' (\p -> if p /= ".miso" then pure True else not <$> anyM (sameDir p) [home, tool]) there
  forM_ [ p | p <- there, p `notElem` ours ] $ \p ->
    step ("not " ++ p ++ ": it is where miso is installed ($MISO_HOME)")
  if null ours
    then step "nothing to clean"
    else do
      run ("rm -rf " ++ unwords (map quote ours))
      success ("Removed " ++ unwords ours)
  where
    filterM' p = fmap concat . mapM (\x -> (\b -> [ x | b ]) <$> p x)
    anyM p = fmap or . mapM p
    -- the same directory, after symbolic links (pwd -P)
    sameDir a b = do
      ra <- readCommand ("cd " ++ quote a ++ " 2>/dev/null && pwd -P")
      rb <- readCommand ("cd " ++ quote b ++ " 2>/dev/null && pwd -P")
      pure (case (ra, rb) of { (Just x, Just y) -> x == y; _ -> False })

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
  url <- maybe installUrl id <$> lookupEnv "MISO_INSTALL_URL"
  info ("Upgrading miso " ++ version ++ " in " ++ home)
  -- Download the installer first: piped to sh, a failed download ran an
  -- empty script, which succeeded, and the upgrade did nothing.
  tmp <- getTemporaryDirectory
  let script = tmp ++ "/miso-install.sh"
  got <- tryRun ("curl -fsSL -o " ++ quote script ++ " " ++ quote url)
  unless got $ failWith ("could not download the installer from " ++ url ++ " (set MISO_INSTALL_URL to another)")
  ok <- tryRun ("MISO_HOME=" ++ quote home ++ maybe "" (\v -> " MISO_VERSION=" ++ quote v) mv ++ " sh " ++ quote script)
  removeFile script
  unless ok $ failWith "the upgrade failed (see above)"

-- | Run hlint on the source dirs of the library and the programs, with the
-- arguments given.  hlint reads its settings from .hlint.yaml, as with any
-- Haskell project; miso.yml has none.
lint :: [String] -> Config -> IO ()
lint as cfg = do
  ok <- have "hlint"
  unless ok $ failWith "miso lint runs hlint, which is not installed (cabal install hlint, or your package manager)"
  dirs <- filterM doesDirectoryExist (allSourceDirs cfg)
  info ("Linting " ++ unwords dirs)
  r <- runExitCode (unwords (map quote ("hlint" : dirs ++ as)))
  exitWith r

-- | miso uninstall [--yes]: remove what miso put in $MISO_HOME (packages,
-- caches, mcabal's files) and the tool itself where install.sh put it.  It
-- shows what it removes and asks first (--yes does not ask; without a
-- terminal --yes is needed).  Shell files that add miso to PATH are listed,
-- not edited.
--
-- It removes only miso's own files and folders ('misoEntries'), never a
-- folder as a whole: a folder that holds only those is removed with them,
-- and one that holds anything else (miso installed as /usr/local/bin/miso,
-- or MISO_HOME pointing at a shared folder) keeps everything else.  In a
-- shared folder the caches' generic names (src/, store/, ...) are left too,
-- as they may not be miso's.  Never the home directory or /, and not a tool
-- in the nix store.
uninstall :: [String] -> IO ()
uninstall as = do
  yes <- case as of
    [] -> pure False
    ["--yes"] -> pure True
    _ -> failWith "usage: miso uninstall [--yes]"
  home <- misoHome
  tool <- toolHome
  user <- getHomeDirectory
  let inNix = "/nix/store/" `isPrefixOf` tool
      dirs = home : [ tool | not inNix, tool /= home ]
  there <- filterM doesDirectoryExist dirs
  forM_ there $ \d ->
    when (stripSlash d `elem` ["", stripSlash user]) $
      failWith (d ++ " is your home directory (or /): not removing it; check MISO_HOME")
  plans <- forM there $ \d -> misoEntries (d == home) d
  let removals = concatMap snd plans
  if null removals
    then success ("Nothing to remove: no miso files in " ++ intercalate " or " dirs)
    else do
      info "miso uninstall removes:"
      forM_ removals $ \p -> do
        size <- maybe "" (takeWhile (`notElem` " \t")) <$> readCommand ("du -sh " ++ quote p ++ " 2>/dev/null")
        step (p ++ (if null size then "" else " (" ++ size ++ ")"))
      forM_ [ d | (d, (True, _)) <- zip there plans ] $ \d ->
        step ("and " ++ d ++ " itself, if that leaves it empty")
      forM_ [ d | (d, (False, _)) <- zip there plans ] $ \d ->
        step (d ++ " has other files too: only miso's are removed")
      when inNix $ step ("not " ++ tool ++ " (installed with nix: nix profile remove miso)")
      tty <- isInputTerminal
      go <- if yes then pure True
            else if not tty then failWith "miso uninstall asks first; without a terminal, run miso uninstall --yes"
            else do
              putStr "Are you sure? This deletes everything listed above. [y/N] "
              hFlush stdout
              answer <- (Just <$> getLine) `catch` noAnswer
              pure (maybe False (`elem` ["y", "Y", "yes", "Yes"]) answer)
      unless go $ success "Nothing removed" >> exitWith ExitSuccess
      -- Windows cannot remove a running miso.exe, but can move it away
      when windows $ do
        tmp <- getTemporaryDirectory
        let exe = tool ++ "/bin/miso.exe"
        running <- doesFileExist exe
        when running $ () <$ tryRun ("mv -f " ++ quote exe ++ " " ++ quote (tmp ++ "/miso-uninstalled.exe"))
      run ("rm -rf " ++ unwords (map quote removals))
      -- the folders left empty: bin/, then the folder itself
      forM_ there $ \d -> do
        _ <- tryRun ("rmdir " ++ quote (d ++ "/bin") ++ " 2>/dev/null")
        tryRun ("rmdir " ++ quote d ++ " 2>/dev/null")
      success "Removed miso"
  -- shell files that add it to PATH
  let rcs = [ user ++ "/" ++ f | f <- [".zshrc", ".zprofile", ".bashrc", ".bash_profile", ".profile", ".config/fish/config.fish"] ]
  found <- fmap concat $ forM rcs $ \f -> do
    ok <- doesFileExist f
    if not ok then pure [] else do
      s <- readFile' f
      pure [ f ++ ":" ++ show n ++ ": " ++ l | (n, l) <- zip [1 :: Int ..] (lines s), any (`isInfixOf` l) (".miso/bin" : [ d ++ "/bin" | d <- dirs ]) ]
  unless (null found) $ do
    info "These lines add miso to PATH; remove them yourself:"
    mapM_ step found
  where
    stripSlash = reverse . dropWhile (== '/') . reverse
    noAnswer :: IOError -> IO (Maybe String)
    noAnswer _ = pure Nothing

-- | What miso put in a folder: whether the folder holds nothing else (so
-- it can go too), and the paths to remove.  The tool's files (bin/miso,
-- bin/cpphs, mhs-*/) are miso's wherever they are.  The caches of
-- $MISO_HOME (mcabal/, src/, store/, rts/) have generic names, so they are
-- removed only from a folder that holds nothing but miso's files.
misoEntries :: Bool -> FilePath -> IO (Bool, [FilePath])
misoEntries isHome d = do
  entries <- listDirectory d
  binEntries <- do
    b <- doesDirectoryExist (d ++ "/bin")
    if b then listDirectory (d ++ "/bin") else pure []
  let toolBins = [ e | e <- binEntries, e `elem` ["miso", "miso.exe", "cpphs", "cpphs.exe"] ]
      dbs = [ e | e <- entries, isDb e ]
      caches = if isHome then [ e | e <- entries, e `elem` ["mcabal", "src", "store", "rts", "miso.exe.old"] ] else []
      ours = dbs ++ caches ++ [ "bin" | all (`elem` toolBins) binEntries ]
      alone = all (`elem` ours) entries
      cacheOk = alone || not isHome
      paths = [ d ++ "/bin/" ++ e | e <- toolBins ] ++ [ d ++ "/" ++ e | e <- dbs ]
              ++ [ d ++ "/" ++ e | cacheOk, e <- caches ]
  pure (alone, paths)
  where
    -- mhs-VERSION, the tool's package database
    isDb e = case stripPrefix' "mhs-" e of
      Just (c : _) -> isDigit c
      _ -> False
    stripPrefix' p x = if p `isPrefixOf` x then Just (drop (length p) x) else Nothing


-- | Where install.sh is served.
installUrl :: String
installUrl = "https://raw.githubusercontent.com/haskell-miso/cli/main/install.sh"

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

-- | miso init: make the current directory a project, from a template,
-- keeping the files that are already there (a .gitignore gets the lines it
-- lacks).  On a terminal it asks for the name, version, synopsis, author,
-- license and template, one at a time, each with a default; the flags
-- answer them, and --yes (or input that is not a terminal) takes the
-- defaults.
initProject :: [String] -> IO ()
initProject as = do
  opts <- maybe (failWith initUsage) pure (options as)
  here <- doesFileExist configFile
  when here $ failWith (configFile ++ " is already here: this is a miso project")
  dir <- getCurrentDirectory
  tty <- isInputTerminal
  gitName <- maybe "" trimLine <$> readCommand "git config user.name"
  gitEmail <- maybe "" trimLine <$> readCommand "git config user.email"
  let interactive = tty && "--yes" `notElem` map fst opts
      given k = lookup k opts
      ask k question dflt ok why =
        case given k of
          Just v -> if ok v then pure v else failWith (k ++ " " ++ v ++ ": " ++ why)
          Nothing | not interactive -> if ok dflt then pure dflt
                                       else failWith (question ++ " " ++ show dflt ++ ": " ++ why ++ "; give " ++ k)
                  | otherwise -> loop
        where
          loop = do
            putStr (question ++ (if null dflt then "" else " [" ++ dflt ++ "]") ++ ": ")
            hFlush stdout
            answer <- (Just . trimLine <$> getLine) `catch` endOfInput
            let v = case answer of { Just a | not (null a) -> a; _ -> dflt }
            case answer of
              _ | ok v -> pure v
              Nothing -> failWith ("no " ++ question ++ " (" ++ why ++ "): give " ++ k)
              Just _ -> putStrLn ("  " ++ why) >> loop
          endOfInput :: IOError -> IO (Maybe String)
          endOfInput _ = pure Nothing
      author0 = case (gitName, gitEmail) of
                  ("", e) -> e
                  (n, e) | null e || n == e -> n
                         | otherwise -> n ++ " <" ++ e ++ ">"
  name <- ask "--name" "name" (lastPart dir) packageName "a letter, then letters, digits and -"
  ver <- ask "--version" "version" "0.1.0" versionLike "numbers separated by dots, like 0.1.0"
  synopsis <- ask "--synopsis" "synopsis" "" (const True) ""
  author <- ask "--author" "author" author0 (const True) ""
  license <- ask "--license" "license (an SPDX id, or none)" "BSD-3-Clause" (not . null) "a license id, e.g. BSD-3-Clause, MIT, Apache-2.0; or none"
  template <- ask "--template" ("template (" ++ intercalate ", " (map fst templateNames) ++ ")") "counter"
                (`elem` map fst templateNames) ("one of " ++ unwords (map fst templateNames))
  files0 <- maybe (failWith ("no template " ++ template)) pure (templates template name)
  year <- maybe "" trimLine <$> readCommand "date +%Y"
  let fields = [ "synopsis: " ++ synopsis | not (null synopsis) ]
            ++ [ "author: " ++ author | not (null author) ]
            ++ [ "license: " ++ license | license /= "none" ]
      yml contents = unlines (concatMap (\l -> if l == "version: 0.1.0" then ("version: " ++ ver) : fields else [l]) (lines contents))
      files = [ (p, if p == configFile then yml c else c) | (p, c) <- files0 ]
           ++ [ ("LICENSE", t) | Just t <- [licenseText license year author] ]
  when interactive $ putStrLn ""
  info ("Initializing " ++ name ++ " (" ++ template ++ ") in " ++ dir)
  forM_ files $ \(path, contents) -> do
    there <- doesFileExist path
    if not there
      then do
        unless (null (parent path)) $ createDirectoryIfMissing True (parent path)
        writeFile path contents
        step path
      else if path == ".gitignore"
        then do
          old <- readFile' path
          let missing = [ l | l <- lines contents, l `notElem` lines old ]
          if null missing
            then step (path ++ " (kept)")
            else do
              appendFile path ((if null old || last old == '\n' then "" else "\n") ++ unlines missing)
              step (path ++ " (added " ++ unwords missing ++ ")")
        else step (path ++ " (kept: it is already here)")
  success ("Initialized " ++ name ++ ": miso dev")
  where
    initUsage = "usage: miso init [--name NAME] [--version V] [--synopsis S] [--author A] [--license L] [--template T] [--yes]"
    options [] = Just []
    options ("--yes" : rest) = (("--yes", "") :) <$> options rest
    options (k : v : rest)
      | k `elem` ["--name", "--version", "--synopsis", "--author", "--license", "--template"] = ((k, v) :) <$> options rest
    options _ = Nothing
    lastPart = reverse . takeWhile (`notElem` "/\\") . reverse
    trimLine = reverse . dropWhile (`elem` " \t\r\n") . reverse . dropWhile (`elem` " \t")
    packageName n = case n of
      c : cs -> isAlpha c && all (\x -> isAlphaNum x || x == '-') cs
      [] -> False
    versionLike v = not (null v) && all (\p -> not (null p) && all isDigit p) (splitDots v)
    splitDots v = case break (== '.') v of
      (x, _ : r) -> x : splitDots r
      (x, []) -> [x]

listTemplates :: IO ()
listTemplates = do
  info "Templates (miso new NAME --template T)"
  forM_ templateNames $ \(t, what) -> do
    t' <- colored 1 [Cyan] t
    putStrLn ("  " ++ t' ++ replicate (10 - length t) ' ' ++ what)
