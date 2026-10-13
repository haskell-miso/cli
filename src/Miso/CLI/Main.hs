-- | The miso command line tool.
--
-- One executable: the MicroHs compiler (miso mhs ...) and MicroCabal
-- (miso mcabal ...) are linked in, and the project commands use them.
module Miso.CLI.Main (main) where


import Control.Exception (catch)
import Control.Monad (forM_, unless, when)
import Foreign.C.Types (CInt (..))
import System.IO (hFlush, stderr, stdout)
import Data.List (isPrefixOf)
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getHomeDirectory )
import System.Environment (getArgs, getExecutablePath, lookupEnv, setEnv, withArgs)
import System.Exit (ExitCode (..), exitWith)
import Data.Maybe (isJust)
import System.Process (system)
import qualified System.Info as Info

import qualified MicroCabal.Main as MCabal
import qualified MicroHs.Main as MHS
import MicroHs.Compile (mhsVersion)

import Miso.CLI.Color
import Miso.CLI.Config
import Miso.CLI.Templates (templates)

version :: String
version = "0.1.0"

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
    "mhs" : as      -> runMhs as
    "mcabal" : as   -> runMcabal as
    ["build"]       -> withConfig build >> pure ()
    "run" : as      -> withConfig (\cfg -> build cfg >>= runExe as)
    ["install"]     -> withConfig installDeps
    ["new", name]   -> new name
    ["dev"]         -> failWith "miso dev (hot reload) is not done yet"
    ["version"]     -> printVersion
    ["--version"]   -> printVersion
    _               -> usage

usage :: IO ()
usage = do
  b <- colored 1 [Bold] "miso"
  putStrLn $ unlines
    [ b ++ " " ++ version ++ " - build miso apps with MicroHs " ++ mhsVersion
    , ""
    , "Usage: miso COMMAND"
    , ""
    , "  new NAME        make a new project in the directory NAME"
    , "  build           build the project of miso.yml"
    , "  run [ARGS]      build and run it"
    , "  install         install the dependencies of miso.yml"
    , "  dev             serve the app and reload it on changes (not done yet)"
    , "  version         show the version"
    , ""
    , "  mhs ARGS        run the MicroHs compiler"
    , "  mcabal ARGS     run MicroCabal"
    , ""
    , "Packages are installed in $MISO_HOME (default ~/.miso)."
    ]

printVersion :: IO ()
printVersion = putStrLn ("miso " ++ version ++ " (MicroHs " ++ mhsVersion ++ ")")

-----------------------------------------------------------------------------
-- Paths and the environment of the tools

misoHome :: IO FilePath
misoHome = do
  m <- lookupEnv "MISO_HOME"
  case m of
    Just d -> pure d
    Nothing -> (++ "/.miso") <$> getHomeDirectory

-- | Where mcabal installs packages, and where mhs finds them.
cabalDir, pkgDir :: IO FilePath
cabalDir = (++ "/mcabal") <$> misoHome
pkgDir = (\d -> d ++ "/mhs-" ++ mhsVersion) <$> cabalDir

-- | The path of the executable, with / (it goes in sh commands).
self :: IO String
self = map (\c -> if c == '\\' then '/' else c) <$> getExecutablePath

windows :: Bool
windows = Info.os == "windows"

-- | A command for system: on Windows system runs cmd.exe, so run sh
-- (from MSYS2) instead.
shell :: String -> String
shell c
  | windows = "sh -c \"" ++ concatMap (\x -> if x == '"' then "\\\"" else [x]) c ++ "\""
  | otherwise = c

-- | mhs runs cpphs on modules with CPP: use the one next to us.
setupCpphs :: IO ()
setupCpphs = do
  set <- lookupEnv "MHSCPPHS"
  case set of
    Just _ -> pure ()
    Nothing -> do
      exe <- self
      let cpphs = parent exe ++ "/cpphs" ++ (if windows then ".exe" else "")
      ok <- doesFileExist cpphs
      when ok $ setEnv "MHSCPPHS" cpphs

-- | Make mcabal install into our package database, and compile with us.
setupEnv :: IO ()
setupEnv = do
  setupCpphs
  cd <- cabalDir
  createDirectoryIfMissing True cd
  setEnv "CABALDIR" cd
  exe <- self
  setEnv "MHS" (quote exe ++ " mhs")

quote :: String -> String
quote s = "'" ++ concatMap (\c -> if c == '\'' then "'\\''" else [c]) s ++ "'"

-- | mhs, also finding the packages installed with miso install.  (The
-- packages that come with the tool are found next to the executable.)
-- A bare -a clears the package path, so it is left alone.
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

-- | Run a command, and stop if it fails.
run :: String -> IO ()
run c = do
  r <- system (shell c)
  case r of
    ExitSuccess -> pure ()
    ExitFailure _ -> failWith ("command failed: " ++ c)

-- | Run ourselves with the arguments.
runSelf :: String -> IO ()
runSelf as = do
  exe <- self
  run (quote exe ++ " " ++ as)

-----------------------------------------------------------------------------
-- Project commands

withConfig :: (Config -> IO a) -> IO a
withConfig k = readConfig >>= either failWith k

-- | Install the dependencies that are not installed yet.
installDeps :: Config -> IO ()
installDeps cfg = do
  setupEnv
  -- mcabal installs into the package database next to the executable, so
  -- the record of what is installed is kept there too (a new release
  -- replaces both).
  exe <- self
  let stamp = parent (parent exe) ++ "/mhs-" ++ mhsVersion ++ "/miso-installed"
  have <- do ok <- doesFileExist stamp
             if ok then lines <$> readFile' stamp else pure []
  -- A local checkout is installed on every build, as it changes; the
  -- others once.
  let local = isJust . depPath
      todo = [ d | d <- cfgDeps cfg, local d || show d `notElem` have ]
  if null todo
    then step "dependencies are installed"
    else forM_ todo $ \d -> do
      info ("Installing " ++ depName d ++ maybe "" (" from " ++) (depPath d))
      installDep d
      unless (local d) $ appendFile stamp (show d ++ "\n")

-- | The directory of a path.
parent :: FilePath -> FilePath
parent = reverse . drop 1 . dropWhile (`notElem` "/\\") . reverse

readFile' :: FilePath -> IO String
readFile' f = do
  s <- readFile f
  length s `seq` pure s

installDep :: Dep -> IO ()
installDep d = do
  exe <- self
  let mcabal = quote exe ++ " mcabal" ++ maybe "" (\f -> " -f" ++ quote f) (depFlags d) ++ " install"
  case (depGit d, depPath d) of
    (_, Just dir) -> do
      ok <- doesDirectoryExist dir
      unless ok $ failWith ("dependency " ++ depName d ++ ": no directory " ++ dir)
      run ("cd " ++ quote dir ++ " && " ++ mcabal)
    (Nothing, Nothing) -> run (mcabal ++ " " ++ quote (depName d))
    (Just repo, Nothing) -> do
      home <- misoHome
      let dir = home ++ "/src/" ++ depName d ++ "-" ++ maybe "HEAD" sanitize (depRev d)
      have <- doesDirectoryExist dir
      unless have $ do
        step ("cloning " ++ repo)
        run ("git clone --quiet " ++ quote repo ++ " " ++ quote dir)
        maybe (pure ()) (\r -> run ("git -C " ++ quote dir ++ " checkout --quiet " ++ quote r)) (depRev d)
      run ("cd " ++ quote dir ++ " && " ++ mcabal)
  where sanitize = map (\c -> if c == '/' then '-' else c)

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
  when (".js" `isSuffixOf'` exe) $ failWith "miso run runs native programs; serve public/ for a browser target"
  r <- system (shell (unwords (map quote (("./" ++ exe) : as))))
  exitWith r
  where isSuffixOf' s x = reverse s `isPrefixOf` reverse x

-- | A new project from the templates.
new :: String -> IO ()
new name = do
  exists <- doesDirectoryExist name
  when exists $ failWith (name ++ " already exists")
  info ("Creating " ++ name)
  forM_ (templates name) $ \(path, contents) -> do
    let file = name ++ "/" ++ path
    createDirectoryIfMissing True (dirOf file)
    writeFile file contents
    step file
  success ("Created " ++ name ++ ": cd " ++ name ++ " && miso build")
  where dirOf = reverse . drop 1 . dropWhile (/= '/') . reverse

