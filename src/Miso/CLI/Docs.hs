-- | miso docs: a documentation site for the library of miso.yml.
--
-- The site is a miso app, the viewer (docs-viewer/ in this repository,
-- which the tool carries in Miso.CLI.DocsViewer), and data: what the
-- library's source says (Miso.CLI.Docs.Extract) with the types the type
-- checker adds (Miso.CLI.Docs.Check), as a script for each module in data/.
-- The viewer does not depend on the library, so it is built once (with
-- mhs and emcc) and kept in $MISO_HOME/docs-viewer.
module Miso.CLI.Docs (docs) where

import Control.Monad (forM, forM_, unless, when)
import Data.Bits (xor)
import Data.Char (isSpace)
import Data.List (foldl', isInfixOf, isPrefixOf)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeFile)
import Data.Word (Word64)
import System.Environment (lookupEnv)

import MicroHs.Compile (mhsVersion)

import Miso.CLI.Build (libraryArgs, modulesIn)
import Miso.CLI.Color
import Miso.CLI.Config (Config (..), Library (..), Native (..))
import Miso.CLI.Deps (installDeps)
import Miso.CLI.Docs.Check (typeCheck)
import Miso.CLI.Docs.Extract
import Miso.CLI.Docs.Json
import Miso.CLI.Docs.Lex (escapeHtml)
import Miso.CLI.DocsViewer (viewerFiles)
import Miso.CLI.Util

-- | miso docs [--out DIR] [--no-types]
docs :: [String] -> Config -> IO ()
docs args cfg = do
  (out, noTypes) <- either failWith pure (docsArgs args)
  lib <- maybe (failWith (cfgName cfg ++ " has no library to document (miso.yml has no library: section)")) pure (cfgLibrary cfg)
  ownOut out
  -- CPP as miso build has it: cpp-options, and the -I (include-dirs) and
  -- -D of its arguments (mhs-options)
  (largs, _) <- libraryArgs cfg lib
  let dirs = libSourceDirs lib
      cpp = cppOptions (libNative lib) ++ [ a | a <- largs, take 2 a `elem` ["-I", "-D"], a `notElem` cppOptions (libNative lib) ]
  mods <- maybe (concat <$> mapM modulesIn dirs) pure (libExposed lib)
  when (null mods) $ failWith ("the library has no modules in " ++ unwords dirs)
  info ("Documenting " ++ cfgName cfg ++ " " ++ cfgVersion cfg)
  setupCpphs
  srcs <- forM mods $ \mn -> do
    f <- findModule dirs mn
    s <- readFile f
    s' <- if usesCpp s then preprocess cpp f s else pure s
    pure (Source mn s' s)
  checked <-
    if noTypes then pure noChecked else do
      -- the type checker needs the library's dependencies
      installDeps cfg
      step "type checking (for the types the source does not say)"
      r <- typeCheck dirs cpp mods
      case r of
        Right c -> pure c
        Left e -> do
          warn ("the library does not type check, so definitions without a signature show no type: " ++ e)
          pure noChecked
  let pkg = extractPackage (cfgName cfg) (cfgVersion cfg) (maybe "" id (cfgSynopsis cfg)) checked srcs
      ndecls = sum [ length [ () | Decl _ <- modItems m ] | m <- pkgModules pkg ]
  step ("read " ++ plural (length mods) "module" ++ ", " ++ plural ndecls "export")
  -- the data of the last run, of modules that may be gone
  run ("rm -rf " ++ quote (out ++ "/data"))
  createDirectoryIfMissing True (out ++ "/data")
  writeFile (out ++ "/" ++ marker) ""
  writeFile (out ++ "/data/" ++ indexFileName) (indexScript pkg)
  writeFile (out ++ "/" ++ cfgName cfg ++ ".txt") (hoogle pkg)
  forM_ (pkgModules pkg) $ \m -> writeFile (out ++ "/data/" ++ dataFileName (modName m)) (moduleScript m)
  viewer <- buildViewer
  copyFile' viewer (out ++ "/index.js")
  forM_ viewerFiles $ \(f, s) ->
    when ("static/" `isPrefixOf` f) $
      writeIn (out ++ "/" ++ drop 7 f) (replace "%TITLE%" (escapeHtml (cfgName cfg ++ " " ++ cfgVersion cfg)) s)
  success ("Wrote " ++ out ++ "/: open " ++ out ++ "/index.html")
  where
    plural n w = show n ++ " " ++ w ++ (if n == 1 then "" else "s")

-- | The file that says miso docs wrote a directory.
marker :: FilePath
marker = ".miso-docs"

-- | Stop unless the output directory is empty, not there, or one miso docs
-- wrote: not a docs/ of the project's own (a site, a manual).
ownOut :: FilePath -> IO ()
ownOut out = do
  there <- doesDirectoryExist out
  fs <- if there then listDirectory out else pure []
  unless (null fs || marker `elem` fs) $
    failWith (out ++ "/ has files miso docs did not write: give it another directory (miso docs --out DIR)")

-- | The viewer's index.js, built if this version of it is not built yet.
buildViewer :: IO FilePath
buildViewer = do
  home <- misoHome
  -- with the tool's packages and the release's miso, not the project's
  tdb <- toolDb
  prebuilt <- doesDirectoryExist (tdb ++ "/miso-prebuilt/packages")
  hasDefault <- doesFileExist (tdb ++ "/miso-default")
  misoRelease <- if hasDefault then readFile' (tdb ++ "/miso-default") else pure ""
  let dir = home ++ "/docs-viewer/" ++ viewerKey misoRelease
      js = dir ++ "/index.js"
  built <- doesFileExist js
  unless built $ do
    emcc <- have "emcc"
    unless emcc $ failWith "miso docs builds its viewer for the browser once: install emscripten (miso doctor says how)"
    step "building the viewer (once for this version of miso)"
    forM_ viewerFiles $ \(f, s) -> unless ("static/" `isPrefixOf` f) $ writeIn (dir ++ "/src/" ++ f) s
    let tmp = dir ++ "/index.tmp.js"
    let path = "-a -a" ++ quote tdb ++ (if prebuilt then " -a" ++ quote (tdb ++ "/miso-prebuilt") else "")
    -- -i first: mhs looks in . (the project) unless told otherwise
    runSelf ("mhs " ++ path ++ " -tbrowser -i -i" ++ quote (dir ++ "/src") ++ " -o" ++ quote tmp ++ " Main")
    ok <- doesFileExist tmp
    unless ok $ failWith ("mhs did not write " ++ tmp)
    copyFile' tmp js
    removeFile tmp
  pure js

-- | A name for this viewer: a checksum (FNV-1a) of the files it is built
-- from (not static/, which is copied as it is), the versions, and the
-- release's miso (its miso-default: commit and flags).
viewerKey :: String -> String
viewerKey misoRelease = hex (foldl' step' 14695981039346656037 (concat [ f ++ s | (f, s) <- viewerFiles, not ("static/" `isPrefixOf` f) ] ++ version ++ mhsVersion ++ misoRelease))
  where
    step' :: Word64 -> Char -> Word64
    step' h c = (h `xor` fromIntegral (fromEnum c)) * 1099511628211
    hex :: Word64 -> String
    hex n = let (q, r) = n `divMod` 16 in (if q > 0 then hex q else "") ++ ["0123456789abcdef" !! fromIntegral r]

-- | The arguments: [--out DIR] [--no-types].
docsArgs :: [String] -> Either String (FilePath, Bool)
docsArgs = go ("docs", False)
  where
    go (o, n) as = case as of
      [] -> Right (o, n)
      "--no-types" : r -> go (o, True) r
      "--out" : d : r | not ("-" `isPrefixOf` d) -> go (d, n) r
      _ -> Left "expected miso docs [--out DIR] [--no-types]"

copyFile' :: FilePath -> FilePath -> IO ()
copyFile' from to = run ("cp " ++ quote from ++ " " ++ quote to)

-- | Whether a module asks for CPP.
usesCpp :: String -> Bool
usesCpp s = any (\l -> "{-#" `isPrefixOf` dropWhile isSpace l && "LANGUAGE" `isInfixOf` l && "CPP" `isInfixOf` l) (take 80 (lines s))

-- | A module after cpphs (keeping its comments and its lines), as mhs sees
-- it; if cpphs fails, the module as it is.
preprocess :: [String] -> FilePath -> String -> IO String
preprocess cpp f s = do
  cpphs <- maybe "cpphs" id <$> lookupEnv "MHSCPPHS"
  r <- readCommand (unwords (map quote ([cpphs, "--noline", "-D__MHS__"] ++ cpp ++ [f])))
  case r of
    Just s' | length (lines s') >= length (lines s) - 1 -> pure s'
    _ -> do
      warn ("cpphs failed on " ++ f ++ ": reading it without CPP")
      pure s

findModule :: [FilePath] -> String -> IO FilePath
findModule dirs mn = do
  let rel = map (\c -> if c == '.' then '/' else c) mn ++ ".hs"
  found <- filterM' doesFileExist [ d ++ "/" ++ rel | d <- dirs ]
  case found of
    f : _ -> pure f
    [] -> failWith ("no source for module " ++ mn ++ " in " ++ unwords dirs)
  where filterM' p = fmap concat . mapM (\x -> (\b -> [ x | b ]) <$> p x)

writeIn :: FilePath -> String -> IO ()
writeIn f s = do
  createDirectoryIfMissing True (reverse (drop 1 (dropWhile (/= '/') (reverse f))))
  writeFile f s

replace :: String -> String -> String -> String
replace pat new = go
  where
    go [] = []
    go s@(c : cs)
      | pat `isPrefixOf` s = new ++ go (drop (length pat) s)
      | otherwise = c : go cs
