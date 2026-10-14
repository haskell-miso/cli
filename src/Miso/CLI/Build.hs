-- | What mhs is given to check the library and to build a program: the
-- source dirs and options of miso.yml, and the C, JavaScript and include
-- files that go with them.
module Miso.CLI.Build
  ( buildProgram
  , checkLibrary
  , libraryArgs
  , programArgs
  , nativeFiles
  , modulesIn
  ) where

import Control.Monad (forM, unless, when)
import Data.List (intercalate, sort)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)

import Miso.CLI.Color
import Miso.CLI.Config
import Miso.CLI.Util

-- | The C files, JavaScript files and include dirs of a program (the
-- library's, then its own), with the globs expanded.  The cbits/*.c,
-- jsbits/*.js and include/ defaults are the library's, or with no library,
-- each program's.
nativeFiles :: Config -> Executable -> IO ([FilePath], [FilePath], [FilePath])
nativeFiles cfg exe = do
  let lib = fmap libNative (cfgLibrary cfg)
      own = exeNative exe
      dflt = maybe True (const False) lib     -- the program has the defaults
  (lc, lj, li) <- maybe (pure ([], [], [])) (files True) lib
  (oc, oj, oi) <- files dflt own
  pure (lc ++ oc, lj ++ oj, li ++ oi)
  where
    files dflt n = do
      c <- globs (pick dflt ["cbits/*.c"] (cSources n))
      j <- globs (pick dflt ["jsbits/*.js"] (jsSources n))
      i <- existing (pick dflt ["include"] (includeDirs n))
      pure (c, j, i)
    -- a default applies where its directory is there; what miso.yml lists must be
    pick dflt d = maybe (if dflt then Left d else Right []) Right
    globs (Left ps) = concat <$> mapM (expand False) ps
    globs (Right ps) = concat <$> mapM (expand True) ps
    existing (Left ds) = filterM' doesDirectoryExist ds
    existing (Right ds) = do
      forM ds $ \d -> do
        ok <- doesDirectoryExist d
        unless ok $ failWith ("miso.yml: include dir " ++ d ++ " does not exist")
        pure d

-- | The files a glob matches: a path, or one with * and ? in its last part
-- (cbits/*.c).  Must it match something?
expand :: Bool -> String -> IO [FilePath]
expand must p
  | any (`elem` "*?") p = do
      let (dir, pat) = splitLast p
      isDir <- doesDirectoryExist (if null dir then "." else dir)
      es <- if isDir then listDirectory (if null dir then "." else dir) else pure []
      let ms = [ (if null dir then "" else dir ++ "/") ++ e | e <- sort es, take 1 e /= ".", matches pat e ]
      when (must && null ms) $ failWith ("miso.yml: no files match " ++ p)
      pure ms
  | otherwise = do
      ok <- doesFileExist p
      when (must && not ok) $ failWith ("miso.yml: " ++ p ++ " does not exist")
      pure [ p | ok ]
  where
    splitLast s = let (f, d) = break (== '/') (reverse s) in (reverse (drop 1 d), reverse f)

matches :: String -> String -> Bool
matches ('*' : ps) s = matches ps s || (not (null s) && matches ('*' : ps) (tail s))
matches ('?' : ps) (_ : s) = matches ps s
matches (p : ps) (c : s) = p == c && matches ps s
matches [] [] = True
matches _ _ = False

filterM' :: (a -> IO Bool) -> [a] -> IO [a]
filterM' p = fmap concat . mapM (\x -> (\b -> [ x | b ]) <$> p x)

-- | The modules under a source dir: A/B.hs is A.B.
modulesIn :: FilePath -> IO [String]
modulesIn dir = go dir []
  where
    go d prefix = do
      isDir <- doesDirectoryExist d
      if not isDir then pure [] else do
        es <- listDirectory d
        fmap concat $ forM [ e | e <- es, take 1 e /= "." ] $ \e -> do
          sub <- doesDirectoryExist (d ++ "/" ++ e)
          if sub
            then go (d ++ "/" ++ e) (prefix ++ [e])
            else pure [ intercalate "." (prefix ++ [take (length e - 3) e]) | ".hs" `isSuffixOf'` e ]

-- | mhs's arguments to type check the library (the exposed modules, or all
-- of them), and how many modules that is.
libraryArgs :: Config -> Library -> IO ([String], Int)
libraryArgs cfg lib = do
  mods <- maybe (concat <$> mapM modulesIn (libSourceDirs lib)) pure (libExposed lib)
  when (null mods) $ failWith ("the library has no modules in " ++ unwords (libSourceDirs lib))
  let n = libNative lib
  incs <- maybe (filterM' doesDirectoryExist ["include"]) pure (includeDirs n)
  pure ( ["-fno-code"] ++ [ "-i" ++ d | d <- libSourceDirs lib ]
         ++ [ "-I" ++ d | d <- incs ] ++ cppOptions n
         ++ cfgMhsOptions cfg ++ libMhsOptions lib ++ mods
       , length mods )

-- | Type check the library.
checkLibrary :: Config -> Library -> IO ()
checkLibrary cfg lib = do
  (args, n) <- libraryArgs cfg lib
  info ("Checking " ++ cfgName cfg ++ " " ++ cfgVersion cfg ++ " (library)")
  (ok, out) <- captureSelf ("mhs " ++ unwords (map quote args))
  putStr (unlines (filter (/= "No code generated") (lines out)))
  unless ok $ failWith "the library does not type check"
  success ("Checked " ++ show n ++ " module" ++ (if n == 1 then "" else "s"))

-- | mhs's arguments to build a program, to the output file, or with
-- Nothing, to type check it.  A dev build compiles the C without
-- optimization (emcc takes about a third of the time of -Oz, for a bigger
-- file, which does not matter on localhost), and is verbose: miso dev shows
-- what mhs is doing while it builds.
programArgs :: Bool -> Config -> Executable -> Maybe FilePath -> IO [String]
programArgs devBuild cfg exe out = do
  (cs, js, incs) <- nativeFiles cfg exe
  let natives = maybe [] (\l -> [libNative l]) (cfgLibrary cfg) ++ [exeNative exe]
      isNat = isNative exe
      browser = exeTarget exe == Just "browser"
      check = maybe True (const False) out
  pure $ [ "-i" ++ d | d <- programDirs cfg exe ]
      ++ [ "-I" ++ d | d <- incs ] ++ concatMap cppOptions natives
      ++ (if isNat || check then [] else maybe [] (\t -> ["-t" ++ t]) (exeTarget exe))
      ++ programOptions cfg exe
      ++ (if check then ["-fno-code"] else
            cs
            ++ concat [ ["-js", j] | not isNat, j <- js ]
            ++ concat [ ["-optc", o] | n <- natives, o <- ccOptions n ]
            ++ concat [ ["-optl", o] | n <- natives, o <- ldOptions n ]
            ++ concat [ ["-optl", "-l" ++ l] | n <- natives, l <- extraLibraries n ]
            -- the instructions for exceptions (and longjmp) that browsers
            -- keep; emcc still makes the old, deprecated try by default
            ++ (if browser then ["-optc", "-sWASM_LEGACY_EXCEPTIONS=0"] else [])
            ++ (if devBuild && not isNat then ["-optc", "-O0"] else [])
            ++ (if devBuild then ["-v"] else [])
            ++ maybe [] (\o -> ["-o" ++ o]) out)
      ++ [exeMain exe]

-- | Build a program, and return the output file.  A browser program is
-- public/index.js, next to a copy of its static dir, or with several
-- browser programs, public/NAME/index.js.
buildProgram :: Bool -> Config -> Executable -> IO FilePath
buildProgram devBuild cfg exe = do
  let nat = isNative exe
      web = webDir cfg exe
      outFile = if nat then "dist/" ++ exeName exe else web ++ "/index.js"
  args <- programArgs devBuild cfg exe (Just outFile)
  createDirectoryIfMissing True (if nat then "dist" else web)
  static <- doesDirectoryExist (exeStaticDir exe)
  when (exeStaticDir exe /= "static" && not static) $
    failWith ("miso.yml: static-dir " ++ exeStaticDir exe ++ " does not exist")
  when (static && not nat) $ run ("cp -R " ++ quote (exeStaticDir exe) ++ "/. " ++ quote web ++ "/")
  info ("Building " ++ cfgName cfg ++ " " ++ cfgVersion cfg ++ (if exeName exe == cfgName cfg then "" else " (" ++ exeName exe ++ ")"))
  runSelf ("mhs " ++ unwords (map quote args))
  success ("Built " ++ outFile)
  pure outFile
