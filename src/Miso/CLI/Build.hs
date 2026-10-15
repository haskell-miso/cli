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

import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forM, unless, when)
import Data.List (intercalate, sort)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)

import Miso.CLI.Color
import Miso.CLI.Config
import Miso.CLI.Progress
import Miso.CLI.Util
import MicroHs.Compile (mhsVersion)

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
      -- browser_rts: the runtime of miso dev's builds (devComb)
      browser = maybe False (`elem` ["browser", "browser_rts"]) (exeTarget exe)
      check = maybe True (const False) out
  pure $ [ "-i" ++ d | d <- programDirs cfg exe ]
      ++ [ "-I" ++ d | d <- incs ] ++ concatMap cppOptions natives
      ++ (if isNat || check then [] else maybe [] (\t -> ["-t" ++ t]) (exeTarget exe))
      ++ programOptions cfg exe
      ++ (if check then ["-fno-code"] else
            cs
            -- mhs embeds the .js files it is given (emcc --pre-js)
            ++ [ j | not isNat, j <- js ]
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
  let what = "Building " ++ cfgName cfg ++ " " ++ cfgVersion cfg ++ (if exeName exe == cfgName cfg then "" else " (" ++ exeName exe ++ ")")
  loader <- rtsLoader
  took <- case loader of
    Just js | devBuild && exeTarget exe == Just "browser" -> info what >> devComb cfg exe web js >> pure Nothing
    _ -> do
      took <- runMhs what args
      -- a dev build's program, which this index.js does not read
      unless nat $ run ("rm -f " ++ quote (web ++ "/app.comb"))
      pure took
  success ("Built " ++ outFile ++ maybe "" (" in " ++) took)
  pure outFile

-- | Run mhs.  On a terminal its output goes to the build log, and the
-- build is shown as it runs (Miso.CLI.Progress), with the log when it
-- fails; how long it took.  Otherwise mhs prints as it goes (miso dev
-- shows the log of miso build --dev this way).
runMhs :: String -> [String] -> IO (Maybe String)
runMhs what args = do
  tty <- isTerminal
  if not tty
    then do
      info what
      runSelf ("mhs " ++ unwords (map quote args))
      pure Nothing
    else do
      me <- self
      (r, took) <- logged (threadDelay 100000) what buildLog
                     (quote me ++ " mhs " ++ unwords (map quote ("-v" : filter (/= "-v") args)))
      when (r /= 0) $ do
        showFailedLog buildLog
        failWith ("the build failed (" ++ took ++ ")")
      pure (Just took)

-- | The loader of the browser runtime (target browser_rts), if the
-- installed MicroHs has it.
rtsLoader :: IO (Maybe FilePath)
rtsLoader = do
  db <- toolDb
  -- the data directory is packages/MicroHs-VERSION (packages/mhs-VERSION before 0.16.9)
  let candidates = [ db ++ "/packages/" ++ d ++ "-" ++ mhsVersion ++ "/data/src/runtime/web/rts-web.js" | d <- ["MicroHs", "mhs"] ]
  found <- filterM doesFileExist candidates
  pure (case found of { js : _ -> Just js; [] -> Nothing })

-- | A dev build without a C compiler: mhs writes the program as combinators
-- (app.comb), which a runtime built once runs (index.js).  The runtime is
-- built, with emcc, for the program's foreign imports (it has their FFI
-- table) and its C and JavaScript files, and kept in $MISO_HOME/rts for the
-- next builds with the same ones.
devComb :: Config -> Executable -> FilePath -> FilePath -> IO ()
devComb cfg exe web loader = do
  -- the program's sources and options (its type checking arguments)
  checkArgs <- programArgs True cfg exe Nothing
  let srcArgs = filter (/= "-fno-code") (init checkArgs)
      comb = web ++ "/app.comb"
  -- the combinators, and after a ##### line the foreign imports
  runSelf ("mhs " ++ unwords (map quote (srcArgs ++ ["-o" ++ web ++ "/app.combffi", exeMain exe])))
  run ("mv -f " ++ quote (web ++ "/app.combffi") ++ " " ++ quote comb)
  -- what the runtime is built from: the foreign imports, the arguments
  -- (C and JavaScript files, their options), and those files
  -- (the output is a placeholder until the key, which names it, is known)
  let out = "-o" ++ "RUNTIME"
  rtsArgs <- (++ [loader]) <$> programArgs False cfg exe { exeTarget = Just "browser_rts" } (Just "RUNTIME")
  let buildArgs = filter (/= out) rtsArgs
  (cs, js, incs) <- nativeFiles cfg exe
  headers <- concat <$> mapM (\d -> listDirectory d >>= filterM doesFileExist . map ((d ++ "/") ++) . sort) incs
  home <- misoHome
  let dir = home ++ "/rts"
      keyFile = dir ++ "/key.tmp"
  createDirectoryIfMissing True dir
  -- (LC_ALL=C: the combinators are bytes, not text in the locale's encoding)
  run ("{ LC_ALL=C sed -n '/^#####$/,$p' " ++ quote comb ++ "; printf '%s\\n' " ++ unwords (map quote buildArgs)
       ++ concatMap (\f -> "; cat " ++ quote f) (cs ++ js ++ headers) ++ "; } | cksum > " ++ quote keyFile)
  key <- map (\c -> if c == ' ' then '-' else c) . takeWhile (/= '\n') <$> readFile' keyFile
  let rts = dir ++ "/rts-" ++ mhsVersion ++ "-" ++ key ++ ".js"
  have <- doesFileExist rts
  unless have $ do
    step "building the runtime with emcc (once, for these foreign imports and C and JavaScript files)"
    runSelf ("mhs " ++ unwords (map quote [ if a == out then "-o" ++ rts ++ ".tmp" else a | a <- rtsArgs ]))
    run ("mv -f " ++ quote (rts ++ ".tmp") ++ " " ++ quote rts)
  run ("cp " ++ quote rts ++ " " ++ quote (web ++ "/index.js"))
