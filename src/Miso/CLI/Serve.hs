-- | miso serve, miso dev (serve, rebuild on changes, and reload the page),
-- and miso watch (type check on changes).
module Miso.CLI.Serve (serve, dev, watch) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, unless, when)
import System.IO (hFlush, stdout)
import Data.List (isPrefixOf)
import Foreign.C.String (CString, withCAString)
import Foreign.C.Types (CInt (..), CLong (..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)

import Miso.CLI.Build
import Miso.CLI.Color
import Miso.CLI.Config
import Miso.CLI.Deps (installDeps)
import Miso.CLI.Progress
import Miso.CLI.Util

-- cbits/serve.c
foreign import ccall "serve.h miso_serve_start" c_start :: CString -> CInt -> CInt -> IO CInt
foreign import ccall "serve.h miso_serve_poll" c_poll :: CInt -> IO ()
foreign import ccall "serve.h miso_serve_reload" c_reload :: IO ()
foreign import ccall "serve.h miso_mtime" c_mtime :: CString -> IO CLong
foreign import ccall "serve.h miso_now_ms" c_nowMs :: IO CLong
foreign import ccall "serve.h miso_quit_on_ctrl_c" c_quitOnCtrlC :: IO ()

-- | Serve dir on the port, or, when none was asked for, on the first free
-- one from 8000; the URL shown is of the page at path in it.
start :: FilePath -> String -> Maybe Int -> Bool -> IO ()
start dir path mport devMode = do
  c_quitOnCtrlC
  let try p = withCAString dir $ \d -> c_start d (fromIntegral p) (if devMode then 1 else 0)
      firstFree [] = failWith "no free port in 8000-8099; pick one with --port"
      firstFree (p : ps) = do
        r <- try p
        if r == 0 then pure p else firstFree ps
  port <- case mport of
    Just p -> do
      r <- try p
      when (r /= 0) $ failWith ("cannot serve on port " ++ show p ++ " (in use?); leave out --port to take a free one")
      pure p
    Nothing -> firstFree [8000 .. 8099]
  when (mport == Nothing && port /= 8000) $ step ("port 8000 is in use, so " ++ show port)
  url <- colored 1 [Bold, Cyan] ("http://localhost:" ++ show port ++ "/" ++ path)
  success ("Serving " ++ dir ++ "/ at " ++ url ++ " (Ctrl-C to stop)")

-- | Serve public/.
serve :: Maybe Int -> IO ()
serve port = do
  ok <- doesDirectoryExist "public"
  unless ok $ failWith "no public/ here: miso build first (target: browser)"
  start "public" "" port False
  let loop = c_poll 1000 >> loop
  loop

-- | Build a browser program (the one named, or the first), serve public/,
-- and on a change of the sources rebuild and reload the page.
dev :: Maybe Int -> Maybe String -> Config -> IO ()
dev mport mname cfg = do
  exe <- case mname of
    Just n -> either failWith pure (findProgram cfg n)
    Nothing -> case filter (not . isNative) (cfgExecutables cfg) of
      e : _ -> pure e
      [] | null (cfgExecutables cfg) -> failWith (noProgram cfg "nothing to serve")
         | otherwise -> failWith "miso dev is for browser targets (target: browser in miso.yml)"
  when (isNative exe) $
    failWith (exeName exe ++ " is a native program; miso dev is for browser targets (target: browser in miso.yml)")
  -- Serve first (the URL shows at once, and the port is checked before a
  -- long build), then build: the page reloads when the build is done.
  let web = webDir cfg exe
  createDirectoryIfMissing True web
  start "public" (drop (length "public/") (web ++ "/")) mport True
  devBuild cfg exe
  info ("Watching " ++ unwords (watched cfg exe) ++ " for changes")
  snap <- snapshot cfg exe
  loop exe snap
  where
    loop exe old = do
      c_poll 300
      new <- snapshot cfg exe
      if new == old
        then loop exe old
        else do
          info ("Changed: " ++ unwords (take 3 (changed old new)))
          devBuild cfg exe
          -- what the build changed (public/ is not watched) does not count
          snapshot cfg exe >>= loop exe

-- | Build in the background, serving all the while, showing the build as
-- it runs (Miso.CLI.Progress).  The log is shown when the build fails.
-- Reload the pages when it works.
devBuild :: Config -> Executable -> IO ()
devBuild cfg exe = do
  me <- self
  (r, took) <- logged (c_poll 100) ("Building " ++ exeName exe) buildLog
                 (quote me ++ " build --dev " ++ quote (exeName exe))
  if r == 0
    then do
      c_reload
      success ("Built " ++ exeName exe ++ " in " ++ took ++ ", reloading the page")
    else do
      showFailedLog buildLog
      warn ("the build failed (" ++ took ++ "); fix it and save, and miso dev builds again (the page keeps the last build)")

-- | What dev watches: the program's sources (and the library's), its
-- static dir, its C, JavaScript and include dirs, miso.yml, and local
-- dependencies.
watched :: Config -> Executable -> [FilePath]
watched cfg exe = watchedFor cfg [exe]

-- | What changes the programs (and the library), of those that are there.
watchedFor :: Config -> [Executable] -> [FilePath]
watchedFor cfg exes = nub' $
  maybe [] libSourceDirs (cfgLibrary cfg) ++ concatMap exeSourceDirs exes
  ++ map exeStaticDir exes
  ++ ["cbits", "jsbits", "include"] ++ concatMap nativeDirs natives
  ++ [configFile] ++ [ p | Just p <- map depPath (allDeps cfg) ]
  where
    natives = maybe [] (\l -> [libNative l]) (cfgLibrary cfg) ++ map exeNative exes
    nativeDirs n = map dirOf (maybe [] id (cSources n) ++ maybe [] id (jsSources n)) ++ maybe [] id (includeDirs n)
    dirOf p = case reverse (dropWhile (/= '/') (reverse p)) of
                "" -> "."
                d -> init d
    nub' = foldr (\x xs -> if x `elem` xs then xs else x : filter (/= x) xs) []

-----------------------------------------------------------------------------
-- miso watch

-- | Type check the library, the programs and the tests (or the one named) whenever
-- they change, and show the first error, as ghcid does: mhs -fno-code
-- takes under a second, where a build takes many (emcc).
watch :: Maybe String -> Config -> IO ()
watch mname cfg = do
  exes <- case mname of
    Just n -> (: []) <$> either failWith pure (either (const (findTest cfg n)) Right (findProgram cfg n))
    Nothing -> pure (cfgExecutables cfg ++ cfgTests cfg)
  installDeps cfg
  tty <- isTerminal
  let paths = filter (/= ".") (watchedFor cfg exes)
      snap = concat <$> mapM files paths
  there <- filterM' (\p -> (||) <$> doesDirectoryExist p <*> doesFileExist p) paths
  let
      check = do
        when tty $ putStr "\ESC[2J\ESC[H" >> hFlush stdout
        t0 <- c_nowMs
        r <- typeCheck cfg exes
        t1 <- c_nowMs
        let took = show ((fromIntegral (t1 - t0) :: Int) `div` 100 `div` 10) ++ "." ++ show ((fromIntegral (t1 - t0) :: Int) `div` 100 `mod` 10) ++ "s"
        case r of
          Right what -> success ("All good: " ++ what ++ " (" ++ took ++ ")")
          Left err -> do
            putStr err
            warn ("type error (" ++ took ++ ")")
        foot <- colored 1 [Dim] ("Watching " ++ unwords there ++ " (Ctrl-C to stop)")
        putStrLn foot
        hFlush stdout
      loop old = do
        threadDelay 300000
        new <- snap
        if new == old then loop old else check >> snap >>= loop
  check
  snap >>= loop

-- | Type check the library, then each program; what was checked, or the
-- first error.
typeCheck :: Config -> [Executable] -> IO (Either String String)
typeCheck cfg exes = do
  lib <- case cfgLibrary cfg of
    Nothing -> pure (Right [])
    Just l -> do
      (args, n) <- libraryArgs cfg l
      r <- mhs args
      pure (fmap (const ["the library (" ++ show n ++ " module" ++ (if n == 1 then "" else "s") ++ ")"]) r)
  case lib of
    Left e -> pure (Left e)
    Right done -> go done exes
  where
    go done [] = pure (Right (intercalate' done))
    go done (e : es) = do
      args <- programArgs False cfg e Nothing
      r <- mhs args
      case r of
        Left err -> pure (Left err)
        Right () -> go (done ++ [exeName e]) es
    mhs args = do
      (ok, out) <- captureSelf ("mhs " ++ unwords (map quote args))
      if ok then pure (Right ()) else Left <$> errorText out
    intercalate' [] = "nothing to check"
    intercalate' xs = foldr1 (\a b -> a ++ ", " ++ b) xs

filterM' :: (a -> IO Bool) -> [a] -> IO [a]
filterM' p = fmap concat . mapM (\x -> (\b -> [ x | b ]) <$> p x)

-- | mhs's error, as an editor reads it: "app/Main.hs": line 3, col 9: ...
-- becomes app/Main.hs:3:9: ..., without the uncaught exception around it.
errorText :: String -> IO String
errorText out = unlines <$> mapM line (dropWhile (all (== ' ')) (filter (/= "No code generated") (lines out)))
  where
    marker = "uncaught exception: error: "
    line l =
      case breakOn marker l of
        Just rest -> location rest
        Nothing -> pure l
    location r =
      case r of
        '"' : r1 | (file, '"' : ':' : ' ' : r2) <- break (== '"') r1
                 , Just (ln, r3) <- num "line " r2
                 , Just (col, r4) <- num ", col " r3
                 , ':' : msg <- r4 -> do
            loc <- colored 1 [Bold] (file ++ ":" ++ ln ++ ":" ++ col ++ ":")
            pure (loc ++ msg)
        _ -> pure r
    num p x = if p `isPrefixOf` x then let (d, rest) = span (`elem` "0123456789") (drop (length p) x) in if null d then Nothing else Just (d, rest) else Nothing
    breakOn p x
      | p `isPrefixOf` x = Just (drop (length p) x)
      | otherwise = case x of { [] -> Nothing; _ : xs -> breakOn p xs }

type Snapshot = [(FilePath, Integer)]

snapshot :: Config -> Executable -> IO Snapshot
snapshot cfg exe = concat <$> mapM files (watched cfg exe)

files :: FilePath -> IO Snapshot
files p = do
  isDir <- doesDirectoryExist p
  if isDir
    then do
      es <- listDirectory p
      concat <$> forM [ e | e <- es, not ("." `isPrefixOf` e), e `notElem` ["dist-mcabal", "dist", "public"] ] (\e -> files (p ++ "/" ++ e))
    else do
      isFile <- doesFileExist p
      if isFile
        then do
          t <- withCAString p c_mtime
          pure [(p, fromIntegral t)]
        else pure []

changed :: Snapshot -> Snapshot -> [FilePath]
changed old new =
  [ p | (p, t) <- new, lookup p old /= Just t ] ++ [ p | (p, _) <- old, lookup p new == Nothing ]
