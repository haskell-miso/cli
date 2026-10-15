-- | miso serve, miso dev (serve, rebuild on changes, and reload the page),
-- and miso watch (type check on changes).
module Miso.CLI.Serve (serve, dev, watch) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, unless, when)
import System.IO (hFlush, stdout)
import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf)
import Foreign.C.String (CString, withCAString)
import Foreign.C.Types (CInt (..), CLong (..))
import System.Environment (lookupEnv)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeFile)

import Miso.CLI.Build
import Miso.CLI.Color
import Miso.CLI.Config
import Miso.CLI.Deps (installDeps)
import Miso.CLI.Util

-- cbits/serve.c
foreign import ccall "serve.h miso_serve_start" c_start :: CString -> CInt -> CInt -> IO CInt
foreign import ccall "serve.h miso_serve_poll" c_poll :: CInt -> IO ()
foreign import ccall "serve.h miso_serve_reload" c_reload :: IO ()
foreign import ccall "serve.h miso_mtime" c_mtime :: CString -> IO CLong
foreign import ccall "serve.h miso_spawn" c_spawn :: CString -> IO CLong
foreign import ccall "serve.h miso_child_done" c_childDone :: CLong -> IO CInt
foreign import ccall "serve.h miso_now_ms" c_nowMs :: IO CLong
foreign import ccall "serve.h miso_quit_on_ctrl_c" c_quitOnCtrlC :: IO ()
foreign import ccall "serve.h miso_term_cols" c_termCols :: IO CInt

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

-- | Build in the background, serving all the while, with an animation
-- (Building app. .. ...) on a terminal, and under it, dimmed and behind a
-- gutter, the last lines of the build: what mhs is doing.  The output of
-- the build goes to a log, which is shown when it fails.  Reload the pages
-- when it works.
devBuild :: Config -> Executable -> IO ()
devBuild cfg exe = do
  me <- self
  let logFile = ".miso-build.log"
  -- not to show the last build's lines before the shell starts this one's
  old <- doesFileExist logFile
  when old $ removeFile logFile
  t0 <- c_nowMs
  pid <- withCAString (quote me ++ " build --dev " ++ quote (exeName exe) ++ " > " ++ logFile ++ " 2>&1") c_spawn
  tty <- isTerminal
  bar <- gutter
  label <- colored 1 [Bold, Blue] "==>"
  let -- draw over the last frame, which had n lines under the first
      frame :: Int -> Int -> IO Int
      frame n i = do
        t <- c_nowMs
        cols <- fromIntegral <$> c_termCols
        started <- doesFileExist logFile
        out <- if started then readFile' logFile else pure ""
        let dots = take (i `mod` 3 + 1) (repeat '.')
            secs = show ((fromIntegral (t - t0) :: Int) `div` 1000) ++ "s"
            shown = lastN glimpseLines [ g | Just g <- map glimpse (lines out) ]
        dim <- mapM (\l -> colored 1 [Dim] (bar ++ " " ++ take (cols - 7) l)) shown
        putStr (up n ++ "\r\ESC[2K" ++ label ++ " Building " ++ exeName exe ++ dots ++ replicate (4 - length dots) ' ' ++ secs
                ++ concatMap (\l -> "\n\ESC[2K    " ++ l) dim)
        hFlush stdout
        pure (length dim)
      up n = if n > 0 then "\ESC[" ++ show n ++ "A" else ""
      clear n = when tty $ putStr (up n ++ "\r\ESC[J") >> hFlush stdout
      wait n i = do
        c_poll 100
        r <- c_childDone pid
        if r < 0
          then do
            n' <- if tty && i `mod` 4 == 0 then frame n (i `div` 4) else pure n
            wait n' (i + 1)
          else pure (r, n)
  unless tty $ info ("Building " ++ exeName exe)
  n0 <- if tty then frame 0 0 else pure 0
  (r, n) <- wait n0 (1 :: Int)
  clear n
  t1 <- c_nowMs
  let took = let ms = fromIntegral (t1 - t0) :: Int
             in  show (ms `div` 1000) ++ "." ++ show ((ms `mod` 1000) `div` 100) ++ "s"
  if r == 0
    then do
      c_reload
      success ("Built " ++ exeName exe ++ " in " ++ took ++ ", reloading the page")
    else do
      out <- readFile' logFile
      putStr (unlines (filter (not . isVerbose) (lines out)))
      warn ("the build failed (" ++ took ++ "); fix it and save, and miso dev builds again (the page keeps the last build)")

-- | The line down the left of the build's lines: a box-drawing one, or
-- without a UTF-8 locale, an ASCII one.
gutter :: IO String
gutter = do
  vs <- mapM lookupEnv ["LC_ALL", "LC_CTYPE", "LANG"]
  let locale = map toLower (head ([ v | Just v <- vs, not (null v) ] ++ [""]))
  pure (if "utf-8" `isInfixOf` locale || "utf8" `isInfixOf` locale then "\9474" else "|")

-- | How many lines of the build miso dev shows while it builds.
glimpseLines :: Int
glimpseLines = 3

lastN :: Int -> [a] -> [a]
lastN k xs = drop (length xs - k) xs

-- | A line of the build log as miso dev shows it while building: the
-- lines of mhs -v said shortly, without paths, and the others as they are.
glimpse :: String -> Maybe String
glimpse l
  | Just p <- after "Loading package " l = Just ("loading " ++ dropSuffix ".pkg" (baseName p))
  | Just m <- after "importing done " l' = Just ("compiled " ++ takeWhile (/= ',') m)
  | Just c <- after "Execute: " l = Just (takeWhile (/= ' ') (filter (/= '"') c) ++ ": compiling the C")
  | any (`isPrefixOf` l) ["used packages", "Found target"] = Nothing
  | all (== ' ') l = Nothing
  | otherwise = Just (unwords (words (map untab l)))
  where
    l' = dropWhile (== ' ') l
    untab c = if c == '\t' then ' ' else c
    baseName = reverse . takeWhile (/= '/') . reverse
    dropSuffix x s = if x `isSuffixOf'` s then take (length s - length x) s else s

-- | A line that only mhs -v prints, left out of the log of a failed build.
isVerbose :: String -> Bool
isVerbose l = any (`isPrefixOf` dropWhile (== ' ') l)
  [ "Loading package ", "importing done ", "total import time", "top level defns", "final pass"
  , "used packages", "Found target", "Execute: ", "C compilation" ]

after :: String -> String -> Maybe String
after p s = if p `isPrefixOf` s then Just (drop (length p) s) else Nothing

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

-- | Type check the library and the programs (or the one named) whenever
-- they change, and show the first error, as ghcid does: mhs -fno-code
-- takes under a second, where a build takes many (emcc).
watch :: Maybe String -> Config -> IO ()
watch mname cfg = do
  exes <- case mname of
    Just n -> (: []) <$> either failWith pure (findProgram cfg n)
    Nothing -> pure (cfgExecutables cfg)
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
