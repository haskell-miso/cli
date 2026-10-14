-- | miso serve, and miso dev (serve, rebuild on changes, and reload the page).
module Miso.CLI.Serve (serve, dev) where

import Control.Monad (forM, unless, when)
import System.IO (hFlush, stdout)
import Data.List (isPrefixOf)
import Foreign.C.String (CString, withCAString)
import Foreign.C.Types (CInt (..), CLong (..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)

import Miso.CLI.Color
import Miso.CLI.Config
import Miso.CLI.Util

-- cbits/serve.c
foreign import ccall "serve.h miso_serve_start" c_start :: CString -> CInt -> CInt -> IO CInt
foreign import ccall "serve.h miso_serve_poll" c_poll :: CInt -> IO ()
foreign import ccall "serve.h miso_serve_reload" c_reload :: IO ()
foreign import ccall "serve.h miso_mtime" c_mtime :: CString -> IO CLong
foreign import ccall "serve.h miso_spawn" c_spawn :: CString -> IO CLong
foreign import ccall "serve.h miso_child_done" c_childDone :: CLong -> IO CInt
foreign import ccall "serve.h miso_now_ms" c_nowMs :: IO CLong

-- | Serve dir on the port, or, when none was asked for, on the first free
-- one from 8000.
start :: FilePath -> Maybe Int -> Bool -> IO ()
start dir mport devMode = do
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
  url <- colored 1 [Bold, Cyan] ("http://localhost:" ++ show port)
  success ("Serving " ++ dir ++ "/ at " ++ url ++ " (Ctrl-C to stop)")

-- | Serve public/.
serve :: Maybe Int -> IO ()
serve port = do
  ok <- doesDirectoryExist "public"
  unless ok $ failWith "no public/ here: miso build first (target: browser)"
  start "public" port False
  let loop = c_poll 1000 >> loop
  loop

-- | Build, serve public/, and on a change of the sources rebuild and
-- reload the page.
dev :: Maybe Int -> Config -> IO ()
dev mport cfg = do
  when (maybe True (== "native") (cfgTarget cfg)) $
    failWith "miso dev is for browser targets (target: browser in miso.yml)"
  -- Serve first (the URL shows at once, and the port is checked before a
  -- long build), then build: the page reloads when the build is done.
  createDirectoryIfMissing True "public"
  start "public" mport True
  devBuild cfg
  info ("Watching " ++ unwords (watched cfg) ++ " for changes")
  snap <- snapshot cfg
  loop snap
  where
    loop old = do
      c_poll 300
      new <- snapshot cfg
      if new == old
        then loop old
        else do
          info ("Changed: " ++ unwords (take 3 (changed old new)))
          devBuild cfg
          -- what the build changed (public/ is not watched) does not count
          snapshot cfg >>= loop

-- | Build in the background, serving all the while, with an animation
-- (Building app. .. ...) on a terminal.  The output of the build goes to
-- a log, which is shown when it fails.  Reload the pages when it works.
devBuild :: Config -> IO ()
devBuild cfg = do
  exe <- self
  let logFile = ".miso-build.log"
  t0 <- c_nowMs
  pid <- withCAString (quote exe ++ " build --dev > " ++ logFile ++ " 2>&1") c_spawn
  tty <- isTerminal
  label <- colored 1 [Bold, Blue] "==>"
  let frame :: Int -> IO ()
      frame i = when tty $ do
        t <- c_nowMs
        let dots = take (i `mod` 3 + 1) (repeat '.')
            secs = show ((fromIntegral (t - t0) :: Int) `div` 1000) ++ "s"
        putStr ("\r" ++ label ++ " Building " ++ cfgName cfg ++ dots ++ replicate (4 - length dots) ' ' ++ secs ++ "   ")
        hFlush stdout
      clearLine = when tty $ putStr ("\r" ++ replicate (30 + length (cfgName cfg)) ' ' ++ "\r")
      wait i = do
        c_poll 100
        r <- c_childDone pid
        if r < 0
          then do
            when (i `mod` 4 == 0) $ frame (i `div` 4)
            wait (i + 1)
          else pure r
  unless tty $ info ("Building " ++ cfgName cfg)
  frame 0
  r <- wait (1 :: Int)
  clearLine
  t1 <- c_nowMs
  let took = let ms = fromIntegral (t1 - t0) :: Int
             in  show (ms `div` 1000) ++ "." ++ show ((ms `mod` 1000) `div` 100) ++ "s"
  if r == 0
    then do
      c_reload
      success ("Built " ++ cfgName cfg ++ " in " ++ took ++ ", reloading the page")
    else do
      out <- readFile logFile
      length out `seq` putStr out
      warn ("the build failed (" ++ took ++ "); fix it and save, and miso dev builds again (the page keeps the last build)")

-- | What dev watches: the sources, static/, miso.yml, and local dependencies.
watched :: Config -> [FilePath]
watched cfg = cfgSourceDirs cfg ++ ["static", configFile] ++ [ p | Just p <- map depPath (cfgDeps cfg) ]

type Snapshot = [(FilePath, Integer)]

snapshot :: Config -> IO Snapshot
snapshot cfg = concat <$> mapM files (watched cfg)

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
