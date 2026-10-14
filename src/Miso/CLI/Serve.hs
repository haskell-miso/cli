-- | miso serve, and miso dev (serve, rebuild on changes, and reload the page).
module Miso.CLI.Serve (serve, dev) where

import Control.Monad (forM, unless, when)
import Data.List (isPrefixOf)
import Foreign.C.String (CString, withCAString)
import Foreign.C.Types (CInt (..), CLong (..))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)

import Miso.CLI.Color
import Miso.CLI.Config
import Miso.CLI.Util

-- cbits/serve.c
foreign import ccall "serve.h miso_serve_start" c_start :: CString -> CInt -> CInt -> IO CInt
foreign import ccall "serve.h miso_serve_poll" c_poll :: CInt -> IO ()
foreign import ccall "serve.h miso_serve_reload" c_reload :: IO ()
foreign import ccall "serve.h miso_mtime" c_mtime :: CString -> IO CLong

start :: FilePath -> Int -> Bool -> IO ()
start dir port devMode = do
  r <- withCAString dir $ \d -> c_start d (fromIntegral port) (if devMode then 1 else 0)
  when (r /= 0) $ failWith ("cannot serve on port " ++ show port ++ " (in use?); try --port")
  url <- colored 1 [Bold, Cyan] ("http://localhost:" ++ show port)
  success ("Serving " ++ dir ++ "/ at " ++ url ++ " (Ctrl-C to stop)")

-- | Serve public/.
serve :: Int -> IO ()
serve port = do
  ok <- doesDirectoryExist "public"
  unless ok $ failWith "no public/ here: miso build first (target: browser)"
  start "public" port False
  let loop = c_poll 1000 >> loop
  loop

-- | Build, serve public/, and on a change of the sources rebuild and
-- reload the page.
dev :: Int -> Config -> IO ()
dev port cfg = do
  when (maybe True (== "native") (cfgTarget cfg)) $
    failWith "miso dev is for browser targets (target: browser in miso.yml)"
  built <- tryRunSelf "build"
  unless built $ warn "the build failed; fix it and save, and miso dev builds again"
  ok <- doesDirectoryExist "public"
  unless ok $ failWith "no public/ to serve"
  start "public" port True
  info ("Watching " ++ unwords (watched cfg))
  snap <- snapshot cfg
  loop snap
  where
    loop old = do
      c_poll 300
      new <- snapshot cfg
      if new == old
        then loop old
        else do
          info ("Changed: " ++ unwords (take 3 (changed old new)) ++ ", building")
          built <- tryRunSelf "build"
          if built
            then c_reload >> success "Reloaded"
            else warn "the build failed; the page keeps the last build"
          -- what the build changed (public/ is not watched) does not count
          snapshot cfg >>= loop

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
