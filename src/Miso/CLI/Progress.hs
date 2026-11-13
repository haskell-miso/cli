-- | A build as it runs: on a terminal, a line that says what is being
-- built (Building app. .. ...) with the seconds so far, and under it,
-- dimmed and behind a gutter, the last lines of the build's log: what mhs
-- is doing.  miso dev and miso build show their builds this way.
module Miso.CLI.Progress
  ( logged
  , buildLog
  , showLog
  ) where

import Control.Monad (unless, when)
import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf)
import Foreign.C.String (CString, withCAString)
import Foreign.C.Types (CInt (..), CLong (..))
import System.Directory (doesFileExist, removeFile)
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)

import Miso.CLI.Color
import Miso.CLI.Util

foreign import ccall "serve.h miso_spawn" c_spawn :: CString -> IO CLong
foreign import ccall "serve.h miso_child_done" c_childDone :: CLong -> IO CInt
foreign import ccall "serve.h miso_now_ms" c_nowMs :: IO CLong
foreign import ccall "serve.h miso_term_cols" c_termCols :: IO CInt

-- | Where a build's output goes.
buildLog :: FilePath
buildLog = ".miso-build.log"

-- | Run a shell command with its output (stdout and stderr) going to the
-- log, showing what it does as above on a terminal, and otherwise the
-- label once.  tick waits about 100 ms (miso dev serves meanwhile).  The
-- exit code, and how long it took (12.3s).
logged :: IO () -> String -> FilePath -> String -> IO (Int, String)
logged tick label logFile cmd = do
  -- not to show the last build's lines before the shell starts this one's
  old <- doesFileExist logFile
  when old $ removeFile logFile
  t0 <- c_nowMs
  pid <- withCAString (cmd ++ " > " ++ quote logFile ++ " 2>&1") c_spawn
  tty <- isTerminal
  bar <- gutter
  arrow <- colored 1 [Bold, Blue] "==>"
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
        putStr (up n ++ "\r\ESC[2K" ++ arrow ++ " " ++ label ++ dots ++ replicate (4 - length dots) ' ' ++ secs
                ++ concatMap (\l -> "\n\ESC[2K    " ++ l) dim)
        hFlush stdout
        pure (length dim)
      up n = if n > 0 then "\ESC[" ++ show n ++ "A" else ""
      clear n = when tty $ putStr (up n ++ "\r\ESC[J") >> hFlush stdout
      wait n i = do
        tick
        r <- c_childDone pid
        if r < 0
          then do
            n' <- if tty && i `mod` 4 == 0 then frame n (i `div` 4) else pure n
            wait n' (i + 1)
          else pure (fromIntegral r, n)
  unless tty $ info label
  n0 <- if tty then frame 0 0 else pure 0
  (r, n) <- wait n0 (1 :: Int)
  clear n
  t1 <- c_nowMs
  let ms = fromIntegral (t1 - t0) :: Int
  pure (r, show (ms `div` 1000) ++ "." ++ show ((ms `mod` 1000) `div` 100) ++ "s")

-- | A build's log without the lines only mhs -v prints: its errors when
-- it failed, and its warnings.
showLog :: FilePath -> IO ()
showLog logFile = do
  out <- readFile' logFile
  putStr (unlines (filter (not . isVerbose) (lines out)))

-- | The line down the left of the build's lines: a box-drawing one, or
-- without a UTF-8 locale, an ASCII one.
gutter :: IO String
gutter = do
  vs <- mapM lookupEnv ["LC_ALL", "LC_CTYPE", "LANG"]
  let locale = map toLower (head ([ v | Just v <- vs, not (null v) ] ++ [""]))
  pure (if "utf-8" `isInfixOf` locale || "utf8" `isInfixOf` locale then "\9474" else "|")

-- | How many lines of the build are shown while it builds.
glimpseLines :: Int
glimpseLines = 3

lastN :: Int -> [a] -> [a]
lastN k xs = drop (length xs - k) xs

-- | A line of the build log as shown while building: the lines of mhs -v
-- said shortly, without paths, and the others as they are.
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
