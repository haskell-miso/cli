-- | Colored terminal output.  Colors are used only when the output is a
-- terminal and NO_COLOR is not set (https://no-color.org).
module Miso.CLI.Color
  ( Color (..)
  , colored
  , info
  , step
  , success
  , warn
  , failWith
  , isTerminal
  ) where


import Foreign.C.Types (CInt (..))
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

-- The library's hIsTerminalDevice is not reliable on every target, so ask libc.
foreign import ccall "isatty" c_isatty :: CInt -> IO CInt

data Color = Red | Green | Yellow | Blue | Magenta | Cyan | Bold | Dim

code :: Color -> String
code c =
  case c of
    Red     -> "31"
    Green   -> "32"
    Yellow  -> "33"
    Blue    -> "34"
    Magenta -> "35"
    Cyan    -> "36"
    Bold    -> "1"
    Dim     -> "2"

-- | Is stdout a terminal (for animations)?
isTerminal :: IO Bool
isTerminal = (/= 0) <$> c_isatty 1

-- | Wrap a string in the color, if the file descriptor is a terminal.
colored :: CInt -> [Color] -> String -> IO String
colored fd cs s = do
  tty <- (/= 0) <$> c_isatty fd
  noColor <- maybe False (not . null) <$> lookupEnv "NO_COLOR"
  pure $
    if tty && not noColor && not (null cs)
      then "\ESC[" ++ foldr1 (\a b -> a ++ ";" ++ b) (map code cs) ++ "m" ++ s ++ "\ESC[0m"
      else s

out :: [Color] -> String -> String -> IO ()
out cs tag msg = do
  t <- colored 1 cs tag
  putStrLn (t ++ " " ++ msg)
  hFlush stdout

-- | A message about what is happening.
info :: String -> IO ()
info = out [Bold, Blue] "==>"

-- | A step of a larger task.
step :: String -> IO ()
step = out [Cyan] "  ->"

success :: String -> IO ()
success = out [Bold, Green] "==>"

warn :: String -> IO ()
warn msg = do
  t <- colored 2 [Bold, Yellow] "warning:"
  hPutStrLn stderr (t ++ " " ++ msg)

-- | Report an error and exit.
failWith :: String -> IO a
failWith msg = do
  t <- colored 2 [Bold, Red] "error:"
  hPutStrLn stderr (t ++ " " ++ msg)
  exitFailure

