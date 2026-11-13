-- | A miso project with a library, as a dependency of another project.
--
-- mcabal builds packages from a .cabal file.  A dependency (from git:, or a
-- path:) that is a miso project has a miso.yml and a library: section
-- instead: 'cabalized' makes a copy of it with the .cabal file of its
-- library ('cabalFile', which miso cabal prints), and the copy is what is
-- read for its dependencies and built.  The checkout itself is not changed.
module Miso.CLI.Cabalize
  ( cabalFile
  , cabalized
  ) where

import Control.Monad (unless, when)
import Data.List (intercalate, isPrefixOf, nub)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, listDirectory)

import Miso.CLI.Build (libraryFiles, modulesIn)
import Miso.CLI.Color
import Miso.CLI.Config
import Miso.CLI.Util

-- | The .cabal file of the library of the miso.yml here (miso cabal).
cabalFile :: Config -> IO String
cabalFile cfg = do
  lib <- maybe (failWith (cfgName cfg ++ " has no library: section in " ++ configFile
                          ++ ", so it has nothing another project can import (a library: section lists its modules' source-dirs)")) pure
                 (cfgLibrary cfg)
  -- What it needs, by name: the packages of the top-level list and of the
  -- library's.  One from git: or a path: of its own would need the project
  -- that uses this one to fetch it too, which it does not do yet.
  let deps = nub (cfgDeps cfg ++ libDeps lib)
  case [ d | d <- deps, isGit d || isLocal d ] of
    [] -> pure ()
    ds -> failWith (cfgName cfg ++ " needs " ++ intercalate ", " (map depName ds)
                    ++ " from git: or a path:, which a project that uses " ++ cfgName cfg
                    ++ " cannot fetch for it yet; list them in that project's " ++ configFile ++ " instead")
  mods <- maybe (concat <$> mapM modulesIn (libSourceDirs lib)) pure (libExposed lib)
  when (null mods) $ failWith ("the library has no modules in " ++ unwords (libSourceDirs lib))
  (cs, js, incs) <- libraryFiles lib
  headers <- case incs of
    d : _ -> filter (".h" `isSuffixOf'`) <$> listDirectory d
    [] -> pure []
  let n = libNative lib
      field k vs = [ "  " ++ k ++ ": " ++ intercalate ", " vs | not (null vs) ]
      opts k vs = [ "  " ++ k ++ ": " ++ unwords vs | not (null vs) ]
  pure $ unlines $
    [ "-- written by miso " ++ version ++ " from " ++ configFile ++ " (miso cabal)"
    , "cabal-version: 2.2"
    , "name: " ++ cfgName cfg
    , "version: " ++ cfgVersion cfg
    ] ++ [ "synopsis: " ++ s | Just s <- [cfgSynopsis cfg] ]
      ++ [ "author: " ++ a | Just a <- [cfgAuthor cfg] ] ++
    [ "build-type: Simple"
    , ""
    , "library"
    , "  default-language: Haskell2010"
    ] ++ field "hs-source-dirs" (libSourceDirs lib)
      ++ field "exposed-modules" mods
      ++ field "build-depends" ("base" : [ depName d ++ maybe "" (" == " ++) (depVersion d) | d <- deps ])
      ++ opts "mhs-options" (cfgMhsOptions cfg ++ libMhsOptions lib)
      ++ field "c-sources" cs
      ++ field "js-sources" js
      ++ field "include-dirs" incs
      ++ field "install-includes" headers
      ++ opts "cpp-options" (cppOptions n)
      ++ opts "cc-options" (ccOptions n)
      ++ opts "ld-options" (ldOptions n)
      ++ field "extra-libraries" (extraLibraries n)

-- | The directory to read and build a dependency from: dir itself when it
-- has a .cabal file (or nothing to make one from), otherwise a copy with the
-- .cabal file of its miso.yml, in $MISO_HOME/src/cabalized.  The copy is
-- made again each time, as the source (a path:) may have changed.  With
-- the name the dependency has in miso.yml, it is checked to be the
-- project's.
cabalized :: Maybe String -> FilePath -> IO FilePath
cabalized mname dir = do
  es <- listDirectory dir
  let hasCabal = any (".cabal" `isSuffixOf'`) es
  hasYml <- doesFileExist (dir ++ "/" ++ configFile)
  if hasCabal || not hasYml then pure dir else do
    name <- maybe (either failWith (pure . cfgName) . parseConfig =<< readFile' (dir ++ "/" ++ configFile)) pure mname
    home <- misoHome
    here <- getCurrentDirectory
    let abs' = if "/" `isPrefixOf` dir then dir else here ++ "/" ++ dir
    key <- maybe "" (takeWhile (/= ' ')) <$> readCommand ("printf '%s' " ++ quote abs' ++ " | cksum")
    let base = home ++ "/src/cabalized"
        stage = base ++ "/" ++ name ++ "-" ++ key
        tmp = stage ++ ".tmp"
    createDirectoryIfMissing True base
    -- its files, without what builds and tools keep there
    run ("rm -rf " ++ quote tmp ++ " && mkdir " ++ quote tmp
         ++ " && cd " ++ quote dir
         ++ " && tar -cf - --exclude=./.git --exclude=./.miso --exclude=./dist --exclude=./dist-mcabal"
         ++ " --exclude=./public --exclude=./vendor --exclude=./.mhscache . | tar -xf - -C " ++ quote tmp)
    me <- self
    (ok, out) <- captureIn tmp (quote me ++ " cabal")
    unless ok $ do
      _ <- tryRun ("rm -rf " ++ quote tmp)
      failWith ("dependency " ++ name ++ " (" ++ dir ++ ") is a miso project, but miso cannot use it as a package:\n" ++ out)
    pkg <- either failWith (pure . cfgName) . parseConfig =<< readFile' (tmp ++ "/" ++ configFile)
    when (pkg /= name) $ do
      _ <- tryRun ("rm -rf " ++ quote tmp)
      failWith ("dependency " ++ name ++ " is " ++ dir ++ ", whose " ++ configFile ++ " names it " ++ pkg
                ++ "; use name: " ++ pkg ++ " for it")
    writeFile (tmp ++ "/" ++ name ++ ".cabal") out
    run ("rm -rf " ++ quote stage ++ " && mv " ++ quote tmp ++ " " ++ quote stage)
    pure stage
  where
    -- run a command in a directory: did it work, and what it printed (stdout)
    captureIn d c = do
      out <- readCommand ("cd " ++ quote d ++ " && MISO_PROJECT= " ++ c)
      case out of
        Just s -> pure (True, s)
        Nothing -> do
          err <- maybe "" id <$> readCommand ("cd " ++ quote d ++ " && MISO_PROJECT= " ++ c ++ " 2>&1; true")
          pure (False, err)
