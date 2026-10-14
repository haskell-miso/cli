-- | The types of what a library exports, from the type checker (mhs, in
-- this process): for the functions without a signature, and for the names
-- a module re-exports from other packages.
module Miso.CLI.Docs.Check (typeCheck) where

import Control.Exception (SomeException, evaluate, try)
import Data.Char (isAlphaNum, isUpper)
import qualified Data.Map as M

import MicroHs.Compile (compileMany, getPaths)
import MicroHs.CompileCache (emptyCache, getCompMdls)
import MicroHs.Expr (showEType, showExprRaw)
import MicroHs.Flags (Flags (..), defaultFlags)
import MicroHs.Ident (mkIdent, unIdent)
import MicroHs.SymTab (Entry (..))
import MicroHs.TypeCheck (TModule (..), ValueExport (..))

import Miso.CLI.Docs.Extract (Checked (..))
import Miso.CLI.Docs.Lex (Kind (..), Tok (..), lexHaskell)
import Miso.CLI.Util (pkgDir, toolDb)

-- | Type check modules (in source dirs, with CPP options), or say why not.
typeCheck :: [FilePath] -> [String] -> [String] -> IO (Either String Checked)
typeCheck dirs cpp mods = do
  (mhsDir, _, _) <- getPaths
  db <- toolDb
  pd <- pkgDir
  let flags = defaultFlags
        { mhsdir = mhsDir, srcPaths = dirs, pkgPaths = [db, pd]
        , cppArgs = cpp, noCode = True, verbose = -1 }
  r <- try $ do
    cache <- compileMany flags (map mkIdent mods) emptyCache
    let checked = M.fromList [ (unIdent (tModuleName tm), map export (tValueExps tm)) | tm <- getCompMdls cache ]
    _ <- evaluate (sum [ length a + length b + length c | es <- M.elems checked, (a, b, c) <- es ])
    pure checked
  pure $ case r of
    Right m -> Right (Checked m)
    Left e -> Left (firstLine (show (e :: SomeException)))
  where
    firstLine = takeWhile (/= '\n')
    export (ValueExport i (Entry qi t)) = (unIdent i, cleanType (showEType t), qualifier (showExprRaw qi))

-- | The module of a qualified name: Data.Map.insert, Data.Map.!
qualifier :: String -> String
qualifier = go []
  where
    go acc s =
      case span (\c -> isAlphaNum c || c == '_' || c == '\'') s of
        (w@(c : _), '.' : rest@(_ : _)) | isUpper c -> go (acc ++ [w]) rest
        _ -> foldr1' (\a b -> a ++ "." ++ b) acc
    foldr1' _ [] = ""
    foldr1' f xs = foldr1 f xs

-- | A type as the source would say it: without its outer forall, and with
-- unqualified names.
cleanType :: String -> String
cleanType s =
  let ts = lexHaskell s
      ts' = case ts of
        f : rest | tText f == "forall" -> dropWhile ((== Space) . tKind) (drop 1 (dropWhile ((/= ".") . tText) rest))
        _ -> ts
  in concatMap (\t -> if tKind t `elem` [Con, Var, Op] then unqualify (tText t) else tText t) ts'
  where
    unqualify n =
      case break (== '.') n of
        (a@(c : _), '.' : b@(_ : _)) | isUpper c && all (\x -> isAlphaNum x || x == '_') a -> unqualify b
        _ -> n
