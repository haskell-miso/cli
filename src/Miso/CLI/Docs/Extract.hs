-- | The documentation of a library's modules, from their source: the
-- declarations and their doc comments (with those of constructors, fields
-- and arguments), instances, and the export lists.  It reads the source as
-- tokens (Miso.CLI.Docs.Lex); the type checker adds the types the source
-- does not say (Checked).
module Miso.CLI.Docs.Extract
  ( Package (..)
  , Module (..)
  , Item (..)
  , Declaration (..)
  , Sub (..)
  , Arg (..)
  , Instance (..)
  , Source (..)
  , Checked (..)
  , noChecked
  , extractPackage
  ) where

import Data.Char (isAlphaNum, isSpace, isUpper, ord)
import Data.List (isPrefixOf, nub)
import qualified Miso.CLI.Docs.Map as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)

import Miso.CLI.Docs.Lex
import Miso.CLI.Docs.Markup

data Package = Package
  { pkgName     :: String
  , pkgVersion  :: String
  , pkgSynopsis :: String
  , pkgModules  :: [Module]
  }

data Module = Module
  { modName   :: String
  , modDoc    :: [Block]
  , modItems  :: [Item]
  , modSource :: [String]   -- ^ highlighted HTML, a line each
  , modHeader :: [(String, String)]  -- ^ the fields of the module header: Copyright, ...
  , modHidden :: Bool       -- ^ OPTIONS_HADDOCK hide: not listed
  }

data Item
  = Heading Int String
  | Prose [Block]
  | Decl Declaration
  | ReExport String Bool    -- ^ a module the export list has, and whether its exports follow

data Declaration = Declaration
  { declName       :: String
  , declAnchor     :: String
  , declKind       :: String
  , declSig        :: String       -- ^ highlighted HTML
  , declDoc        :: [Block]
  , declMembers    :: [Declaration]
  , declSubs       :: [Sub]
  , declArgs       :: [Arg]
  , declInstances  :: [Instance]
  , declLine       :: Int
  , declEnd        :: Int
  , declFrom       :: String       -- ^ the module that defines it
  , declDeprecated :: String
  , declWarning    :: String       -- ^ "deprecated" or "warning", with declDeprecated
  , declFixity     :: String       -- ^ infixl 6, ...
  , declMinimal    :: String       -- ^ a class's minimal complete definition
  , declText       :: String       -- ^ the signature as plain text (for Hoogle)
  }

-- | A constructor or a field.
data Sub = Sub
  { subName   :: String
  , subAnchor :: String
  , subKind   :: String
  , subSig    :: String
  , subDoc    :: [Block]
  , subFields :: [Sub]
  , subText   :: String   -- ^ for Hoogle: C :: a -> T, f :: T -> a
  }

-- | An argument (or the result) of a function with docs on its arguments.
data Arg = Arg
  { argType :: String
  , argDoc  :: [Block]
  }

data Instance = Instance
  { instHead    :: String
  , instDoc     :: [Block]
  , instModule  :: String
  , instLine    :: Int
  , instEnd     :: Int
  , instDerived :: Bool
  }

-- | A module's source: the text to read (after CPP), and the text to show.
data Source = Source
  { srcModule :: String
  , srcText   :: String
  , srcShown  :: String
  }

-- | What the type checker says, for each module: the values it exports,
-- with their types and the modules that define them, and the heads of the
-- instances it defines.
data Checked = Checked
  { checkedValues    :: M.Map String [(String, String, String)]
  , checkedInstances :: M.Map String [String]
  }

noChecked :: Checked
noChecked = Checked M.empty M.empty

-----------------------------------------------------------------------------
-- What a module says, before names are resolved

data Raw = Raw
  { rName    :: String
  , rSpace   :: Space
  , rKind    :: String
  , rSig     :: [Tok]
  , rDoc     :: [String]
  , rMembers :: [Raw]
  , rSubs    :: [RawSub]
  , rArgs    :: [([Tok], [String])]
  , rLine    :: Int
  , rEnd     :: Int
  , rHead    :: String       -- ^ for a type: its name and variables (T a b)
  , rDerived :: [String]     -- ^ the classes of its deriving clauses
  , rMinimal :: String
  }

data RawSub = RawSub
  { sName   :: String
  , sKind   :: String
  , sSig    :: [Tok]
  , sDoc    :: [String]
  , sFields :: [RawSub]
  }

data RawInst = RawInst
  { iHead  :: [Tok]
  , iDoc   :: [String]
  , iLine  :: Int
  , iEnd   :: Int
  , iClass :: String
  , iTypes :: [String]
  }

data Space = Types | Values
  deriving (Eq, Ord, Show)

data Export
  = ExName Space String
  | ExBundled [String]      -- ^ the names in T(.., P): pattern synonyms, if they are
  | ExModule String
  | ExHeading Int String
  | ExDoc [String]
  | ExChunk String

data Parsed = Parsed
  { pName       :: String
  , pDoc        :: [String]
  , pExports    :: Maybe [Export]
  , pBody       :: [Either (Int, String) Raw]
  , pChunks     :: M.Map String [String]
  , pInstances  :: [RawInst]
  , pDeprecated :: M.Map String (String, String)  -- ^ kind (deprecated, warning) and message
  , pFixity     :: M.Map String String
  , pOptions    :: [String]          -- ^ of OPTIONS_HADDOCK: hide, prune, ignore-exports, not-home
  , pToks       :: M.Map Int [Tok]   -- ^ the tokens, by the line they start on
  , pShown      :: [Tok]
  }

parseModule :: Source -> Parsed
parseModule src =
  let toks = lexHaskell (srcText src)
      sig = significant toks
      (doc, name, exps, body) = header sig
      tl = topLevel (segment 1 body)
      shown | srcShown src == srcText src = toks
            | length (lines (srcShown src)) == length (lines (srcText src)) = lexHaskell (srcShown src)
            | otherwise = toks
      options = concat [ words (map (\c -> if c == ',' then ' ' else c) (drop (length "OPTIONS_HADDOCK") (dropWhile isSpace (drop 3 (tText t)))))
                       | t <- takeWhile (\t -> not (tKind t == Keyword && tText t == "module")) sig
                       , tKind t == Pragma, "OPTIONS_HADDOCK" `isPrefixOf` dropWhile isSpace (drop 3 (tText t)) ]
  in Parsed (if null name then srcModule src else name) doc (if "ignore-exports" `elem` options then Nothing else exps)
            (tlItems tl) (tlChunks tl) (tlInstances tl) (tlDeprecated tl) (tlFixity tl) options
            (M.fromListWith (flip (++)) [ (tLine t, [t]) | t <- toks ]) shown
  where
    header ts =
      let (before, rest) = break (\t -> tKind t == Keyword && tText t == "module" && tCol t == 1) ts
          doc = docRun [ t | t <- before, tKind t == Comment ]
      in case rest of
           _ : n : more | tKind n == Con ->
             let (es, afterExps) = case more of
                   p : ps | tText p == "(" -> let (inside, after) = matching 1 ps in (Just (exports inside), after)
                   _ -> (Nothing, more)
                 body = drop 1 (dropWhile (\t -> not (tKind t == Keyword && tText t == "where")) afterExps)
             in (doc, tText n, es, body)
           _ -> (doc, "", Nothing, ts)
    matching :: Int -> [Tok] -> ([Tok], [Tok])
    matching _ [] = ([], [])
    matching d (t : ts)
      | tText t == "(" = let (a, b) = matching (d + 1) ts in (t : a, b)
      | tText t == ")" = if d == 1 then ([], ts) else let (a, b) = matching (d - 1) ts in (t : a, b)
      | otherwise = let (a, b) = matching d ts in (t : a, b)

-- | The last run of doc comment lines: a -- | comment and the -- lines
-- right after it, or a {-| -} comment.
docRun :: [Tok] -> [String]
docRun cs = go cs []
  where
    go [] acc = acc
    go (t : ts) acc
      | isDocStart (tText t) =
          let (more, rest) = continuation (tokEndLine t) ts
          in go rest (map tText (t : more))
      | otherwise = go ts acc
    continuation l (t : ts)
      | "--" `isPrefixOf` tText t && tLine t == l + 1 =
          let (more, rest) = continuation (tLine t) ts in (t : more, rest)
    continuation _ ts = ([], ts)

isDocStart :: String -> Bool
isDocStart s = marked "|" s || "{-|" `isPrefixOf` s

isDocAfter :: String -> Bool
isDocAfter s = marked "^" s || "{-^" `isPrefixOf` s

-- | -- followed by a mark: -- | text, --| text.
marked :: String -> String -> Bool
marked m s = "--" `isPrefixOf` s && (m `isPrefixOf` dropWhile isSpace (dropWhile (== '-') s))

isSection :: String -> Bool
isSection s = marked "*" s || marked "$" s

sectionOf :: String -> Maybe (Int, String)
sectionOf s
  | marked "*" s = let r = dropWhile isSpace (dropWhile (== '-') s)
                       (stars, t) = span (== '*') r
                   in Just (length stars, trimS t)
  | otherwise = Nothing

chunkName :: String -> Maybe String
chunkName s
  | marked "$" s = Just (takeWhile (not . isSpace) (drop 1 (dropWhile isSpace (dropWhile (== '-') s))))
  | otherwise = Nothing

trimS :: String -> String
trimS = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | The items of an export list.
exports :: [Tok] -> [Export]
exports = go
  where
    go [] = []
    go (t : ts)
      | tKind t == Comment, Just (n, h) <- sectionOf (tText t) = ExHeading n h : go ts
      | tKind t == Comment, Just c <- chunkName (tText t) = ExChunk c : go ts
      | tKind t == Comment, isDocStart (tText t) =
          let (more, rest) = span (\x -> tKind x == Comment && not (isSection (tText x)) && not (isDocStart (tText x))) ts
          in ExDoc (commentLines (map tText (t : more))) : go rest
      | tKind t == Comment = go ts
      | tText t == "," = go ts
      | otherwise = let (it, rest) = upToComma 0 (t : ts) in export (code it) ++ go rest
    upToComma :: Int -> [Tok] -> ([Tok], [Tok])
    upToComma _ [] = ([], [])
    upToComma d (t : ts)
      | d == 0 && tText t == "," = ([], ts)
      | d == 0 && tKind t == Comment && (isSection (tText t) || isDocStart (tText t)) = ([], t : ts)
      | tText t == "(" = let (a, b) = upToComma (d + 1) ts in (t : a, b)
      | tText t == ")" = let (a, b) = upToComma (d - 1) ts in (t : a, b)
      | otherwise = let (a, b) = upToComma d ts in (t : a, b)
    export ts =
      case ts of
        k : m : _ | tText k == "module" -> [ExModule (tText m)]
        k : n : _ | tText k == "pattern" -> [ExName Values (unparen [n])]
        k : rest | tText k == "type" -> [ExName Types (unparen rest)]
        p : o : _ | tText p == "(" -> [ExName (if take 1 (tText o) == ":" then Types else Values) (tText o)]
        n : p : rest | tKind n == Con && tText p == "(" ->
          ExName Types (unqual (tText n)) : [ ExBundled [ tText c | c <- rest, tKind c == Con ] ]
        n : _ | tKind n == Con -> [ExName Types (unqual (tText n))]
        n : _ | tKind n == Var -> [ExName Values (unqual (tText n))]
        _ -> []
    unparen (p : o : _) | tText p == "(" = tText o
    unparen (n : _) = tText n
    unparen [] = ""

code :: [Tok] -> [Tok]
code = filter ((/= Comment) . tKind)

unqual :: String -> String
unqual s =
  case break (== '.') s of
    (a, _ : b) | not (null a) && isUpper (head a) && not (null b) -> unqual b
    _ -> s

-----------------------------------------------------------------------------
-- Declarations

-- | Groups of tokens: each starts at a token at the column (the first of its
-- line), and has the tokens up to the next such one.  A comment at the
-- column is a group of its own.
segment :: Int -> [Tok] -> [[Tok]]
segment col = go
  where
    go [] = []
    go (t : ts)
      | tKind t == Comment && tCol t == col = [t] : go ts
      | otherwise = let (g, rest) = break ((== col) . tCol) ts in (t : g) : go rest

data TopLevel = TopLevel
  { tlItems      :: [Either (Int, String) Raw]
  , tlChunks     :: M.Map String [String]
  , tlInstances  :: [RawInst]
  , tlDeprecated :: M.Map String (String, String)
  , tlFixity     :: M.Map String String
  }

-- | The declarations, section headings, named chunks, instances and
-- deprecations of groups.
topLevel :: [[Tok]] -> TopLevel
topLevel = go [] (TopLevel [] M.empty [] M.empty M.empty)
  where
    done tl = tl { tlItems = reverse (tlItems tl), tlInstances = reverse (tlInstances tl) }
    go _ tl [] = done tl
    go doc tl (g : gs) =
      case g of
        [c] | tKind c == Comment ->
          case () of
            -- in a doc comment, a -- * line is part of it (a list), not a section
            _ | not (null doc), tLine c == tokEndLine (last doc) + 1, "--" `isPrefixOf` tText c -> go (doc ++ [c]) tl gs
              | Just h <- sectionOf (tText c) -> go [] tl { tlItems = Left h : tlItems tl } gs
              | Just n <- chunkName (tText c) ->
                  let (more, rest) = commentsAfter (tokEndLine c) gs
                  in go [] tl { tlChunks = M.insert n (commentLines (map tText more)) (tlChunks tl) } rest
              | isDocStart (tText c) -> go [c] tl gs
              | not (null doc), tLine c == tokEndLine (last doc) + 1, "--" `isPrefixOf` tText c -> go (doc ++ [c]) tl gs
              | otherwise -> go [] tl gs
        t : _ | tKind t == Pragma ->
          go doc tl { tlDeprecated = foldr (uncurry M.insert) (tlDeprecated tl) (deprecations (tText t)) } gs
        t : rest | tText t `elem` ["infix", "infixl", "infixr"] ->
          let cs = code rest
              (prec, ops) = case cs of
                n : more | tKind n == Number -> (tText n, more)
                _ -> ("9", cs)
              names = [ tText o | o <- ops, tKind o `elem` [Op, Var, Con], tText o /= "," ]
          in go doc tl { tlFixity = foldr (\n m -> M.insert n (tText t ++ " " ++ prec) m) (tlFixity tl) names } gs
        -- data, newtype and type family instances: instances of their family
        t : rest | tText t `elem` ["data", "newtype", "type"], take 1 (map tText (code rest)) == ["instance"] ->
          let headToks = takeWhile (\x -> tText x `notElem` ["=", "where"]) (drop 1 (code rest))
          in case instanceOf headToks of
               Just (fam, tys) ->
                 go [] tl { tlInstances = RawInst headToks (if null doc then [] else commentLines (map tText doc)) (tLine t) (endLine g) fam tys : tlInstances tl } gs
               Nothing -> go [] tl gs
        t : rest | tText t == "instance" || (tText t == "deriving" && take 1 (map tText (code rest)) == ["instance"]) ->
          let headToks = takeWhile (\x -> tText x /= "where") (dropWhile (\x -> tText x `elem` ["deriving", "instance"]) (code g))
          in case instanceOf headToks of
               Just (cls, tys) ->
                 go [] tl { tlInstances = RawInst headToks (if null doc then [] else commentLines (map tText doc)) (tLine t) (endLine g) cls tys : tlInstances tl } gs
               Nothing -> go [] tl gs
        _ -> go [] tl { tlItems = reverse (map Right (declare (map tText doc) g)) ++ tlItems tl } gs
    commentsAfter l (g : gs) | [c] <- g, tKind c == Comment, "--" `isPrefixOf` tText c, tLine c == l + 1, not (isSection (tText c)) =
      let (more, rest) = commentsAfter (tLine c) gs in (c : more, rest)
    commentsAfter _ gs = ([], gs)

-- | The class and the types of an instance head: (Ord a) => C (T a) U.
instanceOf :: [Tok] -> Maybe (String, [String])
instanceOf ts =
  let afterCtx = case break ((== "=>") . tText) ts of
        (_, _ : r) -> r
        _ -> ts
      afterForall = case afterCtx of
        f : r | tText f == "forall" -> drop 1 (dropWhile ((/= ".") . tText) r)
        _ -> afterCtx
  in case afterForall of
       c : rest | tKind c == Con -> Just (unqual (tText c), nub [ unqual (tText x) | x <- rest, tKind x == Con ])
       _ -> Nothing

-- | The names and messages of a {-# DEPRECATED names "message" #-} pragma.
deprecations :: String -> [(String, (String, String))]
deprecations p =
  let body0 = dropWhile isSpace (drop 3 p)
  in case words body0 of
       w : _ | w `elem` ["DEPRECATED", "WARNING"] ->
         let body = drop (length w) body0
             (names, msg) = break (== '"') body
             message = takeWhile (/= '"') (drop 1 msg)
             kind = if w == "WARNING" then "warning" else "deprecated"
         in [ (n, (kind, message)) | n <- words (map (\c -> if c == ',' then ' ' else c) names), not (null n) ]
       _ -> []

-- | A declaration of a group, with the doc comment before it.
declare :: [String] -> [Tok] -> [Raw]
declare doc g =
  case cs of
    [] -> []
    t : rest ->
      case tText t of
        w | w `elem` ["import", "infix", "infixl", "infixr", "default", "instance", "deriving"] -> []
        "data" | next "family" rest -> typeDecl "family" (drop 1 rest)
               | next "instance" rest -> []
               | otherwise -> typeDecl "data" rest
        "newtype" | next "instance" rest -> []
                  | otherwise -> typeDecl "newtype" rest
        "type" | next "family" rest -> typeDecl "family" (drop 1 rest)
               | next "instance" rest -> []
               | otherwise -> typeDecl "type" rest
        "class" -> classDecl rest
        "pattern" | Just names <- sigNames rest -> [ (raw n Values "pattern" g) { rArgs = argDocs g } | n <- conNames rest ++ names ]
                  | otherwise -> case rest of
                      n : _ -> [ raw (tText n) Values "pattern" g ]
                      [] -> []
        "foreign" -> case [ v | (v, c) <- zip cs (drop 1 cs), tKind v `elem` [Var, Op], tText c == "::" ] of
          v : _ -> [ (raw (tText v) Values "foreign" g) { rArgs = argDocs g } ]
          [] -> []
        _ | Just names <- sigNames cs -> [ (raw n Values "function" g) { rArgs = argDocs g } | n <- names ]
          | Just n <- equationName cs -> [ raw n Values "equation" g ]
          | otherwise -> []
  where
    cs = code g
    next k (x : _) = tText x == k
    next _ [] = False
    raw n sp k sig = Raw n sp k sig (if null doc then [] else commentLines doc) [] [] [] (tLine (head g)) (endLine g) "" [] ""

    typeDecl k ts =
      case nameAfterContext ts of
        Just n ->
          let r = raw n Types k g
          in [ r { rSubs = if k `elem` ["data", "newtype"] then constructors g else []
                 , rHead = typeHead n ts
                 , rDerived = if k `elem` ["data", "newtype"] then derivings g else [] } ]
        Nothing -> []

    classDecl ts =
      case nameAfterContext ts of
        Just n ->
          let (headToks, body) = break (\x -> tKind x == Keyword && tText x == "where") g
              members = case drop 1 body of
                b : _ -> concatMap member (topLevelMembers (segment (tCol b) (drop 1 body)))
                [] -> []
              minimal = concat [ trimS (takeWhile (/= '#') (drop (length "MINIMAL") (dropWhile isSpace (drop 3 (tText p)))))
                               | p <- body, tKind p == Pragma, "MINIMAL" `isPrefixOf` dropWhile isSpace (drop 3 (tText p)) ]
          in [ (raw n Types "class" headToks) { rMembers = members, rMinimal = minimal, rHead = typeHead n ts } ]
        Nothing -> []

    member (d, mg) =
      [ if rKind r == "function" then r { rKind = "method" } else r
      | r <- declare d mg, rKind r `elem` ["function", "type", "data", "family"] ]
    -- pattern P, Q :: T
    conNames ts = [ tText c | c <- takeWhile ((/= "::") . tText) ts, tKind c == Con ]

-- | A type's name with its variables: T a b.
typeHead :: String -> [Tok] -> String
typeHead n ts =
  let afterName = drop 1 (dropWhile (\t -> tText t /= n) ts)
      vars = [ tText t | t <- takeWhile (\t -> tText t `notElem` ["=", "where", "::"] && tKind t /= Keyword) afterName, tKind t == Var ]
  in unwords (n : vars)

-- | The classes of the deriving clauses of a data declaration.
derivings :: [Tok] -> [String]
derivings g = go (code g)
  where
    go [] = []
    go (t : ts)
      | tKind t == Keyword && tText t == "deriving" =
          let ts' = case ts of
                s : r | tText s `elem` ["stock", "newtype", "anyclass"] -> r
                _ -> ts
          in case ts' of
               p : r | tText p == "(" ->
                 let (inside, rest) = break ((== ")") . tText) r
                 in [ unqual (tText c) | c <- inside, tKind c == Con ] ++ go rest
               c : r | tKind c == Con -> unqual (tText c) : go r
               _ -> go ts'
      | otherwise = go ts

-- | Member groups with the doc comments before them.
topLevelMembers :: [[Tok]] -> [([String], [Tok])]
topLevelMembers = go []
  where
    go _ [] = []
    go doc (g : gs) =
      case g of
        [c] | tKind c == Comment ->
          if isDocStart (tText c) then go [tText c] gs
          else if not (null doc) && "--" `isPrefixOf` tText c then go (doc ++ [tText c]) gs
          else go [] gs
        _ -> (doc, g) : go [] gs

endLine :: [Tok] -> Int
endLine g = maximum (map tokEndLine (code g ++ take 1 g))

-- | The names of a signature: a, b, (+) :: t.  Nothing if it is not one.
sigNames :: [Tok] -> Maybe [String]
sigNames ts =
  case break (\t -> tText t `elem` ["::", "=", "|"]) ts of
    (names, t : _) | tText t == "::" -> Just (go names)
    _ -> Nothing
  where
    go (p : o : c : rest) | tText p == "(" && tText c == ")" = tText o : go rest
    go (t : rest) | tText t == "," = go rest
                  | tKind t == Var = tText t : go rest
                  | otherwise = go rest
    go [] = []

-- | The name an equation defines: f x = ..., x <+> y = ..., x `f` y = ...
equationName :: [Tok] -> Maybe String
equationName ts
  | not (any (\t -> tText t `elem` ["=", "|"]) ts) = Nothing
  | otherwise =
      case ts of
        p : o : c : _ | tText p == "(" && tText c == ")" && tKind o == Op -> Just (tText o)
        _ : b : f : b' : _ | tText b == "`" && tText b' == "`" && tKind f == Var -> Just (tText f)
        _ : o : _ | tKind o == Op && tText o `notElem` ["=", "|", "::", "@", "~", "!"] -> Just (tText o)
        f : _ | tKind f == Var -> Just (tText f)
        _ -> Nothing

-- | The name declared after a context: (Eq a, Show a) => C a, or C a.
nameAfterContext :: [Tok] -> Maybe String
nameAfterContext ts =
  let stop t = tText t `elem` ["=", "where", "|"] || (tKind t == Keyword && tText t == "deriving")
      upTo = takeWhile (not . stop) (code ts)
      afterCtx = case break ((== "=>") . tText) upTo of
        (_, _ : r) -> r
        _ -> upTo
  in case afterCtx of
       p : o : c : _ | tText p == "(" && tText c == ")" && tKind o == Op -> Just (tText o)
       p : t : _ | tText p == "(" && tKind t == Con -> Just (tText t)
       t : _ | tKind t == Con -> Just (tText t)
       _ -> Nothing

-- | The arguments of a signature with -- ^ docs: the type split at its
-- arrows, each part with the docs in it.  None if no part has docs.
argDocs :: [Tok] -> [([Tok], [String])]
argDocs g =
  let afterSig = drop 1 (dropWhile ((/= "::") . tText) g)
      parts = splitArrows (0 :: Int) [] afterSig
      withDocs = [ (code p, commentLines [ tText c | c <- p, tKind c == Comment, isDocAfter (tText c) || isDocStart (tText c) ]) | p <- parts ]
  in if any (not . null . snd) withDocs then [ x | x@(p, _) <- withDocs, not (null p) ] else []
  where
    -- the parts between -> (and after =>) at depth 0
    splitArrows _ acc [] = [reverse acc]
    splitArrows d acc (t : ts)
      | tText t `elem` ["(", "[", "{"] = splitArrows (d + 1) (t : acc) ts
      | tText t `elem` [")", "]", "}"] = splitArrows (d - 1) (t : acc) ts
      | d == 0 && tText t `elem` ["->", "=>"] = reverse acc : splitArrows d [] ts
      | otherwise = splitArrows d (t : acc) ts

-- | Comments, then the rest of some tokens.
leadingComments :: [Tok] -> ([Tok], [Tok])
leadingComments = span ((== Comment) . tKind)

-- | The comments at the end of some tokens, and the tokens before them.
trailingComments :: [Tok] -> ([Tok], [Tok])
trailingComments ts =
  let (tr, rest) = span ((== Comment) . tKind) (reverse ts)
  in (reverse rest, reverse tr)

-- | The doc of a part (a constructor, a field): the -- | comments before it
-- (or that the part before carried over), else its -- ^ comments.
partDoc :: [Tok] -> [Tok] -> [Tok] -> [String]
partDoc carried before rest =
  let docBefore = [ tText c | c <- carried ++ before, isDocStart (tText c) || ("--" `isPrefixOf` tText c && not (isDocAfter (tText c))) ]
      docAfter = [ tText c | c <- rest, tKind c == Comment, isDocAfter (tText c) ]
  in commentLines (if any isDocStart docBefore then docBefore else docAfter)

-- | The -- | comments at the end of a part: the doc of the next one.
carriedDoc :: [Tok] -> [Tok]
carriedDoc ts = let (_, tr) = trailingComments ts in dropWhile (not . isDocStart . tText) tr

-- | Split at a separator at depth 0.
splitTop :: String -> [Tok] -> [[Tok]]
splitTop sep = go (0 :: Int) []
  where
    go _ acc [] = [reverse acc]
    go d acc (t : ts)
      | tKind t == Comment = go d (t : acc) ts
      | tText t `elem` ["(", "[", "{"] = go (d + 1) (t : acc) ts
      | tText t `elem` [")", "]", "}"] = go (d - 1) (t : acc) ts
      | d == 0 && tText t == sep = reverse acc : go d [] ts
      | otherwise = go d (t : acc) ts

-- | The constructors of a data declaration, with their docs and fields.
constructors :: [Tok] -> [RawSub]
constructors g =
  case break (\t -> tKind t /= Comment && tText t `elem` ["=", "where"]) g of
    (_, t : rest) | tText t == "=" -> alts [] (splitTop "|" (takeWhile notDeriving rest))
                  | otherwise -> gadt rest
    _ -> []
  where
    notDeriving t = not (tKind t == Keyword && tText t == "deriving")
    alts _ [] = []
    alts carried (a : as) =
      let (before, rest) = leadingComments a
          cs = code rest
          sub = case cs of
            c : more | tKind c == Con || tText c == "(" ->
              let name = if tText c == "(" then maybe "" tText (listToMaybe more) else tText c
                  (sig, fields) = record rest
              in [RawSub name "constructor" sig (partDoc carried before (outsideBraces rest)) fields]
            _ -> []
      in sub ++ alts (carriedDoc rest) as
    -- the tokens not in { }
    outsideBraces = go' (0 :: Int)
      where
        go' _ [] = []
        go' d (t : ts)
          | tText t == "{" = go' (d + 1) ts
          | tText t == "}" = go' (d - 1) ts
          | d == 0 = t : go' d ts
          | otherwise = go' d ts
    -- a record's fields, or none
    record ts =
      case break ((== "{") . tText) ts of
        (pre, _ : inside) -> (code pre, fields [] (splitTop "," (takeWhile ((/= "}") . tText) inside)))
        _ -> (fst (trailingComments ts), [])
    fields _ [] = []
    fields carried (f : fs) =
      let (before, rest) = leadingComments f
          cs = code rest
          names = [ tText v | v <- takeWhile ((/= "::") . tText) cs, tKind v == Var ]
          ty = drop 1 (dropWhile ((/= "::") . tText) cs)
      in [ RawSub n "field" ty (partDoc carried before rest) [] | n <- names ] ++ fields (carriedDoc rest) fs
    -- C :: t, at the column of the first constructor
    gadt ts =
      case dropWhile ((== Comment) . tKind) ts of
        f : _ -> concatMap con (topLevelMembers (segment (tCol f) (takeWhile notDeriving ts)))
        [] -> []
      where
        con (d, cg) =
          let cs = code cg
              names = [ tText c | c <- takeWhile ((/= "::") . tText) cs, tKind c == Con ]
          in [ RawSub n "constructor" cs (commentLines d) [] | n <- names ]

-----------------------------------------------------------------------------
-- The package

extractPackage :: String -> String -> String -> Checked -> [Source] -> Package
extractPackage name version synopsis checked srcs =
  Package name version synopsis' (map finish parsed)
  where
    values = checkedValues checked
    parsed = map parseModule srcs
    synopsis' = if null synopsis then fromMaybe "" (listToMaybe (mapMaybe description parsed)) else synopsis

    declsByModule = M.fromList [ (pName p, dedup [ r | Right r <- pBody p ]) | p <- parsed ]
    declsOf p = M.findWithDefault [] (pName p) declsByModule
    parsedByName = M.fromList [ (pName p, p) | p <- parsed ]

    hidden p = "hide" `elem` pOptions p
    visible = [ pName p | p <- parsed, not (hidden p) ]
    notHome = [ pName p | p <- parsed, "not-home" `elem` pOptions p ]

    -- where names are defined: the module, the anchor, and the lines
    defined :: M.Map (Space, String) [(String, String, Int, Int)]
    defined = M.fromListWith (flip (++))
      ([ ((rSpace r, rName r), [(pName p, anchor (rSpace r) (rName r), rLine r, rEnd r)]) | p <- parsed, r <- declsOf p ]
       ++ [ ((sp, sName s), [(pName p, anchor Values (sName s), rLine r, rEnd r)]) | p <- parsed, r <- declsOf p, s <- allSubs r, sp <- [Values, Types] ]
       ++ [ ((rSpace m, rName m), [(pName p, anchor (rSpace m) (rName m), rLine m, rEnd m)]) | p <- parsed, r <- declsOf p, m <- rMembers r ])
    allSubs r = concat [ s : sFields s | s <- rSubs r ]

    -- the visible modules that export a name (by name, or with module M of
    -- the module that defines it): where it can be linked to
    exporters :: M.Map (Space, String) [String]
    exporters = M.fromListWith (flip (++))
      [ (k, [pName p]) | p <- parsed, not (hidden p), Just es <- [pExports p], e <- es, k <- keys es e ]
      where
        keys _ (ExName sp n) = [(sp, n)]
        keys _ (ExModule m) = [ (rSpace r, rName r) | r <- M.findWithDefault [] m declsByModule ]
        keys _ _ = []
    -- the module whose page shows a name defined in a module
    home :: (Space, String) -> String -> Maybe String
    home k from =
      let ex = M.findWithDefault [] k exporters
          ok m = m `elem` visible && m `notElem` notHome
      in listToMaybe ([ from | ok from ] ++ filter ok ex ++ [ m | m <- ex, m `elem` visible ] ++ [ from | from `elem` visible ])

    -- the instances of the package (with the derived ones, and those only
    -- the type checker knows of), by the names of their classes and types
    instancesByName :: M.Map String [Instance]
    instancesByName = M.fromListWith (flip (++)) $
      [ (n, [inst]) | p <- parsed, i <- pInstances p
                    , let inst = Instance (highlightLinked (const False) (typeLink (pName p)) (lexHaskell (oneLine (render (within p (iHead i))))))
                                          (parseDoc (resolver (pName p)) (iDoc i)) (pName p) (iLine i) (iEnd i) False
                    , n <- nub (iClass i : iTypes i) ]
      ++ [ (n, [inst]) | p <- parsed, d <- declsOf p, c <- rDerived d
                       , let inst = Instance (highlightLinked (const False) (typeLink (pName p)) (lexHaskell (c ++ " " ++ paren (rHead d)))) [] (pName p) (rLine d) (rEnd d) True
                       , n <- nub [rName d, c] ]
      ++ [ (n, [inst]) | p <- parsed, h <- M.findWithDefault [] (pName p) (checkedInstances checked)
                       , Just (c, tys) <- [instanceOf (code (lexHaskell h))]
                       , not (known p c tys)
                       , let inst = Instance (highlightLinked (const False) (typeLink (pName p)) (lexHaskell h)) [] (pName p) 0 0 False
                       , n <- nub (c : tys) ]
    -- an instance the source has (declared or derived)
    known p c tys =
      or [ iClass i == c && iTypes i == tys | i <- pInstances p ]
      || or [ c `elem` rDerived d && take 1 tys == [rName d] | d <- declsOf p ]

    -- the declarations of the package, each made once (re-exports share them)
    declarations :: M.Map (String, Space, String) Declaration
    declarations = M.fromList
      [ ((pName p, rSpace r, rName r), declaration p (pName p) r) | p <- parsed, r <- declsOf p ]
    declarationOf p r = M.findWithDefault (declaration p (pName p) r) (pName p, rSpace r, rName r) declarations

    finish p =
      let res = resolver (pName p)
          items0 = case pExports p of
            Just es -> concatMap (exportItem [pName p] p) es
            Nothing -> concatMap (bodyItem p) (pBody p)
          items = if "prune" `elem` pOptions p then filter documented items0 else items0
      in Module (pName p) (parseDoc res (moduleDoc (pDoc p))) items
                (highlightLinesLinked (sourceLink (pName p)) (pShown p)) (moduleHeader (pDoc p)) (hidden p)
    documented it = case it of
      Decl d -> not (null (declDoc d)) || any (not . null . subDoc) (declSubs d) || not (null (declArgs d))
      _ -> True

    bodyItem p e =
      case e of
        Left (n, h) -> [Heading n h]
        Right r | isFirst p r -> [Decl (declarationOf p r)]
        _ -> []
    isFirst p r = case [ x | x <- declsOf p, rName x == rName r, rSpace x == rSpace r ] of
      x : _ -> rLine x == rLine r
      [] -> False

    exportItem seen p e =
      case e of
        ExHeading n h -> [Heading n h]
        ExDoc ls -> [Prose (parseDoc (resolver (pName p)) ls)]
        ExChunk c -> maybe [] (\ls -> [Prose (parseDoc (resolver (pName p)) ls)]) (M.lookup c (pChunks p))
        ExModule m
          | m == pName p || m `elem` seen -> []
          | Just q <- M.lookup m parsedByName ->
              -- a module of the package: its exports, here
              let its = maybe (concatMap (bodyItem q) (pBody q)) (concatMap (exportItem (m : seen) q)) (pExports q)
              in ReExport m True : [ it | it@(Decl _) <- its ]
          | otherwise ->
              case [ external n t from | (n, t, from) <- M.findWithDefault [] (pName p) values, from == m ] of
                [] -> [ReExport m False]
                ds -> ReExport m False : map Decl ds
        ExBundled ns ->
          [ Decl (declarationOf (parsedByName M.! from) r) | n <- ns, Just (from, r) <- [findDecl (pName p) Values n], rKind r == "pattern" ]
        ExName sp n ->
          case findDecl (pName p) sp n of
            Just (from, r) -> [Decl (declarationOf (parsedByName M.! from) r)]
            Nothing ->
              case [ external n' t from | (n', t, from) <- M.findWithDefault [] (pName p) values, n' == n, sp == Values ] of
                d : _ -> [Decl d]
                [] -> [Decl (blankDecl n (anchor sp n) "reexport" (escapeHtml n) (pName p))]

    -- a value from outside the package, with the type the type checker says
    external n t from =
      (blankDecl n (anchor Values n) "reexport" (highlightLinked (const False) (typeLink from) (lexHaskell (operatorName n ++ " :: " ++ t))) from)
        { declText = operatorName n ++ " :: " ++ t }

    blankDecl n a k s from = Declaration n a k s [] [] [] [] [] 0 0 from "" "" "" "" n

    -- a name of a module: its own, or one it re-exports from the package
    findDecl mn sp n =
      let own = [ r | r <- M.findWithDefault [] mn declsByModule, rName r == n, rSpace r == sp ]
          ownMember = [ m | r <- M.findWithDefault [] mn declsByModule, m <- rMembers r, rName m == n, rSpace m == sp ]
          froms = nub [ from | (from, _, _, _) <- M.findWithDefault [] (sp, n) defined ]
          elsewhere = [ (from, r) | from <- froms, r <- M.findWithDefault [] from declsByModule, rName r == n, rSpace r == sp ]
          elsewhereMember = [ (from, m) | from <- froms, r <- M.findWithDefault [] from declsByModule, m <- rMembers r, rName m == n ]
      in case own ++ ownMember of
           r : _ -> Just (mn, r)
           [] -> listToMaybe (elsewhere ++ elsewhereMember)

    declaration p mn r =
      let res = resolver mn
          (sigHtml, sigText) = signature p mn r
          (warnKind, warnMsg) = M.findWithDefault ("", "") (rName r) (pDeprecated p)
      in Declaration
        { declName = rName r
        , declAnchor = anchor (rSpace r) (rName r)
        , declKind = if rKind r == "equation" then "function" else rKind r
        , declSig = sigHtml
        , declDoc = parseDoc res (rDoc r)
        , declMembers = map (declaration p mn) (rMembers r)
        , declSubs = [ sub p mn r s | s <- rSubs r ]
        , declArgs = [ Arg (highlightLinked (const False) (typeLink mn) (lexHaskell (oneLine (render (within p ts))))) (parseDoc res d) | (ts, d) <- rArgs r ]
        , declInstances = instancesOf r
        , declLine = rLine r
        , declEnd = rEnd r
        , declFrom = mn
        , declDeprecated = warnMsg
        , declWarning = warnKind
        , declFixity = M.findWithDefault "" (rName r) (pFixity p)
        , declMinimal = rMinimal r
        , declText = sigText
        }

    sub p mn r s =
      let txt = oneLine (render (within p (sSig s)))
          html = case sKind s of
            "field" -> highlightLinked (const False) (typeLink mn) (lexHaskell txt)
            _ -> highlightLinked (\t -> tText t == sName s && tLine t == 1 && tCol t == 1) (typeLink mn) (lexHaskell txt)
          hoogle = case sKind s of
            "field" -> sName s ++ " :: " ++ rHead r ++ " -> " ++ dropWhile (`elem` "!~") txt
            _ | "::" `elem` words txt -> txt
              | otherwise -> sName s ++ " :: " ++ concatMap (++ " -> ") (argsOf (dropWhile ((== Space) . tKind) (drop 1 (lexHaskell txt)))) ++ rHead r
      in Sub (sName s) (anchor Values (sName s)) (sKind s) html (parseDoc (resolver mn) (sDoc s)) (map (sub p mn r) (sFields s)) hoogle
    -- the arguments of a constructor: the parts of its type split at the
    -- white space at depth 0 (without strictness marks)
    argsOf ts = filter (not . null) (map (dropWhile (`elem` "!~") . concatMap tText) (go (0 :: Int) [] ts))
      where
        go _ acc [] = [reverse acc]
        go d acc (t : rest)
          | tText t `elem` ["(", "["] = go (d + 1) (t : acc) rest
          | tText t `elem` [")", "]"] = go (d - 1) (t : acc) rest
          | d == 0 && tKind t == Space = reverse acc : go d [] rest
          | otherwise = go d (t : acc) rest

    instancesOf r =
      case rSpace r of
        Types | rKind r `elem` ["data", "newtype", "class", "type", "family"] -> M.findWithDefault [] (rName r) instancesByName
        _ -> []
    paren h = if ' ' `elem` h then "(" ++ h ++ ")" else h

    -- the signature without its comments, with its name stressed and its
    -- types linked; for a function without one, the type the type checker
    -- says.  And as plain text, on one line.
    signature p mn r
      | rKind r == "equation" =
          let txt = case [ t | (n, t, _) <- M.findWithDefault [] mn values, n == rName r ] of
                t : _ -> operatorName (rName r) ++ " :: " ++ t
                [] -> operatorName (rName r)
          in (highlightSig mn (rName r) txt, txt)
      | rKind r `elem` ["data", "newtype", "type", "family", "class"] =
          let txt = render (within p (rSig r))
              plain = (if rKind r == "family" then "type family" else rKind r) ++ " " ++ rHead r
          in (highlightSig mn (rName r) txt, plain)
      | otherwise =
          let txt = render (within p (rSig r))
              -- one name for Hoogle (a, b :: t is a :: t and b :: t)
              plain = case breakOnSig (oneLine txt) of
                Just t -> (if rKind r == "pattern" then "pattern " else "") ++ operatorName (rName r) ++ " :: " ++ t
                Nothing -> oneLine txt
          in (highlightSig mn (rName r) txt, plain)
    breakOnSig x = case x of
      ':' : ':' : ' ' : t -> Just t
      _ : rest -> breakOnSig rest
      [] -> Nothing

    highlightSig mn n s =
      let ts = lexHaskell s
          first = listToMaybe [ (tLine t, tCol t) | t <- ts, tText t == n, tKind t `elem` [Var, Con, Op] ]
      in highlightLinked (\t -> Just (tLine t, tCol t) == first) (typeLink mn) ts

    -- the tokens (with white space) from the first of some to the last
    within p sig =
      case sig of
        [] -> []
        _ -> let pos t = (tLine t, tCol t)
                 a = pos (head sig)
                 b = pos (last sig)
             in [ t | l <- [fst a .. fst b], t <- M.findWithDefault [] l (pToks p), pos t >= a, pos t <= b ]

    -- links: to the docs of a name (on the page of its home module), from a
    -- type in a signature, and to the source of a name, from the source
    resolver mn = Resolve (refUrl mn) (`elem` visible)
    refUrl mn n =
      let (q, base) = splitQual n
          candidates sp = [ (sp, from, a) | (from, a, _, _) <- M.findWithDefault [] (sp, base) defined, null q || from == q ]
          pick = [ x | sp <- if isUpperName base then [Types, Values] else [Values, Types], x <- candidates sp ]
          prefer = [ x | x@(_, from, _) <- pick, from == mn ] ++ pick
      in case [ "#/" ++ h ++ "/" ++ a | (sp, from, a) <- prefer, Just h <- [home (sp, base) from] ] of
           u : _ -> u
           [] -> ""
    typeLink mn t
      | tKind t == Con = refUrl mn (tText t)
      | otherwise = ""
    sourceLink mn t
      | tKind t `elem` [Con, Var] =
          let (q, base) = splitQual (tText t)
              sps = if tKind t == Con then [Types, Values] else [Values]
              cands = [ x | sp <- sps, x@(from, _, _, _) <- M.findWithDefault [] (sp, base) defined, null q || from == q ]
              prefer = [ x | x@(from, _, _, _) <- cands, from == mn ] ++ cands
          in case prefer of
               (from, _, l, e) : _ | l > 0 -> "#/" ++ from ++ "/src/" ++ show l ++ (if e > l then "-" ++ show e else "")
               _ -> ""
      | otherwise = ""

    description p = listToMaybe [ trimS (drop 1 (dropWhile (/= ':') l)) | l <- commentLines (pDoc p), "Description" `isPrefixOf` dropWhile isSpace l ]

-- | The fields of a module header: Copyright, License, Maintainer, ...
moduleHeader :: [String] -> [(String, String)]
moduleHeader ds =
  [ (trimS k, trimS (drop 1 v)) | l <- commentLines ds, (k, v@(':' : _)) <- [break (== ':') l]
  , trimS k `elem` ["Copyright", "License", "Maintainer", "Stability", "Portability", "Safe Haskell", "Language"] ]

-- | An operator in parentheses, a name as it is.
operatorName :: String -> String
operatorName n = if not (null n) && not (isAlphaNum (head n)) && head n /= '_' then "(" ++ n ++ ")" else n

highlightToks :: [Tok] -> String
highlightToks = highlight (const False)

-- | Text on one line: its lines joined, with single spaces.
oneLine :: String -> String
oneLine = unwords . words

-- | The module doc without the fields of a module header (Module :, ...).
moduleDoc :: [String] -> [String]
moduleDoc ds =
  let ls = commentLines ds
      isField l = case break (== ':') l of
        (k, ':' : _) -> trimS k `elem` ["Module", "Description", "Copyright", "License", "Maintainer", "Stability", "Portability", "Safe Haskell", "Language"]
        _ -> False
      continued l = not (null l) && isSpace (head l)
      drop' (l : rest) | isField l = drop' (dropWhile continued rest)
      drop' rest = rest
  in drop' ls

-- | The declarations, each once: the equations of a function are part of
-- its signature (its source goes to the last of them).
dedup :: [Raw] -> [Raw]
dedup rs =
  let ends = M.fromListWith max [ ((rSpace r, rName r), rEnd r) | r <- rs ]
      go _ [] = []
      go seen (r : rest)
        | M.member (rSpace r, rName r) seen = go seen rest
        | otherwise = r : go (M.insert (rSpace r, rName r) () seen) rest
  in [ r { rEnd = if rKind r `elem` ["function", "equation"] then M.findWithDefault (rEnd r) (rSpace r, rName r) ends else rEnd r }
     | r <- go M.empty rs ]

anchor :: Space -> String -> String
anchor sp n = (if sp == Types then "t:" else "v:") ++ concatMap enc n
  where
    enc c | isAlphaNum c || c `elem` "_'" = [c]
          | otherwise = "-" ++ show (ord c)

splitQual :: String -> (String, String)
splitQual n =
  case break (== '.') n of
    (a, _ : b) | not (null a) && isUpper (head a) && not (null b) ->
      let (q, base) = splitQual b in if null q then (a, base) else (a ++ "." ++ q, base)
    _ -> ("", n)

isUpperName :: String -> Bool
isUpperName n = not (null n) && (isUpper (head n) || head n == ':')

-- | Tokens put back together, without comments, and without the lines that
-- were only comments.
render :: [Tok] -> String
render ts =
  let src = concatMap tText (dropEndSpace (filter ((/= Comment) . tKind) ts))
      first = case ts of { t : _ -> tCol t - 1; [] -> 0 }
      ls = filter (not . all isSpace) (indentFirst first (lines src))
  in joinLines (map (dropEnd . drop' first) ls)
  where
    dropEndSpace = reverse . dropWhile ((== Space) . tKind) . reverse
    dropEnd = reverse . dropWhile isSpace . reverse
    indentFirst n (l : ls) = (replicate n ' ' ++ l) : ls
    indentFirst _ [] = []
    drop' n l = let (sp, r) = splitAt n l in if all isSpace sp then r else l
    joinLines [] = ""
    joinLines xs = foldr1 (\a b -> a ++ "\n" ++ b) xs
