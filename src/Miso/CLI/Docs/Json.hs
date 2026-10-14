-- | The data of a documentation site, as JSON for the viewer: an index (the
-- package, its modules and what search finds), and a file for each module.
-- Each file is a script (data/NAME.js) that hands its JSON to the viewer,
-- so the site works from file:// as well as from a server.
module Miso.CLI.Docs.Json
  ( indexScript
  , moduleScript
  , dataFileName
  ) where

import Data.Char (ord)

import Miso.CLI.Docs.Extract
import Miso.CLI.Docs.Lex (highlight, lexHaskell)
import Miso.CLI.Docs.Markup (Block (..), Inline (..))

data J = JS String | JN Int | JB Bool | JA [J] | JO [(String, J)]

render :: J -> String
render j = go j ""
  where
    go v = case v of
      JS s -> str s
      JN n -> showString (show n)
      JB b -> showString (if b then "true" else "false")
      JA xs -> showChar '[' . commas (map go xs) . showChar ']'
      JO kvs -> showChar '{' . commas [ str k . showChar ':' . go x | (k, x) <- kvs ] . showChar '}'
    commas [] = id
    commas (x : xs) = x . foldr (\y acc -> showChar ',' . y . acc) id xs
    str s = showChar '"' . foldr (\c acc -> esc c . acc) id s . showChar '"'

-- | A character in a JSON (and JavaScript) string: ASCII as it is, the rest
-- as \u escapes.
esc :: Char -> ShowS
esc c
  | c == '"' = showString "\\\""
  | c == '\\' = showString "\\\\"
  | c == '\n' = showString "\\n"
  | c == '\t' = showString "\\t"
  | c == '\r' = id
  | ord c < 0x20 || (ord c >= 0x7f && ord c < 0x10000) = showString (u (ord c))
  | ord c >= 0x10000 = let n = ord c - 0x10000 in showString (u (0xD800 + n `div` 0x400)) . showString (u (0xDC00 + n `mod` 0x400))
  | c == '<' = showString "\\u003c"     -- no </script> in a script
  | otherwise = showChar c
  where
    u n = "\\u" ++ pad (hex n)
    pad s = replicate (4 - length s) '0' ++ s
    hex n = let (q, r) = n `divMod` 16 in (if q > 0 then hex q else "") ++ ["0123456789abcdef" !! r]

-- | A script that gives the viewer some JSON, by name.  (JSON is
-- JavaScript: the page turns it back into a string for the viewer.)
script :: String -> J -> String
script name j = "misoDocsData(" ++ render (JS name) ++ "," ++ render j ++ ");\n"

-- | The file of a module's data, in data/.
dataFileName :: String -> String
dataFileName mn = mn ++ ".js"

-- | The package, a summary of each module, and every name, for search.
indexScript :: Package -> String
indexScript pkg = script "index" $ JO
  [ ("name", JS (pkgName pkg))
  , ("version", JS (pkgVersion pkg))
  , ("synopsis", JS (pkgSynopsis pkg))
  , ("modules", JA [ JO [ ("n", JS (modName m))
                         , ("sum", JA (map inline (summary (modDoc m))))
                         , ("lines", JN (length (modSource m)))
                         , ("exports", JN (length [ () | Decl _ <- modItems m ])) ]
                   | m <- pkgModules pkg ])
  , ("defs", JA [ JA [JS n, JS k, JS (modName m), JS a]
                | m <- pkgModules pkg, Decl d <- modItems m, declFrom d == modName m
                , (n, k, a) <- (declName d, declKind d, declAnchor d)
                               : [ (subName s, subKind s, subAnchor s) | s <- declSubs d ++ concatMap subFields (declSubs d) ]
                               ++ [ (declName x, declKind x, declAnchor x) | x <- declMembers d ] ])
  ]
  where
    summary bs = case [ is | Para is <- bs ] of
      is : _ -> is
      [] -> []

moduleScript :: Module -> String
moduleScript m = script (modName m) $ JO
  [ ("name", JS (modName m))
  , ("doc", blocks (modDoc m))
  , ("items", JA (map item (modItems m)))
  , ("src", JA (map JS (modSource m)))
  ]

item :: Item -> J
item it = case it of
  Heading n t -> JA [JS "h", JN n, JS t]
  Prose bs -> JA [JS "p", blocks bs]
  Decl d -> JA [JS "d", decl d]
  ReExport m inlined -> JA [JS "re", JS m, JB inlined]

decl :: Declaration -> J
decl d = JO $
  [ ("n", JS (declName d)), ("a", JS (declAnchor d)), ("k", JS (declKind d)), ("s", JS (declSig d))
  , ("d", blocks (declDoc d)), ("l", JN (declLine d)), ("e", JN (declEnd d)), ("f", JS (declFrom d)) ]
  ++ [ ("m", JA (map decl (declMembers d))) | not (null (declMembers d)) ]
  ++ [ ("c", JA (map sub (declSubs d))) | not (null (declSubs d)) ]
  ++ [ ("args", JA [ JA [JS (argType a), blocks (argDoc a)] | a <- declArgs d ]) | not (null (declArgs d)) ]
  ++ [ ("i", JA (map inst (declInstances d))) | not (null (declInstances d)) ]
  ++ [ ("dep", JS (declDeprecated d)) | not (null (declDeprecated d)) ]

sub :: Sub -> J
sub s = JO $
  [ ("n", JS (subName s)), ("a", JS (subAnchor s)), ("k", JS (subKind s)), ("s", JS (subSig s)), ("d", blocks (subDoc s)) ]
  ++ [ ("fs", JA (map sub (subFields s))) | not (null (subFields s)) ]

inst :: Instance -> J
inst i = JO
  [ ("h", JS (instHead i)), ("d", blocks (instDoc i)), ("m", JS (instModule i))
  , ("l", JN (instLine i)), ("e", JN (instEnd i)), ("dv", JB (instDerived i)) ]

blocks :: [Block] -> J
blocks = JA . map block

block :: Block -> J
block b = case b of
  Para is -> JA [JS "p", inlines is]
  CodeBlock c -> JA [JS "code", JS (highlight (const False) (lexHaskell c))]
  Pre is -> JA [JS "pre", inlines is]
  Example e rs -> JA [JS "ex", JS e, JA (map JS rs)]
  Bullets xs -> JA [JS "ul", JA (map blocks xs)]
  Numbered xs -> JA [JS "ol", JA (map blocks xs)]
  Definitions xs -> JA [JS "dl", JA [ JA [inlines t, blocks d] | (t, d) <- xs ]]
  Table rows h -> JA [JS "table", JA [ JA (map inlines r) | r <- rows ], JN h]
  Header n is -> JA [JS "hd", JN n, inlines is]
  MathBlock m -> JA [JS "math", JS m]
  Since v -> JA [JS "since", JS v]

inlines :: [Inline] -> J
inlines = JA . map inline

inline :: Inline -> J
inline i = case i of
  Str s -> JS s
  Code s -> JA [JS "c", JS s]
  Mono is -> JA [JS "m", inlines is]
  Emph is -> JA [JS "e", inlines is]
  Strong is -> JA [JS "b", inlines is]
  Ref n u -> JA [JS "r", JS n, JS u]
  ModRef m k -> JA [JS "M", JS m, JB k]
  Link l u -> JA [JS "l", JS l, JS u]
  Image u t -> JA [JS "img", JS u, JS t]
  Math m -> JA [JS "x", JS m]
  Anchor a -> JA [JS "a", JS a]
