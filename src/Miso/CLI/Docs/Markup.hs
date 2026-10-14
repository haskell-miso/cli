-- | Haddock's markup: paragraphs, code blocks (bird tracks and @ blocks,
-- with markup in them), examples (>>>), lists (nested), definition lists,
-- tables, headings, display math, \@since, and in text, \@code\@,
-- 'identifiers', \"Modules\", /emphasis/, __bold__, links, images, math
-- and anchors.
module Miso.CLI.Docs.Markup
  ( Block (..)
  , Inline (..)
  , Resolve (..)
  , parseDoc
  , commentLines
  , plainText
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)
import Data.List (isPrefixOf, isSuffixOf)

data Block
  = Para [Inline]
  | CodeBlock String              -- ^ code, highlighted later
  | Pre [Inline]                  -- ^ an @ block with markup in it
  | Example String [String]
  | Bullets [[Block]]
  | Numbered [[Block]]
  | Definitions [([Inline], [Block])]
  | Table [[[Inline]]] Int        -- ^ rows of cells, and how many are header rows
  | Header Int [Inline]
  | MathBlock String
  | Since String
  deriving (Show)

data Inline
  = Str String
  | Code String
  | Mono [Inline]                 -- ^ code with references in it: @'f' x@
  | Emph [Inline]
  | Strong [Inline]
  | Ref String String             -- ^ an identifier, and its URL ("" for none)
  | ModRef String Bool
  | Link String String
  | Image String String           -- ^ URL and title
  | Math String
  | Anchor String
  deriving (Show)

-- | Where identifiers and modules are.
data Resolve = Resolve
  { resolveName   :: String -> String   -- ^ the URL of an identifier, or ""
  , resolveModule :: String -> Bool     -- ^ whether a module is documented
  }

-- | The text of inlines, without markup.
plainText :: [Inline] -> String
plainText = concatMap one
  where
    one i = case i of
      Str s -> s
      Code s -> s
      Mono is -> plainText is
      Emph is -> plainText is
      Strong is -> plainText is
      Ref n _ -> n
      ModRef m _ -> m
      Link l _ -> l
      Image _ t -> t
      Math m -> m
      Anchor _ -> ""

-- | The text of a doc comment: -- | lines, or {-| -}, without the comment
-- marks (and the | or ^ of the first line).
commentLines :: [String] -> [String]
commentLines cs = dedent (concat (zipWith one [0 :: Int ..] cs))
  where
    -- the | or ^ is the mark of the first line only (in the others, | is text: a table)
    one i c
      | i > 0 && "--" `isPrefixOf` c = [ dropWhile (== '-') c ]
      | otherwise = one' c
    one' c
      | "{-" `isPrefixOf` c =
          let body = dropEnd 2 (drop 2 c)
              body' = case dropWhile (== ' ') body of { m : r | m `elem` "|^" -> r; _ -> body }
          in lines body'
      | otherwise =
          let r = dropWhile (== '-') c
          in [ case dropWhile (== ' ') r of { m : r' | m `elem` "|^" -> r'; _ -> r } ]
    dropEnd n s = take (length s - n) s
    dedent ls =
      let ind = [ length (takeWhile (== ' ') l) | l <- ls, not (all isSpace l) ]
          n = if null ind then 0 else minimum ind
      in trimBlank (map (drop n . trimEnd) ls)
    trimBlank = reverse . dropWhile (all isSpace) . reverse . dropWhile (all isSpace)

trimEnd :: String -> String
trimEnd = reverse . dropWhile isSpace . reverse

trim :: String -> String
trim = dropWhile isSpace . trimEnd

indentOf :: String -> Int
indentOf = length . takeWhile (== ' ')

blank :: String -> Bool
blank = all isSpace

parseDoc :: Resolve -> [String] -> [Block]
parseDoc r = blocks
  where
    blocks [] = []
    blocks (l : ls)
      | blank l = blocks ls
      | ">>>" `isPrefixOf` t =
          let (res, rest) = break (\x -> blank x || ">>>" `isPrefixOf` trim x) ls
          in Example (trim (drop 3 t)) (map trim res) : blocks rest
      | isBird l =
          let (code, rest) = span isBird (l : ls)
          in CodeBlock (unlines' (map unbird code)) : blocks rest
      | t == "@" =
          let (code, rest) = break ((== "@") . trim) ls
              body = dedent code
          in (if any hasMarkup body then Pre (inlinesKeep (joinWith "\n" body)) else CodeBlock (unlines' (map unescape body)))
             : blocks (drop 1 rest)
      | "\\[" `isPrefixOf` t =
          let (m, rest) = mathBlock (drop 2 t : ls)
          in MathBlock (trim m) : blocks rest
      | "@since" `isPrefixOf` t = Since (trim (drop 6 t)) : blocks ls
      | isTableLine t =
          let (rows, rest) = span (isTableLine . trim) (l : ls)
          in table (map trim rows) : blocks rest
      | Just (n, h) <- heading t = Header n (inlines h) : blocks ls
      | Just _ <- bullet t = let (items, rest) = list bullet (l : ls) in Bullets items : blocks rest
      | Just _ <- numbered t = let (items, rest) = list numbered (l : ls) in Numbered items : blocks rest
      | Just _ <- definition t = let (items, rest) = defs (l : ls) in Definitions items : blocks rest
      | otherwise =
          let (para, rest) = break stop ls
          in Para (inlines (unwords (map trim (l : para)))) : blocks rest
      where t = trim l

    stop x = blank x || isBird x || startsBlock (trim x) || isTableLine (trim x)
             || "@since" `isPrefixOf` trim x || "\\[" `isPrefixOf` trim x

    isBird x = ">" `isPrefixOf` x && not (">>>" `isPrefixOf` x)
    unbird x = case drop 1 x of { ' ' : rest -> rest; rest -> rest }
    unlines' = trimEnd . unlines
    dedent ls =
      let ind = [ indentOf x | x <- ls, not (blank x) ]
      in map (drop (if null ind then 0 else minimum ind)) ls

    hasMarkup x = any (`elem` "'\"") x || contains "__" x
    contains p s = any (p `isPrefixOf`) (tailsOf s)
    tailsOf s = s : case s of { [] -> []; _ : cs -> tailsOf cs }
    joinWith sep xs = case xs of
      [] -> ""
      _ -> foldr1 (\a b -> a ++ sep ++ b) xs

    mathBlock ls =
      case breakOn "\\]" (joinWith "\n" ls) of
        (m, _ : _ : rest) -> (m, lines rest)
        (m, _) -> (m, [])

    heading x =
      let (eqs, rest) = span (== '=') x
      in if not (null eqs) && length eqs <= 6 && take 1 rest == " " then Just (length eqs, trim rest) else Nothing

    bullet x = case x of
      c : ' ' : rest | c `elem` "*-" -> Just rest
      _ -> Nothing
    numbered x = case x of
      '(' : rest | (ds@(_ : _), ')' : ' ' : r') <- span isDigit rest -> const (Just r') ds
      _ | (ds@(_ : _), '.' : ' ' : r') <- span isDigit x -> const (Just r') ds
      _ -> Nothing
    -- [term]: definition, or [term] definition
    definition x = case x of
      '[' : rest | (term, ']' : more) <- breakBracket 0 rest, take 1 more /= "(" ->
        Just (term, trim (case more of { ':' : m -> m; m -> m }))
      _ -> Nothing
    breakBracket :: Int -> String -> (String, String)
    breakBracket _ [] = ([], [])
    breakBracket d (c : cs)
      | c == ']' && d == 0 = ([], c : cs)
      | otherwise = let d' = if c == '[' then d + 1 else if c == ']' then d - 1 else d
                        (a, b) = breakBracket d' cs in (c : a, b)

    -- items: each its first line and the lines after it that belong to it,
    -- parsed as blocks (so items can have lists and code in them)
    list item ls =
      case ls of
        x : rest | Just first <- item (trim x) ->
          let (body, rest') = itemBody (indentOf x) rest
              (items, rest'') = list item (dropBlankBefore item rest')
          in (blocks (first : dedent body) : items, if null items then rest' else rest'')
        _ -> ([], ls)
    defs ls =
      case ls of
        x : rest | Just (term, first) <- definition (trim x) ->
          let (body, rest') = itemBody (indentOf x) rest
              (items, rest'') = defs (dropBlankBefore (fmap fst . definition) rest')
          in ((inlines term, blocks (first : dedent body)) : items, if null items then rest' else rest'')
        _ -> ([], ls)
    -- the lines of an item: indented more than it, or (not after a blank
    -- line) lines that do not start a block of their own
    itemBody ind ls =
      case ls of
        y : ys | blank y ->
          case dropWhile blank ys of
            z : _ | indentOf z > ind -> let (a, b) = itemBody ind ys in (y : a, b)
            _ -> ([], ls)
        y : ys | indentOf y > ind || not (startsBlock (trim y)) ->
          let (a, b) = itemBody ind ys in (y : a, b)
        _ -> ([], ls)
    startsBlock y = bullet y /= Nothing || numbered y /= Nothing || definition y /= Nothing
                    || ">>>" `isPrefixOf` y || isBird y || y == "@" || heading y /= Nothing
    dropBlankBefore item ys = case span blank ys of
      (_, z : zs) | item (trim z) /= Nothing -> z : zs
      _ -> ys

    -- +---+---+ grid tables; a +===+ line ends the header rows
    isTableLine x = "+-" `isPrefixOf` x || "+=" `isPrefixOf` x || ("|" `isPrefixOf` x && "|" `isSuffixOf` x && length x > 1)
    table rows =
      let groups = splitRows rows
          headerRows = case break fst groups of
            (_, []) -> 0
            (before, _ : _) -> length before + 1
          cells g = [ inlines (trim (unwords c)) | c <- transposeCells [ splitCells x | x <- g ] ]
      in Table [ cells g | (_, g) <- groups ] headerRows
    -- the rows between separator lines, and whether a === line ends them
    splitRows ls = go [] ls
      where
        go acc [] = [ (False, reverse acc) | not (null acc) ]
        go acc (x : xs)
          | "+" `isPrefixOf` x = (if null acc then id else ((('=' `elem` x), reverse acc) :)) (go [] xs)
          | otherwise = go (x : acc) xs
    splitCells x = map trim (splitOnBar (drop 1 (take (length x - 1) x)))
    splitOnBar s = case break (== '|') s of
      (a, []) -> [a]
      (a, _ : b) -> a : splitOnBar b
    transposeCells [] = []
    transposeCells rs
      | any null rs = []
      | otherwise = map head rs : transposeCells (map tail rs)

    unescape = go
      where go ('\\' : c : cs) = c : go cs
            go (c : cs) = c : go cs
            go [] = []

    inlines = merge . inl False
    inlinesKeep = merge . inl True

    merge (Str a : Str b : rest) = merge (Str (a ++ b) : rest)
    merge (x : rest) = x : merge rest
    merge [] = []

    -- the inlines of a string; in an @ block (keep), white space stays
    inl :: Bool -> String -> [Inline]
    inl keep s = case s of
      [] -> []
      '\\' : '(' : cs | (m, _ : _ : rest) <- breakOn "\\)" cs -> Math m : go rest
      '\\' : '[' : cs | (m, _ : _ : rest) <- breakOn "\\]" cs -> Math m : go rest
      '\\' : c : cs -> Str [c] : go cs
      '<' : '<' : cs | (u, '>' : '>' : rest) <- breakOn ">>" cs ->
        let (url, title) = break isSpace u in Image url (trim title) : go rest
      '@' : cs | not keep, (code, '@' : rest) <- breakEsc (== '@') cs ->
        (if any (`elem` "'\"") code then Mono (merge (inl True code)) else Code (unescape code)) : go rest
      '`' : cs | (n, '`' : rest) <- break (== '`') cs, all isRefChar n, isName n -> ref n : go rest
      '\'' : cs | (n, '\'' : rest) <- break (== '\'') cs, all isRefChar n, isName n -> ref n : go rest
      c : '\'' : cs | c `elem` "vt", (n, '\'' : rest) <- break (== '\'') cs, all isRefChar n, isName n -> ref n : go rest
      '"' : cs | (n, '"' : rest) <- span (\c -> isAlphaNum c || c `elem` "._'#") cs, isModule (takeWhile (/= '#') n) ->
        let m = takeWhile (/= '#') n
            a = drop 1 (dropWhile (/= '#') n)
            known = resolveModule r m
        in (if null a || not known then ModRef m known else Link n ("#/" ++ m ++ "/#" ++ a)) : go rest
      '#' : cs | (a, '#' : rest) <- span (\c -> isAlphaNum c || c `elem` "_-:.") cs, not (null a) -> Anchor a : go rest
      '_' : '_' : cs | (b, '_' : '_' : rest) <- breakOn "__" cs, not (null b) -> Strong (inl keep b) : go rest
      '/' : cs | (e, '/' : rest) <- break (`elem` "/\n") cs, emphOk e rest -> Emph (inl keep e) : go rest
      '<' : cs | (u, '>' : rest) <- break (`elem` "<>") cs, isUrl u ->
        let (url, label) = break isSpace u
        in Link (if null label then url else trim label) url : go rest
      '[' : cs | (label, ']' : '(' : more) <- break (== ']') cs, (url, ')' : rest) <- break (== ')') more ->
        Link label url : go rest
      c : cs | isSpace c && keep -> let (sp, rest) = span isSpace cs in Str (c : sp) : go rest
      c : cs | isAlphaNum c -> let (plain, rest) = break special cs in wordThen (c : plain) rest
      -- one character at a time after punctuation, so a word after it that
      -- starts a reference (t'T', v'f') is seen as one
      c : cs -> Str [c] : go cs
      where
        go = inl keep
        -- a word: a quote in it (don't) is not a reference
        wordThen w rest =
          case rest of
            '\'' : cs | not (null w) && isAlphaNum (last w) -> Str (w ++ "'") : go cs
            _ -> Str w : go rest

    special c = c `elem` "\\@`'\"_/<[#" || isSpace c

    breakEsc p xs = case xs of
      '\\' : c : cs -> let (a, b) = breakEsc p cs in ('\\' : c : a, b)
      c : cs | p c -> ([], c : cs)
             | otherwise -> let (a, b) = breakEsc p cs in (c : a, b)
      [] -> ([], [])

    breakOn pat xs
      | pat `isPrefixOf` xs = ([], xs)
      | otherwise = case xs of
          [] -> ([], [])
          c : cs -> let (a, b) = breakOn pat cs in (c : a, b)

    emphOk e rest =
      not (null e) && not (isSpace (head e)) && not (isSpace (last e))
      && case rest of { c : _ -> not (isAlphaNum c); [] -> True }

    isUrl u = any (`isPrefixOf` u) ["http://", "https://", "mailto:", "ftp://"]

    isRefChar c = isAlphaNum c || c `elem` "_'.!#$%&*+/<=>?@\\^|-~:"
    isName n = not (null n) && (isAlpha (head n) || head n == '_' || all (`elem` "!#$%&*+./<=>?@\\^|-~:") n || qualified n)
    qualified n = case break (== '.') n of { (m@(c : _), '.' : _) -> isUpper c && all isAlphaNum m; _ -> False }

    isModule n = not (null n) && all part (splitDots n)
      where part p = not (null p) && isUpper (head p) && all (\c -> isAlphaNum c || c `elem` "_'") p
    splitDots s = case break (== '.') s of
      (a, []) -> [a]
      (a, _ : b) -> a : splitDots b

    ref n = Ref n (resolveName r n)
