-- | A Haskell lexer for miso docs: enough to find the declarations of a
-- module and their comments, and to highlight source.  Every character of
-- the input is in a token, so the tokens put back together are the input.
module Miso.CLI.Docs.Lex
  ( Tok (..)
  , Kind (..)
  , lexHaskell
  , significant
  , tokEndLine
  , highlight
  , highlightLines
  , escapeHtml
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isHexDigit, isSpace, isUpper)

data Kind
  = Space | Comment | Pragma | StringLit | CharLit | Number
  | Var | Con | Keyword | Op | Special | Cpp
  deriving (Eq, Show)

data Tok = Tok
  { tKind :: Kind
  , tText :: String
  , tLine :: Int   -- ^ from 1
  , tCol  :: Int   -- ^ from 1
  } deriving (Show)

-- | The tokens that are not white space.
significant :: [Tok] -> [Tok]
significant = filter ((/= Space) . tKind)

-- | The line a token ends on.
tokEndLine :: Tok -> Int
tokEndLine t = tLine t + length (filter (== '\n') (tText t))

lexHaskell :: String -> [Tok]
lexHaskell = go 1 1
  where
    go _ _ [] = []
    go l c s@(x : xs)
      | c == 1 && x == '#' = emit Cpp (cppLine s)
      | isSpace x = emit Space (span isSpace s)
      | prefix "{-#" s = emit Pragma (nested 0 s)
      | prefix "{-" s = emit Comment (nested 0 s)
      | isLineComment s = emit Comment (break (== '\n') s)
      | x == '"' = emit StringLit (stringLit xs)
      | x == '\'' , Just r <- charLit xs = emit CharLit r
      | isDigit x = emit Number (number s)
      | isAlpha x || x == '_' = let (w, r) = ident s in emit (identKind w) (w, r)
      | isSym x = let (o, r) = span isSym s in emit Op (o, r)
      | otherwise = emit Special ([x], xs)
      where
        emit k (t, rest) = Tok k t l c : uncurry go (advance l c t) rest

    advance l c [] = (l, c)
    advance l c (ch : cs)
      | ch == '\n' = advance (l + 1) 1 cs
      | otherwise = advance l (c + 1) cs

    prefix p s = take (length p) s == p

    -- a CPP line, with its continuation lines
    cppLine s =
      case break (== '\n') s of
        (a, '\n' : b) | not (null a) && last a == '\\' -> let (a', b') = cppLine b in (a ++ "\n" ++ a', b')
        r -> r

    -- {- ... -}, nested; the depth is that of the comments around
    nested :: Int -> String -> (String, String)
    nested d s
      | prefix "{-" s = let (a, b) = nested (d + 1) (drop 2 s) in ("{-" ++ a, b)
      | prefix "-}" s = if d == 1 then ("-}", drop 2 s) else let (a, b) = nested (d - 1) (drop 2 s) in ("-}" ++ a, b)
      | otherwise = case s of
          [] -> ([], [])
          ch : cs -> let (a, b) = nested d cs in (ch : a, b)

    -- -- starts a comment when the dashes are not part of an operator
    isLineComment s =
      let (ds, r) = span (== '-') s
      in length ds >= 2 && case r of { ch : _ -> not (isSym ch); [] -> True }

    stringLit s =
      let (a, b) = str s in ('"' : a, b)
    str [] = ([], [])
    str ('\\' : ch : cs) = let (a, b) = str cs in ('\\' : ch : a, b)
    str ('"' : cs) = ("\"", cs)
    str s@('\n' : _) = ([], s)
    str (ch : cs) = let (a, b) = str cs in (ch : a, b)

    -- 'a' and '\n'; otherwise a quote is a TH name or a promoted constructor
    charLit s =
      case s of
        '\\' : cs -> case break (== '\'') cs of
                       (e, '\'' : r) | length e <= 10 && not (any (== '\n') e) -> Just ("'\\" ++ e ++ "'", r)
                       _ -> Nothing
        ch : '\'' : r | ch /= '\n' -> Just (['\'', ch, '\''], r)
        _ -> Nothing

    number s =
      case s of
        '0' : x : cs | x `elem` "xX" -> let (a, b) = span (\ch -> isHexDigit ch || ch == '_') cs in ('0' : x : a, b)
        _ -> let (a, b) = span (\ch -> isDigit ch || ch == '_') s
             in case b of
                  '.' : d : cs | isDigit d -> let (f, r) = span (\ch -> isDigit ch || ch == '_') (d : cs)
                                                  (e, r') = expo r
                                              in (a ++ "." ++ f ++ e, r')
                  _ -> let (e, r) = expo b in (a ++ e, r)
    expo (e : cs) | e `elem` "eE" =
      case cs of
        sg : d : r | sg `elem` "+-" && isDigit d -> let (a, b) = span isDigit (d : r) in (e : sg : a, b)
        d : r | isDigit d -> let (a, b) = span isDigit (d : r) in (e : a, b)
        _ -> ([], e : cs)
    expo s = ([], s)

    -- an identifier, qualified with modules (M.x, M.T, M.+)
    ident s =
      let (w, r) = span isIdChar s
      in case r of
           '.' : r'@(ch : _) | isUpper (head w) && (isAlpha ch || ch == '_') ->
             let (w', r'') = ident r' in (w ++ "." ++ w', r'')
           '.' : r'@(ch : _) | isUpper (head w) && isSym ch ->
             let (o, r'') = span isSym r' in (w ++ "." ++ o, r'')
           _ -> (w, r)

    identKind w
      | w `elem` keywords = Keyword
      | isSym (last w) = Op
      | isUpper (head (lastPart w)) = Con
      | otherwise = Var
    lastPart w = case break (== '.') w of
                   (a, []) -> a
                   (_, _ : b) -> if null b then w else lastPart b

isIdChar :: Char -> Bool
isIdChar ch = isAlphaNum ch || ch == '_' || ch == '\''

isSym :: Char -> Bool
isSym ch = ch `elem` "!#$%&*+./<=>?@\\^|-~:" || (ch > '\x7f' && not (isAlphaNum ch) && not (isSpace ch) && ch `notElem` "()[]{},;`'\"")

keywords :: [String]
keywords =
  [ "case", "class", "data", "default", "deriving", "do", "else", "family", "foreign"
  , "forall", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let"
  , "mdo", "module", "newtype", "of", "pattern", "proc", "rec", "then", "type", "where" ]

-----------------------------------------------------------------------------
-- HTML

escapeHtml :: String -> String
escapeHtml = concatMap esc
  where
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '&' = "&amp;"
    esc '"' = "&quot;"
    esc ch = [ch]

cls :: Kind -> String
cls k =
  case k of
    Comment -> "hl-com"
    Pragma -> "hl-pragma"
    StringLit -> "hl-str"
    CharLit -> "hl-str"
    Number -> "hl-num"
    Con -> "hl-ty"
    Keyword -> "hl-kw"
    Op -> "hl-op"
    Cpp -> "hl-cpp"
    _ -> ""

-- | Highlighted HTML, with the tokens of a name stressed.
highlight :: (Tok -> Bool) -> [Tok] -> String
highlight isName = concatMap tok
  where
    tok t
      | isName t = span' "hl-name" (tText t)
      | otherwise = span' (cls (tKind t)) (tText t)

-- | Highlighted HTML, a string for each line (its spans closed in it).
highlightLines :: [Tok] -> [String]
highlightLines ts =
  let ls = splitLines [ (cls (tKind t), tText t) | t <- ts ]
      ls' = if not (null ls) && null (last ls) then init ls else ls
  in [ concat [ span' c s | (c, s) <- l ] | l <- ls' ]

-- | Segments split at their newlines into lines of segments.
splitLines :: [(String, String)] -> [[(String, String)]]
splitLines = go []
  where
    go cur [] = [reverse cur]
    go cur ((c, s) : rest) =
      case break (== '\n') s of
        (a, []) -> go (add c a cur) rest
        (a, _ : b) -> reverse (add c a cur) : go [] ((c, b) : rest)
    add _ [] cur = cur
    add c a cur = (c, a) : cur

span' :: String -> String -> String
span' "" s = escapeHtml s
span' c s = "<span class=\"" ++ c ++ "\">" ++ escapeHtml s ++ "</span>"
