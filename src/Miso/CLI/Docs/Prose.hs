-- | Documentation as HTML: the viewer sets it as it is (innerHTML), so
-- reading a module's data and drawing its page do not have a node for each
-- word, code span and link of its documentation.  The elements and classes
-- are those the viewer drew before (see docs.css).
module Miso.CLI.Docs.Prose
  ( blocksHtml
  , summaryHtml
  ) where

import Data.List (isPrefixOf)

import Miso.CLI.Docs.Lex (escapeHtml, highlight, lexHaskell)
import Miso.CLI.Docs.Markup (Block (..), Inline (..))

el :: String -> [(String, String)] -> String -> String
el t as c = "<" ++ t ++ concat [ " " ++ k ++ "=\"" ++ escapeHtml v ++ "\"" | (k, v) <- as ] ++ ">" ++ c ++ "</" ++ t ++ ">"

cls :: String -> [(String, String)]
cls c = [("class", c)]

-- | The HTML of documentation, in a module (for the ids of its anchors).
blocksHtml :: String -> [Block] -> String
blocksHtml mn = concatMap block
  where
    block b = case b of
      Para is -> el "p" [] (inlines is)
      CodeBlock c -> el "pre" (cls "code") (el "code" [] (highlight (const False) (lexHaskell c)))
      Pre is -> el "pre" (cls "code pre-rich") (el "code" [] (inlines is))
      Example e rs ->
        el "div" (cls "example") $
          el "div" (cls "example-input") (el "span" (cls "prompt") "&gt;&gt;&gt;" ++ el "code" [] (escapeHtml e))
          ++ (if null rs then "" else el "pre" (cls "example-output") (escapeHtml (joinLines rs)))
      Property e ->
        el "div" (cls "example") (el "div" (cls "example-input") (el "span" (cls "prompt") "prop&gt;" ++ el "code" [] (escapeHtml e)))
      Bullets xs -> el "ul" [] (concat [ el "li" [] (blocksHtml mn x) | x <- xs ])
      Numbered xs -> el "ol" [] (concat [ el "li" [] (blocksHtml mn x) | x <- xs ])
      Definitions xs -> el "dl" [] (concat [ el "dt" [] (inlines t) ++ el "dd" [] (blocksHtml mn d) | (t, d) <- xs ])
      Table rows h ->
        el "div" (cls "table-wrap") $ el "table" [] $
          el "thead" [] (concat [ el "tr" [] (concat [ el "th" [] (inlines c) | c <- r ]) | r <- take h rows ])
          ++ el "tbody" [] (concat [ el "tr" [] (concat [ el "td" [] (inlines c) | c <- r ]) | r <- drop h rows ])
      Header n is -> el (if n <= 1 then "h3" else "h4") (cls "doc-heading") (inlines is)
      MathBlock t -> el "div" [("class", "math display"), ("data-tex", t)] (escapeHtml t)
      Since v -> el "div" (cls "since") ("Since " ++ escapeHtml v)
    inlines = inlinesHtml True mn

-- | The first sentence of documentation, without links (a card is a link).
summaryHtml :: [Block] -> String
summaryHtml bs = case [ is | Para is <- bs ] of
  is : _ -> inlinesHtml False "" (cut is)
  [] -> ""
  where
    cut is = case is of
      Str s : rest -> case sentence s of
        Just a -> [Str (a ++ ".")]
        Nothing -> Str s : cut rest
      i : rest -> i : cut rest
      [] -> []
    sentence s
      | ". " `isPrefixOf` s = Just ""
      | otherwise = case s of
          [] -> Nothing
          c : cs -> (c :) <$> sentence cs

-- | The HTML of inlines, with links or without.
inlinesHtml :: Bool -> String -> [Inline] -> String
inlinesHtml links mn = concatMap inline
  where
    inline i = case i of
      Str s -> escapeHtml s
      Code c -> el "code" [] (escapeHtml c)
      Mono is -> el "code" (cls "mono") (inlinesHtml links mn is)
      Emph is -> el "em" [] (inlinesHtml links mn is)
      Strong is -> el "strong" [] (inlinesHtml links mn is)
      Ref n u | not links -> el "code" [] (escapeHtml n)
              | null u -> el "code" (cls "ref unresolved") (escapeHtml n)
              | otherwise -> el "a" [("class", "ref"), ("href", u)] (el "code" [] (escapeHtml n))
      ModRef m known
        | links && known -> el "a" [("class", "modref"), ("href", "#/" ++ m)] (escapeHtml m)
        | otherwise -> el "span" (cls ("modref" ++ if known then "" else " unresolved")) (escapeHtml m)
      Link l u | links -> el "a" [("class", "external"), ("href", u)] (escapeHtml l)
               | otherwise -> escapeHtml l
      Image u t -> "<img src=\"" ++ escapeHtml u ++ "\" alt=\"" ++ escapeHtml t ++ "\" title=\"" ++ escapeHtml t ++ "\" class=\"doc-image\">"
      Math t -> el "span" [("class", "math"), ("data-tex", t)] (escapeHtml t)
      Anchor a -> el "span" [("class", "anchor"), ("id", "/" ++ mn ++ "/#" ++ a)] ""

joinLines :: [String] -> String
joinLines [] = ""
joinLines xs = foldr1 (\a b -> a ++ "\n" ++ b) xs
