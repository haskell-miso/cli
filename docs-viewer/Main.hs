{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The documentation viewer of miso docs: a miso app that shows the data
-- of data/ (see Docs.Types).  The index comes with the page (data/index.js);
-- a module's data is loaded when its page is shown.  Pages are named by the
-- URL hash:
--
-- > #/                 the package
-- > #/M.N              a module
-- > #/M.N/ANCHOR       a declaration (or constructor, field) of a module
-- > #/M.N/src          the source of a module
-- > #/M.N/src/L-E      the source, with lines L to E marked
-- > #/:index           every name, A to Z
--
-- The hash is read from window.location (miso's URI parser does not take
-- these hashes).  The script of index.html loads data files, scrolls to
-- what a hash names once it is drawn, and draws math (with KaTeX).
module Main (main) where

import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Miso hiding (Image (..), Sub)
import qualified Miso.CSS as CSS
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as E
import qualified Miso.Html.Property as P
import Miso.JSON (eitherDecode)
import qualified Miso.String as S

import Docs.Types

data Route
  = Home
  | IndexPage
  | ModulePage MisoString
  | SourcePage MisoString Int Int  -- ^ a module, and the lines to mark (0 0 for none)
  deriving (Eq)

data Theme = System | Light | Dark
  deriving (Eq)

data Model = Model
  { route    :: Route
  , query    :: MisoString
  , theme    :: Theme
  , menuOpen :: Bool
  , loaded   :: M.Map MisoString Module
  , missing  :: [MisoString]
  , palette  :: Bool          -- ^ the search palette is open
  , selected :: Int           -- ^ the result it has selected
  } deriving (Eq)

data Action
  = SetHash MisoString
  | SetQuery MisoString
  | NextTheme
  | ToggleMenu
  | Loaded MisoString (Maybe MisoString)
  | OpenPalette
  | ClosePalette
  | PaletteKey KeyCode
  | Select Int

main :: IO ()
main = do
  h <- getHash
  raw <- getData "index"
  case maybe (Left "no data/index.js") eitherDecode raw of
    Left e -> startApp defaultEvents (component () (\_ -> pure ()) (\_ -> broken e))
    Right ix -> startApp defaultEvents (app ix (Model (routeOf h) "" System False M.empty [] False 0))

-- | The page when the data cannot be read.
broken :: MisoString -> View context props () action
broken e =
  H.div_ [ P.class_ "app" ]
    [ H.div_ [ P.class_ "empty" ]
        [ H.h1_ [] [ text "These docs cannot be shown" ]
        , H.p_ [] [ text e ]
        ]
    ]

app :: Index -> Model -> App Model Action
app ix m = (component m (update_ ix) (view_ ix)) { subs = [ hashSub SetHash ], mount = Just (SetHash (hashOf (route m))) }

-- | The hash of the page's URL.
getHash :: IO MisoString
getHash = do
  loc <- jsg "location"
  h <- loc ! "hash"
  fromMaybe "" <$> castJSVal h

-- | The data a script of data/ gave, if it is here.
getData :: MisoString -> IO (Maybe MisoString)
getData name = do
  docs <- jsg "misoDocs"
  d <- docs ! "data"
  v <- d ! name
  castJSVal v

-- | Load a module's data (data/NAME.js), and then the action.
loadModule :: MisoString -> (Maybe MisoString -> IO ()) -> IO ()
loadModule name k = do
  cb <- asyncCallback1 (\v -> castJSVal v >>= k)
  w <- jsg "window"
  _ <- w # "misoDocsLoad" $ (name, cb)
  pure ()

-- | The hash, whenever it changes.
hashSub :: (MisoString -> action) -> (action -> IO ()) -> IO model -> IO ()
hashSub f sink _ = do
  w <- jsg "window"
  _ <- addEventListener w "hashchange" (\_ -> getHash >>= sink . f)
  pure ()

update_ :: Index -> Action -> Effect parent props Model Action
update_ ix = \case
  SetHash h -> do
    modify (\m -> m { route = routeOf h, menuOpen = False })
    m <- get
    case routeModule (route m) of
      Just mn | not (M.member mn (loaded m)) && mn `notElem` missing m && known mn ->
        withSink (\sink -> loadModule mn (sink . Loaded mn))
      _ -> pure ()
  Loaded mn r ->
    case r >>= either (const Nothing) Just . eitherDecode of
      Just mo -> modify (\m -> m { loaded = M.insert mn mo (loaded m) })
      Nothing -> modify (\m -> m { missing = mn : missing m })
  SetQuery q -> modify (\m -> m { query = q, selected = 0 })
  OpenPalette -> modify (\m -> m { palette = True, selected = 0 })
  ClosePalette -> modify (\m -> m { palette = False })
  Select i -> modify (\m -> m { selected = i })
  PaletteKey (KeyCode k) -> do
    m <- get
    let rs = results ix (query m)
        n = length rs
    case k of
      27 -> modify (\m' -> m' { palette = False })
      40 -> modify (\m' -> m' { selected = if n == 0 then 0 else (selected m' + 1) `mod` n })
      38 -> modify (\m' -> m' { selected = if n == 0 then 0 else (selected m' - 1) `mod` n })
      13 -> case drop (selected m) rs of
        r : _ -> do
          modify (\m' -> m' { palette = False, query = "" })
          io_ (go (resHref r))
        [] -> pure ()
      _ -> pure ()
  NextTheme -> modify (\m -> m { theme = next (theme m) })
  ToggleMenu -> modify (\m -> m { menuOpen = not (menuOpen m) })
  where
    known mn = mn `elem` map sumName (ixModules ix)
    go href = do
      w <- jsg "window"
      _ <- w # "misoDocsGo" $ href
      pure ()
    next System = Dark
    next Dark = Light
    next Light = System

-----------------------------------------------------------------------------
-- Routes

-- | The route of a hash: split at the slashes.
routeOf :: MisoString -> Route
routeOf h =
  case filter (not . S.null) (S.splitOn "/" (S.dropWhile (== '#') h)) of
    [] -> Home
    [":index"] -> IndexPage
    [mn] -> ModulePage mn
    [mn, "src"] -> SourcePage mn 0 0
    [mn, "src", l] -> sourceAt mn (S.unpack l)
    mn : _ -> ModulePage mn
  where
    sourceAt mn l =
      case break (== '-') l of
        (a, '-' : b) | isNum a && isNum b -> SourcePage mn (read a) (read b)
        (a, "") | isNum a -> SourcePage mn (read a) (read a)
        _ -> SourcePage mn 0 0
    isNum s = not (null s) && all (`elem` ("0123456789" :: String)) s

hashOf :: Route -> MisoString
hashOf = \case
  Home -> "#/"
  IndexPage -> "#/:index"
  ModulePage mn -> "#/" <> mn
  SourcePage mn l e -> sourceHref mn l e

routeModule :: Route -> Maybe MisoString
routeModule = \case
  Home -> Nothing
  IndexPage -> Nothing
  ModulePage mn -> Just mn
  SourcePage mn _ _ -> Just mn

moduleHref :: MisoString -> MisoString
moduleHref mn = "#/" <> mn

declHref :: MisoString -> MisoString -> MisoString
declHref mn a = "#/" <> mn <> "/" <> a

sourceHref :: MisoString -> Int -> Int -> MisoString
sourceHref mn 0 _ = "#/" <> mn <> "/src"
sourceHref mn l e = "#/" <> mn <> "/src/" <> ms l <> (if e > l then "-" <> ms e else "")

-----------------------------------------------------------------------------
-- The page

view_ :: Index -> Model -> View context props Model Action
view_ ix m =
  H.div_
    [ P.classList_
        [ ("app", True)
        , ("theme-dark", theme m == Dark)
        , ("theme-light", theme m == Light)
        , ("menu-open", menuOpen m)
        ]
    ]
    [ topBar ix m
    , H.div_ [ P.class_ "layout" ]
        [ sidebar ix m
        , H.main_ [ P.class_ "content" ] (page ix m)
        , toc m
        ]
    , if palette m then searchPalette ix m else text ""
    ]

topBar :: Index -> Model -> View context props Model Action
topBar ix m =
  H.header_ [ P.class_ "topbar" ]
    [ H.button_ [ P.class_ "icon-button menu-button", E.onClick ToggleMenu, P.title_ "Modules" ] [ icon "menu" ]
    , H.a_ [ P.class_ "brand", P.href_ "#/" ]
        [ H.span_ [ P.class_ "brand-mark" ] [ text "λ" ]
        , H.span_ [ P.class_ "brand-name" ] [ text (ixName ix) ]
        , H.span_ [ P.class_ "version" ] [ text (ixVersion ix) ]
        ]
    , H.button_ [ P.class_ "search", P.id_ "search-button", E.onClick OpenPalette ]
        [ icon "search"
        , H.span_ [ P.class_ "search-label" ] [ text "Search modules and definitions" ]
        , H.kbd_ [] [ text "/" ]
        ]
    , H.button_ [ P.class_ "icon-button", E.onClick NextTheme, P.title_ (themeLabel (theme m)) ]
        [ icon (themeIcon (theme m)) ]
    ]
  where
    themeLabel System = "Theme: system"
    themeLabel Dark = "Theme: dark"
    themeLabel Light = "Theme: light"
    themeIcon System = "contrast"
    themeIcon Dark = "moon"
    themeIcon Light = "sun"

-- | The modules, and the index of every name.
sidebar :: Index -> Model -> View context props Model Action
sidebar ix m =
  H.aside_ [ P.class_ "sidebar" ]
    [ H.nav_ []
        [ H.a_ [ P.classList_ [ ("nav-module", True), ("active", route m == IndexPage) ], P.href_ "#/:index" ]
            [ icon "list", text " Index" ] ]
    , H.div_ [ P.class_ "sidebar-title" ] [ text "Modules" ]
    , H.nav_ [] [ moduleLink (sumName s) | s <- ixModules ix, not (sumHidden s) ]
    ]
  where
    current = fromMaybe "" (routeModule (route m))
    moduleLink mn =
      H.a_ [ P.classList_ [ ("nav-module", True), ("active", mn == current) ], P.href_ (moduleHref mn) ]
        (moduleName mn)

-- | A result of a search: a module or a name.
data Result = Result
  { resName   :: MisoString
  , resKind   :: MisoString
  , resModule :: MisoString
  , resHref   :: MisoString
  }

-- | What matches a query: the modules, then the names, the best first.
results :: Index -> MisoString -> [Result]
results ix query0
  | S.null q = []
  | otherwise =
      take 6 [ Result mn "module" "" (moduleHref mn) | s <- ixModules ix, not (sumHidden s), let mn = sumName s, matches mn ]
      ++ take 40 [ Result n k mn (declHref mn a) | Def n k mn a <- sortOn rank (filter (\(Def n _ _ _) -> matches n) (ixDefs ix)) ]
  where
    q = S.toLower (S.strip query0)
    matches s = q `S.isInfixOf` S.toLower s
    rank (Def n _ _ _) = (S.toLower n /= q, not (q `S.isPrefixOf` S.toLower n), S.length n, n)

-- | The search palette: a box at the top of the page, with what matches
-- as you type.  Up and down select, enter goes, escape closes.
searchPalette :: Index -> Model -> View context props Model Action
searchPalette ix m =
  H.div_ [ P.class_ "palette-layer" ]
    [ H.div_ [ P.class_ "palette-backdrop", E.onClick ClosePalette ] []
    , H.div_ [ P.class_ "palette", textProp "role" "dialog" ]
        [ H.div_ [ P.class_ "palette-input" ]
            [ icon "search"
            , H.input_
                [ P.type_ "text", P.id_ "palette-input", P.placeholder_ "Search modules and definitions"
                , P.value_ (query m), E.onInput SetQuery, E.onKeyDown PaletteKey
                , textProp "autocomplete" "off", textProp "spellcheck" "false" ]
            , H.kbd_ [ E.onClick ClosePalette ] [ text "esc" ]
            ]
        , H.div_ [ P.class_ "palette-results" ] $
            case results ix (query m) of
              [] | S.null (S.strip (query m)) -> [ H.div_ [ P.class_ "palette-hint" ] [ text "Type a module or a name: map, Html, onClick" ] ]
                 | otherwise -> [ H.div_ [ P.class_ "palette-hint" ] [ text ("Nothing matches " <> query m) ] ]
              rs -> zipWith row [0 ..] rs
        , H.div_ [ P.class_ "palette-foot" ]
            [ H.span_ [] [ H.kbd_ [] [ text "↑" ], H.kbd_ [] [ text "↓" ], text " select" ]
            , H.span_ [] [ H.kbd_ [] [ text "↵" ], text " open" ]
            , H.span_ [] [ H.kbd_ [] [ text "esc" ], text " close" ]
            ]
        ]
    ]
  where
    row i r =
      H.a_ [ P.classList_ [ ("palette-row", True), ("selected", i == selected m) ]
           , P.href_ (resHref r), E.onClick ClosePalette, onMouseEnter' i ]
        [ H.span_ [ P.class_ ("kind kind-" <> resKind r) ] [ text (kindLetter (resKind r)) ]
        , H.span_ [ P.class_ "palette-name" ] (marked (resName r))
        , H.span_ [ P.class_ "palette-module" ] [ text (if S.null (resModule r) then "module" else resModule r) ]
        ]
    onMouseEnter' i = E.onMouseEnter (Select i)
    -- the name, with what the query matches stressed
    marked n =
      let q = S.toLower (S.strip (query m))
          (pre, rest) = S.breakOn q (S.toLower n)
          a = S.length pre
          b = S.length q
      in if S.null rest || S.null q then [ text n ]
         else [ text (S.take a n), H.mark_ [] [ text (S.take b (S.drop a n)) ], text (S.drop (a + b) n) ]

-- | A module name with its last component stressed.
moduleName :: MisoString -> [View context props model action]
moduleName mn =
  case S.breakOnEnd "." mn of
    ("", _) -> [ H.span_ [ P.class_ "mod-last" ] [ text mn ] ]
    (pre, lst) -> [ H.span_ [ P.class_ "mod-prefix" ] [ text pre ], H.span_ [ P.class_ "mod-last" ] [ text lst ] ]

kindLetter :: MisoString -> MisoString
kindLetter = \case
  "function" -> "f"
  "data" -> "D"
  "newtype" -> "N"
  "type" -> "T"
  "class" -> "C"
  "method" -> "m"
  "pattern" -> "P"
  "foreign" -> "F"
  "family" -> "T"
  "constructor" -> "c"
  "field" -> "·"
  "module" -> "M"
  _ -> "·"

page :: Index -> Model -> [View context props Model Action]
page ix m =
  case route m of
    Home -> homePage ix
    IndexPage -> indexPage ix
    ModulePage mn -> withModule mn modulePage
    SourcePage mn l e -> withModule mn (sourcePage l e)
  where
    withModule mn f
      | Just mo <- M.lookup mn (loaded m) = f mo
      | mn `elem` missing m || mn `notElem` map sumName (ixModules ix) = notFound ix mn
      | otherwise = loading mn

loading :: MisoString -> [View context props model action]
loading mn =
  [ H.div_ [ P.class_ "module-header" ]
      [ H.div_ [ P.class_ "eyebrow" ] [ text "Module" ]
      , H.h1_ [ P.class_ "module-title" ] (moduleName mn)
      ]
  , H.div_ [ P.class_ "skeleton" ] [ H.div_ [] [], H.div_ [] [], H.div_ [] [] ]
  ]

notFound :: Index -> MisoString -> [View context props model action]
notFound ix mn =
  [ H.div_ [ P.class_ "empty" ]
      [ H.h1_ [] [ text "Not found" ]
      , H.p_ [] [ text ("There is no module " <> mn <> " in " <> ixName ix <> ".") ]
      , H.a_ [ P.class_ "button", P.href_ "#/" ] [ text "All modules" ]
      ]
  ]

homePage :: Index -> [View context props Model Action]
homePage ix =
  [ H.section_ [ P.class_ "hero" ]
      [ H.div_ [ P.class_ "eyebrow" ] [ text "Package" ]
      , H.h1_ [] [ text (ixName ix), H.span_ [ P.class_ "hero-version" ] [ text (ixVersion ix) ] ]
      , if S.null (ixSynopsis ix) then text "" else H.p_ [ P.class_ "lead" ] [ text (ixSynopsis ix) ]
      , H.div_ [ P.class_ "stats" ]
          [ stat (length (ixModules ix)) "modules"
          , stat (length (ixDefs ix)) "definitions"
          , stat (sum (map sumLines (ixModules ix))) "lines"
          ]
      ]
  , H.div_ [ P.class_ "cards" ]
      [ H.a_ [ P.class_ "card", P.href_ (moduleHref (sumName s)) ]
          [ H.div_ [ P.class_ "card-title" ] (moduleName (sumName s))
          , H.p_ [ P.class_ "card-text" ] (if null (sumText s) then [ H.span_ [ P.class_ "muted" ] [ text "No description" ] ] else inlines (firstSentence (sumText s)))
          , H.div_ [ P.class_ "card-meta" ]
              [ text (ms (sumExports s) <> " exports · " <> ms (sumLines s) <> " lines") ]
          ]
      | s <- ixModules ix
      ]
  ]
  where
    stat n what = H.div_ [ P.class_ "stat" ] [ H.strong_ [] [ text (ms n) ], H.span_ [] [ text what ] ]

-- | Every name, A to Z.
indexPage :: Index -> [View context props Model Action]
indexPage ix =
  [ H.div_ [ P.class_ "module-header" ]
      [ H.div_ [ P.class_ "eyebrow" ] [ text "Index" ]
      , H.h1_ [ P.class_ "module-title" ] [ text "Every name" ]
      , H.div_ [ P.class_ "letters" ] [ H.a_ [ P.href_ ("#/:index/" <> l) ] [ text l ] | (l, _) <- groups ]
      ]
  ] ++
  [ H.section_ [ P.class_ "index-group", P.id_ ("/:index/" <> l) ]
      [ H.h2_ [ P.class_ "index-letter" ] [ text l ]
      , H.div_ [ P.class_ "index-names" ]
          [ H.a_ [ P.class_ "index-name", P.href_ (declHref mn a) ]
              [ H.span_ [ P.class_ ("kind kind-" <> k) ] [ text (kindLetter k) ]
              , H.code_ [] [ text n ]
              , H.span_ [ P.class_ "index-module" ] [ text mn ]
              ]
          | Def n k mn a <- ds ]
      ]
  | (l, ds) <- groups ]
  where
    groups = foldr add [] (sortOn (\(Def n _ _ _) -> (S.toLower n, n)) (ixDefs ix))
    add d@(Def n _ _ _) acc =
      let l = letter n
      in case acc of
           (l', ds) : rest | l' == l -> (l, d : ds) : rest
           _ -> (l, [d]) : acc
    letter n = case S.unpack (S.take 1 n) of
      [c] | c >= 'a' && c <= 'z' -> S.toUpper (S.singleton c)
          | c >= 'A' && c <= 'Z' -> S.singleton c
      _ -> "Symbols"

-- | Inlines up to the end of their first sentence.
firstSentence :: [Inline] -> [Inline]
firstSentence = \case
  Str s : rest ->
    case S.breakOn ". " s of
      (a, b) | not (S.null b) -> [ Str (a <> ".") ]
      _ -> Str s : firstSentence rest
  i : rest -> i : firstSentence rest
  [] -> []

modulePage :: Module -> [View context props Model Action]
modulePage mo =
  [ H.div_ [ P.class_ "module-header" ]
      [ H.div_ [ P.class_ "eyebrow" ] [ text "Module" ]
      , H.h1_ [ P.class_ "module-title" ] (moduleName (modName mo))
      , H.div_ [ P.class_ "module-actions" ]
          [ H.a_ [ P.class_ "button", P.href_ (sourceHref (modName mo) 0 0) ] [ icon "code", text "Source" ]
          , H.span_ [ P.class_ "muted" ] [ text (ms (length (modSource mo)) <> " lines") ]
          ]
      ]
  , if null (modHeader mo) then text ""
    else H.dl_ [ P.class_ "module-fields" ] (concat [ [ H.dt_ [] [ text k ], H.dd_ [] [ text v ] ] | (k, v) <- modHeader mo ])
  , H.div_ [ P.class_ "doc module-doc" ] (blocks (modName mo) (modDoc mo))
  ] ++ map (item mo) (modItems mo)

item :: Module -> Item -> View context props Model Action
item mo = \case
  Heading n t -> (if n <= 1 then H.h2_ else H.h3_) [ P.class_ "section-heading", P.id_ ("/" <> modName mo <> "/" <> slug t) ] [ text t ]
  Prose bs -> H.div_ [ P.class_ "doc prose" ] (blocks (modName mo) bs)
  ReExport mn inlined ->
    H.div_ [ P.class_ ("reexport" <> (if inlined then " inlined" else "")) ]
      [ icon "export"
      , text (if inlined then "Re-exported from " else "Re-exports module ")
      , modRef mn inlined
      ]
  Decl d -> declaration mo False d

declaration :: Module -> Bool -> Declaration -> View context props Model Action
declaration mo member d =
  H.article_ [ P.classList_ [ ("decl", True), ("member", member), ("deprecated", declWarning d == "deprecated") ]
             , P.id_ ("/" <> modName mo <> "/" <> declAnchor d) ]
    [ H.div_ [ P.class_ "decl-head" ]
        [ H.span_ [ P.class_ ("kind-badge kind-" <> declKind d) ] [ text (kindLabel (declKind d)) ]
        , H.a_ [ P.class_ "decl-name", P.href_ (declHref (modName mo) (declAnchor d)) ] [ text (declName d) ]
        , if declFrom d /= modName mo
            then H.span_ [ P.class_ "decl-from" ] [ text "from ", H.code_ [] [ text (declFrom d) ] ]
            else text ""
        , if S.null (declFixity d) then text "" else H.span_ [ P.class_ "fixity", P.title_ "Fixity" ] [ text (declFixity d) ]
        , H.span_ [ P.class_ "spacer" ] []
        , if declLine d > 0
            then H.a_ [ P.class_ "source-link", P.href_ (sourceHref (declFrom d) (declLine d) (declEnd d)), P.title_ "Show in the module's source" ]
                   [ icon "code", text "Source" ]
            else text ""
        ]
    , if S.null (declWarning d) then text ""
      else H.div_ [ P.class_ "deprecation" ]
             [ icon "alert", H.strong_ [] [ text (if declWarning d == "warning" then "Warning" else "Deprecated") ]
             , text (if S.null (declDeprecated d) then "" else ": " <> declDeprecated d) ]
    , H.pre_ [ P.class_ "signature" ] [ H.code_ [ prop "innerHTML" (declSig d) ] [] ]
    , if null (declArgs d) then text ""
      else H.div_ [ P.class_ "args" ]
             [ H.div_ [ P.class_ "arg" ]
                 [ H.code_ [ P.class_ "arg-type", prop "innerHTML" t ] []
                 , H.div_ [ P.class_ "doc arg-doc" ] (blocks (modName mo) ad)
                 ]
             | (t, ad) <- declArgs d ]
    , if null (declDoc d) then text "" else H.div_ [ P.class_ "doc" ] (blocks (modName mo) (declDoc d))
    , if null (declSubs d) then text ""
      else H.div_ [ P.class_ "subs" ]
             (H.div_ [ P.class_ "subs-title" ] [ text (if any ((== "constructor") . subKind) (declSubs d) then "Constructors" else "Fields") ]
              : map (sub mo) (declSubs d))
    , if S.null (declMinimal d) then text ""
      else H.div_ [ P.class_ "minimal" ] [ text "Minimal complete definition: ", H.code_ [] [ text (declMinimal d) ] ]
    , if null (declMembers d) then text ""
      else H.div_ [ P.class_ "members" ] (map (declaration mo True) (declMembers d))
    , if null (declInstances d) then text ""
      else H.details_ [ P.class_ "instances" ]
             [ H.summary_ [] [ icon "chevron", text ("Instances (" <> ms (length (declInstances d)) <> ")") ]
             , H.div_ [ P.class_ "instance-list" ] (map instanceRow (declInstances d))
             ]
    , if member || declLine d == 0 || declFrom d /= modName mo then text ""
      else H.details_ [ P.class_ "inline-source" ]
             [ H.summary_ [] [ icon "chevron", text "Show source" ]
             , H.pre_ [ P.class_ "code" ] [ H.code_ [ prop "innerHTML" (snippet mo (declLine d) (declEnd d)) ] [] ]
             ]
    ]
  where
    kindLabel "reexport" = "re-export"
    kindLabel k = k

sub :: Module -> Sub -> View context props Model Action
sub mo s =
  H.div_ [ P.class_ ("sub sub-" <> subKind s), P.id_ ("/" <> modName mo <> "/" <> subAnchor s) ]
    [ H.div_ [ P.class_ "sub-head" ]
        [ if subKind s == "field"
            then H.code_ [ P.class_ "sub-sig" ] [ H.span_ [ P.class_ "hl-name" ] [ text (subName s) ], text " :: ", H.span_ [ prop "innerHTML" (subSig s) ] [] ]
            else H.code_ [ P.class_ "sub-sig", prop "innerHTML" (subSig s) ] []
        ]
    , if null (subDoc s) then text "" else H.div_ [ P.class_ "doc sub-doc" ] (blocks (modName mo) (subDoc s))
    , if null (subFields s) then text "" else H.div_ [ P.class_ "fields" ] (map (sub mo) (subFields s))
    ]

instanceRow :: Instance -> View context props Model Action
instanceRow i =
  H.div_ [ P.class_ "instance" ]
    [ H.div_ [ P.class_ "instance-head" ]
        [ H.code_ [ prop "innerHTML" (instHead i) ] []
        , if instDerived i then H.span_ [ P.class_ "derived" ] [ text "derived" ] else text ""
        , H.span_ [ P.class_ "spacer" ] []
        , if instLine i > 0
            then H.a_ [ P.class_ "source-link", P.href_ (sourceHref (instModule i) (instLine i) (instEnd i)) ] [ icon "code", text "Source" ]
            else H.span_ [ P.class_ "derived", P.title_ "Known from the type checker" ] [ text "type checker" ]
        ]
    , if null (instDoc i) then text "" else H.div_ [ P.class_ "doc" ] (blocks (instModule i) (instDoc i))
    ]

-- | Lines of a module's source, as one highlighted snippet.
snippet :: Module -> Int -> Int -> MisoString
snippet mo l e = S.intercalate "\n" (take (e - l + 1) (drop (l - 1) (modSource mo)))

sourcePage :: Int -> Int -> Module -> [View context props Model Action]
sourcePage l e mo =
  [ H.div_ [ P.class_ "module-header" ]
      [ H.div_ [ P.class_ "eyebrow" ] [ text "Source" ]
      , H.h1_ [ P.class_ "module-title" ] (moduleName (modName mo))
      , H.div_ [ P.class_ "module-actions" ]
          [ H.a_ [ P.class_ "button", P.href_ (moduleHref (modName mo)) ] [ icon "book", text "Documentation" ]
          , H.span_ [ P.class_ "muted" ] [ text (ms (length (modSource mo)) <> " lines") ]
          ]
      ]
  , H.div_ [ P.class_ "source-view" ]
      [ if l > 0
          then H.div_ [ P.class_ "mark", P.id_ ("/" <> modName mo <> "/src/" <> range)
                      , CSS.styleInline_ ("top: calc(" <> ms (l - 1) <> " * var(--line) + 16px); height: calc(" <> ms (e - l + 1) <> " * var(--line))") ] []
          else text ""
      , H.pre_ [ P.class_ "code numbered" ]
          [ H.code_ [ prop "innerHTML" (S.concat [ "<span class=\"ln\">" <> x <> "</span>" | x <- modSource mo ]) ] [] ]
      ]
  ]
  where range = ms l <> (if e > l then "-" <> ms e else "")

-- | On this page: the sections and declarations of a module.
toc :: Model -> View context props Model Action
toc m =
  case route m of
    ModulePage mn | Just mo <- M.lookup mn (loaded m) ->
      H.aside_ [ P.class_ "toc" ]
        [ H.div_ [ P.class_ "sidebar-title" ] [ text "On this page" ]
        , H.nav_ [] (concatMap (entry mn) (modItems mo))
        ]
    _ -> H.aside_ [ P.class_ "toc" ] []
  where
    entry mn = \case
      Heading n t -> [ H.a_ [ P.classList_ [ ("toc-heading", True), ("sub", n > 1) ], P.href_ (declHref mn (slug t)) ] [ text t ] ]
      Decl d -> [ H.a_ [ P.class_ "toc-decl", P.href_ (declHref mn (declAnchor d)) ] [ text (declName d) ] ]
      ReExport r True -> [ H.span_ [ P.class_ "toc-heading sub" ] [ text r ] ]
      _ -> []

slug :: MisoString -> MisoString
slug = S.intercalate "-" . S.words . S.toLower

-----------------------------------------------------------------------------
-- Documentation

blocks :: MisoString -> [Block] -> [View context props model action]
blocks mn = map (block mn)

block :: MisoString -> Block -> View context props model action
block mn = \case
  Para is -> H.p_ [] (inlinesIn mn is)
  CodeBlock h -> H.pre_ [ P.class_ "code" ] [ H.code_ [ prop "innerHTML" h ] [] ]
  Pre is -> H.pre_ [ P.class_ "code pre-rich" ] [ H.code_ [] (inlinesIn mn is) ]
  Example e rs ->
    H.div_ [ P.class_ "example" ]
      [ H.div_ [ P.class_ "example-input" ] [ H.span_ [ P.class_ "prompt" ] [ text ">>>" ], H.code_ [] [ text e ] ]
      , if null rs then text "" else H.pre_ [ P.class_ "example-output" ] [ text (S.intercalate "\n" rs) ]
      ]
  QuickCheck e ->
    H.div_ [ P.class_ "example" ]
      [ H.div_ [ P.class_ "example-input" ] [ H.span_ [ P.class_ "prompt" ] [ text "prop>" ], H.code_ [] [ text e ] ] ]
  Bullets xs -> H.ul_ [] [ H.li_ [] (blocks mn x) | x <- xs ]
  Numbered xs -> H.ol_ [] [ H.li_ [] (blocks mn x) | x <- xs ]
  Definitions xs -> H.dl_ [] (concat [ [ H.dt_ [] (inlinesIn mn t), H.dd_ [] (blocks mn d) ] | (t, d) <- xs ])
  Table rows h ->
    H.div_ [ P.class_ "table-wrap" ]
      [ H.table_ []
          [ H.thead_ [] [ H.tr_ [] [ H.th_ [] (inlinesIn mn c) | c <- r ] | r <- take h rows ]
          , H.tbody_ [] [ H.tr_ [] [ H.td_ [] (inlinesIn mn c) | c <- r ] | r <- drop h rows ]
          ]
      ]
  Header n is -> (if n <= 1 then H.h3_ else H.h4_) [ P.class_ "doc-heading" ] (inlinesIn mn is)
  MathBlock t -> H.div_ [ P.class_ "math display", textProp "data-tex" t, prop "innerHTML" (escapeHtml t) ] []
  Since v -> H.div_ [ P.class_ "since" ] [ text ("Since " <> v) ]

inlines :: [Inline] -> [View context props model action]
inlines = inlinesIn ""

inlinesIn :: MisoString -> [Inline] -> [View context props model action]
inlinesIn mn = map (inline mn)

inline :: MisoString -> Inline -> View context props model action
inline mn = \case
  Str s -> text s
  Code c -> H.code_ [] [ text c ]
  Mono is -> H.code_ [ P.class_ "mono" ] (inlinesIn mn is)
  Emph is -> H.em_ [] (inlinesIn mn is)
  Strong is -> H.strong_ [] (inlinesIn mn is)
  Ref n "" -> H.code_ [ P.class_ "ref unresolved" ] [ text n ]
  Ref n href -> H.a_ [ P.class_ "ref", P.href_ href ] [ H.code_ [] [ text n ] ]
  ModRef m known -> modRef m known
  Link label url -> H.a_ [ P.href_ url, P.class_ "external" ] [ text label ]
  Image url title -> H.img_ [ P.src_ url, P.alt_ title, P.title_ title, P.class_ "doc-image" ]
  Math t -> H.span_ [ P.class_ "math", textProp "data-tex" t, prop "innerHTML" (escapeHtml t) ] []
  Anchor a -> H.span_ [ P.class_ "anchor", P.id_ ("/" <> mn <> "/#" <> a) ] []

-- | Text as HTML.  (Math is drawn by KaTeX in its element, which miso
-- leaves alone when its content is innerHTML.)
escapeHtml :: MisoString -> MisoString
escapeHtml = S.concatMap esc
  where
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '&' = "&amp;"
    esc c = S.singleton c

modRef :: MisoString -> Bool -> View context props model action
modRef mn True = H.a_ [ P.class_ "modref", P.href_ (moduleHref mn) ] [ text mn ]
modRef mn False = H.span_ [ P.class_ "modref unresolved" ] [ text mn ]

-----------------------------------------------------------------------------
-- Icons (Lucide, ISC license), drawn with the CSS of index.html

icon :: MisoString -> View context props model action
icon n = H.span_ [ P.class_ ("icon icon-" <> n), textProp "aria-hidden" "true" ] []
