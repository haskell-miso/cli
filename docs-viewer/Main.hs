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
-- what a hash names once it is drawn, draws math (with KaTeX), and searches
-- the index for the palette.
--
-- The search palette is a component of its own, so typing in it draws the
-- palette, not the page.  Documentation comes as HTML (the tool draws it),
-- so a page is a few nodes for each declaration.
module Main (main) where

import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Miso hiding (Sub)
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
  , theme    :: Theme
  , menuOpen :: Bool
  , loaded   :: M.Map MisoString Module
  , missing  :: [MisoString]
  } deriving (Eq)

data Action
  = SetHash MisoString
  | NextTheme
  | ToggleMenu
  | Loaded MisoString (Maybe MisoString)
  | OpenPalette

main :: IO ()
main = do
  h <- getHash
  raw <- getData "index"
  case maybe (Left "no data/index.js") eitherDecode raw of
    Left e -> startApp defaultEvents (component () (\_ -> pure ()) (\_ -> broken e))
    Right ix -> startApp events (app ix (Model (routeOf h) System False M.empty []))

-- | The events the page listens for: the palette needs keydown (its
-- arrows) and mouseenter, which are not among the default ones.
events :: Events
events = defaultEvents <> keyboardEvents <> mouseEvents

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
  OpenPalette -> io_ $ do
    w <- jsg "window"
    _ <- w # "misoDocsOpenPalette" $ ()
    pure ()
  NextTheme -> modify (\m -> m { theme = next (theme m) })
  ToggleMenu -> modify (\m -> m { menuOpen = not (menuOpen m) })
  where
    known mn = mn `elem` map sumName (ixModules ix)
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

view_ :: Index -> Model -> View () props Model Action
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
    , "palette" +> paletteComponent
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

-----------------------------------------------------------------------------
-- The search palette: a component, with its own model.  The search is the
-- page's (misoDocsSearch, over the index); it shows what that finds.  It
-- opens on misodocs:palette (the search button, "/", Ctrl-K, Cmd-K); up and
-- down select, enter goes, escape closes.

-- | A module or a name that matches a query.
data Result = Result
  { resName   :: MisoString
  , resKind   :: MisoString
  , resModule :: MisoString
  , resHref   :: MisoString
  } deriving (Eq)

data Palette = Palette
  { open     :: Bool
  , query    :: MisoString
  , selected :: Int
  , results  :: [Result]
  } deriving (Eq)

data PaletteAction
  = Open
  | Close
  | SetQuery MisoString
  | Found MisoString [Result]
  | Key KeyCode
  | Select Int

paletteComponent :: Component () () Palette PaletteAction
paletteComponent = (component (Palette False "" 0 []) updatePalette viewPalette)
  { subs = [ windowEvent "misodocs:palette" Open ] }

-- | An event of the window, as an action.
windowEvent :: MisoString -> action -> (action -> IO ()) -> IO model -> IO ()
windowEvent name action sink _ = do
  w <- jsg "window"
  _ <- addEventListener w name (\_ -> sink action)
  pure ()

updatePalette :: PaletteAction -> Effect parent props Palette PaletteAction
updatePalette = \case
  -- a new input each time, so a new query
  Open -> modify (\m -> m { open = True, query = "", results = [], selected = 0 })
  Close -> modify (\m -> m { open = False })
  Select i -> modify (\m -> m { selected = i })
  SetQuery q -> do
    modify (\m -> m { query = q })
    io (Found q <$> search q)
  Found q rs -> modify (\m -> if query m == q then m { results = rs, selected = 0 } else m)
  Key (KeyCode k) -> do
    m <- get
    let n = length (results m)
    case k of
      27 -> modify (\m' -> m' { open = False })
      40 -> modify (\m' -> m' { selected = if n == 0 then 0 else (selected m' + 1) `mod` n })
      38 -> modify (\m' -> m' { selected = if n == 0 then 0 else (selected m' - 1) `mod` n })
      13 -> case drop (selected m) (results m) of
        r : _ -> do
          modify (\m' -> m' { open = False })
          io_ (go (resHref r))
        [] -> pure ()
      _ -> pure ()
  where
    go href = do
      w <- jsg "window"
      _ <- w # "misoDocsGo" $ href
      pure ()

-- | What the page's search finds for a query: a line for each result, its
-- fields separated by tabs.
search :: MisoString -> IO [Result]
search q = do
  w <- jsg "window"
  r <- w # "misoDocsSearch" $ q
  s <- fromMaybe "" <$> castJSVal r
  pure [ Result a b c d | l <- S.lines s, [a, b, c, d] <- [S.splitOn "\t" l] ]

viewPalette :: Palette -> View context props Palette PaletteAction
viewPalette m
  | not (open m) = H.div_ [] []
  | otherwise =
      H.div_ [ P.class_ "palette-layer" ]
        [ H.div_ [ P.class_ "palette-backdrop", E.onClick Close ] []
        , H.div_ [ P.class_ "palette", textProp "role" "dialog" ]
            [ H.div_ [ P.class_ "palette-input" ]
                [ icon "search"
                , H.input_
                    [ P.type_ "text", P.id_ "palette-input", P.placeholder_ "Search modules and definitions"
                    -- the input has its value (no value_: drawing it while a
                    -- key is typed would undo the key)
                    , E.onInput SetQuery, E.onKeyDown Key
                    , textProp "autocomplete" "off", textProp "spellcheck" "false" ]
                , H.kbd_ [ E.onClick Close ] [ text "esc" ]
                ]
            , H.div_ [ P.class_ "palette-results" ] $
                case results m of
                  [] | S.null (S.strip (query m)) -> [ hint "Type a module or a name" ]
                     | otherwise -> [ hint ("Nothing matches " <> query m) ]
                  rs -> zipWith row [0 ..] rs
            , H.div_ [ P.class_ "palette-foot" ]
                [ H.span_ [] [ H.kbd_ [] [ text "↑" ], H.kbd_ [] [ text "↓" ], text " select" ]
                , H.span_ [] [ H.kbd_ [] [ text "↵" ], text " open" ]
                , H.span_ [] [ H.kbd_ [] [ text "esc" ], text " close" ]
                ]
            ]
        ]
  where
    hint t = H.div_ [ P.class_ "palette-hint" ] [ text t ]
    row i r =
      H.a_ [ P.classList_ [ ("palette-row", True), ("selected", i == selected m) ]
           , P.href_ (resHref r), E.onClick Close, E.onMouseEnter (Select i) ]
        [ H.span_ [ P.class_ ("kind kind-" <> resKind r) ] [ text (kindLetter (resKind r)) ]
        , H.span_ [ P.class_ "palette-name" ] (marked (resName r))
        , H.span_ [ P.class_ "palette-module" ] [ text (if S.null (resModule r) then "module" else resModule r) ]
        ]
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
          , if S.null (sumText s) then H.p_ [ P.class_ "card-text muted" ] [ text "No description" ]
            else H.p_ [ P.class_ "card-text", prop "innerHTML" (sumText s) ] []
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
  , doc "doc module-doc" (modDoc mo)
  ] ++ map (item mo) (modItems mo)

item :: Module -> Item -> View context props Model Action
item mo = \case
  Heading n t -> (if n <= 1 then H.h2_ else H.h3_) [ P.class_ "section-heading", P.id_ ("/" <> modName mo <> "/" <> slug t) ] [ text t ]
  Prose h -> doc "doc prose" h
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
                 , doc "doc arg-doc" ad
                 ]
             | (t, ad) <- declArgs d ]
    , doc "doc" (declDoc d)
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
    , doc "doc sub-doc" (subDoc s)
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
    , doc "doc" (instDoc i)
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

-- | Documentation (HTML, from the tool), or nothing.  (Math in it is drawn
-- by KaTeX, which miso leaves alone in an element whose content is innerHTML.)
doc :: MisoString -> MisoString -> View context props model action
doc c h
  | S.null h = text ""
  | otherwise = H.div_ [ P.class_ c, prop "innerHTML" h ] []

modRef :: MisoString -> Bool -> View context props model action
modRef mn True = H.a_ [ P.class_ "modref", P.href_ (moduleHref mn) ] [ text mn ]
modRef mn False = H.span_ [ P.class_ "modref unresolved" ] [ text mn ]

-----------------------------------------------------------------------------
-- Icons (Lucide, ISC license), drawn with the CSS of index.html

icon :: MisoString -> View context props model action
icon n = H.span_ [ P.class_ ("icon icon-" <> n), textProp "aria-hidden" "true" ] []
