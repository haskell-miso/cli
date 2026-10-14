-- | The files of a new project (miso new NAME [--template T]).
module Miso.CLI.Templates (templates, templateNames) where

-- | The templates, with what they show.
templateNames :: [(String, String)]
templateNames =
  [ ("counter", "a counter: the smallest miso app (the default)")
  , ("router", "pages with URLs: a router, and links between the pages")
  , ("canvas", "drawing on a canvas, driven by the model")
  ]

templates :: String -> String -> Maybe [(FilePath, String)]
templates template name =
  case lookup template [("counter", counterHs), ("router", routerHs), ("canvas", canvasHs)] of
    Nothing -> Nothing
    Just mainHs -> Just (files mainHs)
  where
    files mainHs =
      [ ("miso.yml", misoYml name)
      , ("app/Main.hs", mainHs name)
      , ("static/index.html", indexHtml name)
      , (".gitignore", "public/\ndist/\n.mhscache\n")
      , ("README.md", readme name)
      ]

misoYml :: String -> String
misoYml name = unlines
  [ "name: " ++ name
  , "version: 0.1.0"
  , "main: Main"
  , "source-dirs: [app]"
  , "# browser: WebAssembly (needs emcc), browser_js: JavaScript, native: an executable"
  , "target: browser"
  , "# miso, containers, mtl, ... come with the miso tool; add other packages here"
  , "dependencies: []"
  ]

counterHs :: String -> String
counterHs name = unlines
  [ "{-# LANGUAGE LambdaCase        #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "module Main where"
  , ""
  , "import Miso"
  , "import qualified Miso.Html.Element as H"
  , "import qualified Miso.Html.Event as E"
  , "import Miso.Lens"
  , ""
  , "data Action = AddOne | SubtractOne"
  , "  deriving (Show, Eq)"
  , ""
  , "main :: IO ()"
  , "main = startApp defaultEvents app"
  , ""
  , "app :: App Int Action"
  , "app = component 0 update_ view_"
  , ""
  , "update_ :: Action -> Effect parent props Int Action"
  , "update_ = \\case"
  , "  AddOne -> this += 1"
  , "  SubtractOne -> this -= 1"
  , ""
  , "view_ :: Int -> View context props Int Action"
  , "view_ n = H.div_ []"
  , "  [ H.h1_ [] [ " ++ show ("🍜 " ++ name) ++ " ]"
  , "  , H.button_ [ E.onClick SubtractOne ] [ \"-\" ]"
  , "  , text (ms n)"
  , "  , H.button_ [ E.onClick AddOne ] [ \"+\" ]"
  , "  ]"
  ]

indexHtml :: String -> String
indexHtml name = unlines
  [ "<!DOCTYPE html>"
  , "<html>"
  , "  <head>"
  , "    <meta charset=\"utf-8\">"
  , "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "    <title>" ++ name ++ "</title>"
  , "  </head>"
  , "  <body>"
  , "    <script src=\"index.js\"></script>"
  , "  </body>"
  , "</html>"
  ]

routerHs :: String -> String
routerHs name = unlines
  [ "{-# LANGUAGE DeriveAnyClass     #-}"
  , "{-# LANGUAGE DeriveGeneric      #-}"
  , "{-# LANGUAGE DerivingStrategies #-}"
  , "{-# LANGUAGE LambdaCase        #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "module Main where"
  , ""
  , "import GHC.Generics (Generic)"
  , "import Miso"
  , "import qualified Miso.Html.Element as H"
  , "import qualified Miso.Html.Event as E"
  , "import Miso.Lens"
  , "import Miso.Router (Router)"
  , ""
  , "-- | The pages: /index, /home, /about (the constructor names are the paths)."
  , "data Route = Index | Home | About | NotFound"
  , "  deriving anyclass Router"
  , "  deriving stock (Show, Eq, Generic)"
  , ""
  , "data Action = SetRoute Route | GoTo Route | RouteError MisoString"
  , "  deriving (Show, Eq)"
  , ""
  , "main :: IO ()"
  , "main = startApp defaultEvents app"
  , ""
  , "app :: App Route Action"
  , "app = (component Index update_ view_)"
  , "  { subs = [ routerSub (either (RouteError . ms . show) SetRoute) ] }"
  , ""
  , "update_ :: Action -> Effect parent props Route Action"
  , "update_ = \\case"
  , "  SetRoute r -> this .= r"
  , "  GoTo r -> io_ (pushRoute r)"
  , "  RouteError e -> do"
  , "    io_ (consoleError e)"
  , "    this .= NotFound"
  , ""
  , "view_ :: Route -> View context props Route Action"
  , "view_ r = H.div_ []"
  , "  [ H.h1_ [] [ " ++ show ("🍜 " ++ name) ++ " ]"
  , "  , H.nav_ [] [ link Home \"home\", text \" | \", link About \"about\" ]"
  , "  , case r of"
  , "      About -> H.p_ [] [ \"About this app.\" ]"
  , "      NotFound -> H.p_ [] [ \"No such page.\" ]"
  , "      _ -> H.p_ [] [ \"Welcome home.\" ]"
  , "  ]"
  , "  where"
  , "    link to label = H.button_ [ E.onClick (GoTo to) ] [ text label ]"
  ]

canvasHs :: String -> String
canvasHs name = unlines
  [ "{-# LANGUAGE LambdaCase        #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "module Main where"
  , ""
  , "import Miso"
  , "import qualified Miso.Canvas as Canvas"
  , "import qualified Miso.CSS.Color as Color"
  , "import qualified Miso.Html.Element as H"
  , "import qualified Miso.Html.Event as E"
  , "import qualified Miso.Html.Property as P"
  , "import Miso.Lens"
  , ""
  , "-- | The model: the radius of the circle."
  , "data Action = Grow | Shrink"
  , "  deriving (Show, Eq)"
  , ""
  , "main :: IO ()"
  , "main = startApp defaultEvents app"
  , ""
  , "app :: App Double Action"
  , "app = component 40 update_ view_"
  , ""
  , "update_ :: Action -> Effect parent props Double Action"
  , "update_ = \\case"
  , "  Grow -> this %= \\r -> min 140 (r + 10)"
  , "  Shrink -> this %= \\r -> max 10 (r - 10)"
  , ""
  , "view_ :: Double -> View context props Double Action"
  , "view_ r = H.div_ []"
  , "  [ H.h1_ [] [ " ++ show ("🍜 " ++ name) ++ " ]"
  , "  , H.button_ [ E.onClick Shrink ] [ \"-\" ]"
  , "  , H.button_ [ E.onClick Grow ] [ \"+\" ]"
  , "  , H.div_ []"
  , "    [ Canvas.canvas [ P.width_ \"300\", P.height_ \"300\" ] (\\_ -> pure ()) (\\() -> draw r) ]"
  , "  ]"
  , ""
  , "-- | Draw the circle (the canvas is drawn again when the model changes)."
  , "draw :: Double -> Canvas.Canvas ()"
  , "draw r = do"
  , "  Canvas.clearRect (0, 0, 300, 300)"
  , "  Canvas.fillStyle (Canvas.color (Color.rgb 124 108 240))"
  , "  Canvas.beginPath ()"
  , "  Canvas.arc (150, 150, r, 0, 2 * pi)"
  , "  Canvas.fill ()"
  ]

readme :: String -> String
readme name = unlines
  [ "# " ++ name
  , ""
  , "A [miso](https://haskell-miso.org) app."
  , ""
  , "```"
  , "miso build      # public/index.js, next to static/"
  , "```"
  , ""
  , "Then serve `public/` with any web server."
  ]
