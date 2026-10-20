-- | The files of a new project (miso new NAME [--template T], miso init).
module Miso.CLI.Templates (templates, templateNames, licenseText) where

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
      , (".gitignore", "public/\ndist/\n.miso/\n.mhscache\n.miso-build.log\n")
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
  , "update_ :: Action -> Effect context props Int Action"
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
  [ "{-# LANGUAGE LambdaCase        #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "module Main where"
  , ""
  , "import Miso"
  , "import qualified Miso.Html.Element as H"
  , "import qualified Miso.Html.Event as E"
  , "import Miso.Lens"
  , "import Miso.Router (Router (..), Token (IndexToken), path, routes, toPath)"
  , "import Miso.Util.Parser (satisfy)"
  , ""
  , "-- | The pages: / (Index), /home, /about."
  , "data Route = Index | Home | About | NotFound"
  , "  deriving (Show, Eq)"
  , ""
  , "-- | How a route is written in a URL (fromRoute), and read from one"
  , "-- (routeParser): / is IndexToken, /home is the path \"home\"."
  , "instance Router Route where"
  , "  fromRoute = \\case"
  , "    Index -> [ IndexToken ]"
  , "    Home -> [ toPath \"home\" ]"
  , "    About -> [ toPath \"about\" ]"
  , "    NotFound -> [ toPath \"notfound\" ]"
  , "  routeParser = routes"
  , "    [ Index <$ satisfy (== IndexToken)"
  , "    , Home <$ path \"home\""
  , "    , About <$ path \"about\""
  , "    , NotFound <$ path \"notfound\""
  , "    ]"
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
  , "update_ :: Action -> Effect context props Route Action"
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
  , "  , H.nav_ [] [ link Index \"index\", text \" | \", link Home \"home\", text \" | \", link About \"about\" ]"
  , "  , case r of"
  , "      Index -> H.p_ [] [ \"The index.\" ]"
  , "      Home -> H.p_ [] [ \"Welcome home.\" ]"
  , "      About -> H.p_ [] [ \"About this app.\" ]"
  , "      NotFound -> H.p_ [] [ \"No such page.\" ]"
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
  , "update_ :: Action -> Effect context props Double Action"
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

-- | The text of a LICENSE file for a license id (SPDX), the year and the
-- author ("Name <email>": the name is the copyright holder); Nothing for a
-- license it does not have the text of.
licenseText :: String -> String -> String -> Maybe String
licenseText license year author =
  case license of
    "MIT" -> Just mit
    "BSD-2-Clause" -> Just (bsd False)
    "BSD-3-Clause" -> Just (bsd True)
    _ -> Nothing
  where
    holder = case takeWhile (/= '<') author of
               "" -> "the authors"
               n -> reverse (dropWhile (== ' ') (reverse n))
    copyright = "Copyright (c) " ++ year ++ " " ++ holder
    mit = unlines
      [ "MIT License"
      , ""
      , copyright
      , ""
      , "Permission is hereby granted, free of charge, to any person obtaining a copy"
      , "of this software and associated documentation files (the \"Software\"), to deal"
      , "in the Software without restriction, including without limitation the rights"
      , "to use, copy, modify, merge, publish, distribute, sublicense, and/or sell"
      , "copies of the Software, and to permit persons to whom the Software is"
      , "furnished to do so, subject to the following conditions:"
      , ""
      , "The above copyright notice and this permission notice shall be included in all"
      , "copies or substantial portions of the Software."
      , ""
      , "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR"
      , "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,"
      , "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE"
      , "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER"
      , "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,"
      , "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE"
      , "SOFTWARE."
      ]
    bsd three = unlines $
      [ copyright
      , ""
      , "Redistribution and use in source and binary forms, with or without"
      , "modification, are permitted provided that the following conditions are met:"
      , ""
      , "1. Redistributions of source code must retain the above copyright notice, this"
      , "   list of conditions and the following disclaimer."
      , ""
      , "2. Redistributions in binary form must reproduce the above copyright notice,"
      , "   this list of conditions and the following disclaimer in the documentation"
      , "   and/or other materials provided with the distribution."
      ] ++ (if three then
      [ ""
      , "3. Neither the name of the copyright holder nor the names of its"
      , "   contributors may be used to endorse or promote products derived from"
      , "   this software without specific prior written permission."
      ] else []) ++
      [ ""
      , "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\""
      , "AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE"
      , "IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE"
      , "DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE"
      , "FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL"
      , "DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR"
      , "SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER"
      , "CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,"
      , "OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE"
      , "OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
      ]
