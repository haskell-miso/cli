-- | The files of a new project (miso new NAME).
module Miso.CLI.Templates (templates) where

templates :: String -> [(FilePath, String)]
templates name =
  [ ("miso.yml", misoYml name)
  , ("app/Main.hs", mainHs name)
  , ("static/index.html", indexHtml name)
  , (".gitignore", "public/\ndist/\n.mhscache\n")
  , ("README.md", readme name)
  ]

misoYml :: String -> String
misoYml name = unlines
  [ "name: " ++ name
  , "main: Main"
  , "source-dirs: [app]"
  , "# browser: WebAssembly (needs emcc), browser_js: JavaScript, native: an executable"
  , "target: browser"
  , "# miso, containers, mtl, ... come with the miso tool; add other packages here"
  , "dependencies: []"
  ]

mainHs :: String -> String
mainHs name = unlines
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
