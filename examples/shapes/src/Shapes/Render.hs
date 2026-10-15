{-# LANGUAGE GADTs #-}
-- | Drawing shapes as SVG.  The output is a complete @\<svg\>@ document, with
-- a view box around the shape, so it can be shown as it is.
--
-- How each shape is drawn:
--
-- +-----------+--------------+
-- | Shape     | SVG element  |
-- +===========+==============+
-- | Circle    | @circle@     |
-- +-----------+--------------+
-- | Rectangle | @rect@       |
-- +-----------+--------------+
-- | Polygon   | @polygon@    |
-- +-----------+--------------+
--
-- [@fill@]: the colour inside a shape
--
-- [@stroke@]: the colour of its outline
module Shapes.Render where

import Data.List (intercalate)
import Shapes.Types

-- * Styles

-- | How a shape is painted.
data Style = Style
  { fill        :: String  -- ^ a CSS colour, or @"none"@
  , stroke      :: String
  , strokeWidth :: Double
  }

-- | Black outlines, no fill.
defaultStyle :: Style
defaultStyle = Style { fill = "none", stroke = "black", strokeWidth = 1 }

-- * Rendering

-- | A shape as an SVG document, in 'defaultStyle'.
renderSvg :: Shape -> String
renderSvg = renderSvgWith defaultStyle

-- | A shape as an SVG document, in a style.
--
-- The view box is the 'bounds' of the shape, with a margin of the stroke
-- width, so the outline is not cut off.
renderSvgWith :: Style -> Shape -> String
renderSvgWith st s =
  let Bounds (Point x0 y0) (Point x1 y1) = bounds s
      m = strokeWidth st
  in "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\""
       ++ unwords (map show [x0 - m, y0 - m, x1 - x0 + 2 * m, y1 - y0 + 2 * m]) ++ "\">"
       ++ element st s ++ "</svg>"

-- | The SVG element of a shape, without the document around it.
element :: Style -> Shape -> String
element st s =
  case s of
    Circle (Point x y) r -> tag "circle" [("cx", show x), ("cy", show y), ("r", show r)]
    Rectangle (Point x y) w h -> tag "rect" [("x", show x), ("y", show y), ("width", show w), ("height", show h)]
    Polygon ps -> tag "polygon" [("points", unwords [ show x ++ "," ++ show y | Point x y <- ps ])]
    Group ss -> "<g>" ++ concatMap (element st) ss ++ "</g>"
  where
    tag t as = "<" ++ t ++ " " ++ attrs (as ++ paint) ++ "/>"
    paint = [("fill", fill st), ("stroke", stroke st), ("stroke-width", show (strokeWidth st))]
    attrs as = intercalate " " [ k ++ "=\"" ++ v ++ "\"" | (k, v) <- as ]

-- * Paths

-- | A step of an SVG path.
data Step where
  -- | Move to a point without drawing.
  MoveTo :: Point -> Step
  -- | Draw a line to a point.
  LineTo :: Point -> Step
  -- | Close the path.
  Close :: Step
