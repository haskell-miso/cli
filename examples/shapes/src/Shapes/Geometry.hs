-- | Measuring shapes: their areas, perimeters, and the distances between
-- points.  Everything here is exact for circles and rectangles, and for
-- polygons uses the /shoelace formula/.
module Shapes.Geometry
  ( -- * Distance
    distance
  , (<->)
    -- * Area and perimeter
  , area
  , perimeter
    -- $polygons
  , isConvex
    -- * Transformations
  , scale
  , centroid
  ) where

import Shapes.Types

-- | The distance between two points.
--
-- >>> distance origin (Point 3 4)
-- 5.0
distance :: Point -> Point -> Length
distance (Point x0 y0) (Point x1 y1) = sqrt ((x1 - x0) ^ (2 :: Int) + (y1 - y0) ^ (2 :: Int))

-- | 'distance', as an operator.
--
-- @
-- origin '<->' Point 3 4 == 5
-- @
(<->) :: Point -> Point -> Length
(<->) = distance
infixl 6 <->

-- | The area of a shape.  The area of a 'Group' is the sum of the areas
-- of its shapes, so overlapping shapes are counted twice.
--
-- >>> area (Rectangle origin 2 3)
-- 6.0
area :: Shape -> Double
area s =
  case s of
    Circle _ r -> pi * r * r
    Rectangle _ w h -> w * h
    Polygon ps -> abs (shoelace ps) / 2
    Group ss -> sum (map area ss)

-- | The length of the outline of a shape.
--
-- 1. a circle: @2 * pi * r@
-- 2. a rectangle: @2 * (w + h)@
-- 3. a polygon: the sum of its sides
perimeter :: Shape -> Length
perimeter s =
  case s of
    Circle _ r -> 2 * pi * r
    Rectangle _ w h -> 2 * (w + h)
    Polygon ps -> sum (zipWith distance ps (drop 1 ps ++ take 1 ps))
    Group ss -> sum (map perimeter ss)

-- $polygons
-- A polygon is /convex/ when every line between two of its points is
-- inside it.  Rendering does not need polygons to be convex, but some
-- algorithms do.

-- | Whether a polygon is convex: its turns all go the same way.
isConvex :: [Point] -> Bool
isConvex ps =
  let turns = zipWith3 turn ps (drop 1 (cycle ps)) (drop 2 (cycle ps))
  in all (>= 0) turns || all (<= 0) turns
  where
    turn (Point ax ay) (Point bx by) (Point cx cy) = (bx - ax) * (cy - by) - (by - ay) * (cx - bx)

shoelace :: [Point] -> Double
shoelace ps = sum (zipWith cross ps (drop 1 ps ++ take 1 ps))
  where cross (Point x0 y0) (Point x1 y1) = x0 * y1 - x1 * y0

-- | Scale a shape about a point.
--
-- @since 0.1.0
--
-- Scaling by a factor \(k\) multiplies areas by \(k^2\):
--
-- \[ A(\mathrm{scale}\ k\ p\ s) = k^2 \cdot A(s) \]
--
-- * a factor above 1 makes the shape bigger
--
--     * and a factor between 0 and 1 makes it smaller
--
-- * a negative factor also mirrors it
scale
  :: Double  -- ^ the factor
  -> Point   -- ^ the fixed point
  -> Shape   -- ^ the shape to scale
  -> Shape   -- ^ the scaled shape
scale k (Point cx cy) = go
  where
    at (Point x y) = Point (cx + k * (x - cx)) (cy + k * (y - cy))
    go s = case s of
      Circle c r -> Circle (at c) (abs k * r)
      Rectangle p w h -> Rectangle (at p) (k * w) (k * h)
      Polygon ps -> Polygon (map at ps)
      Group ss -> Group (map go ss)

-- | The centre of the bounding box of a shape.
centroid s =
  let Bounds (Point x0 y0) (Point x1 y1) = bounds s
  in Point ((x0 + x1) / 2) ((y0 + y1) / 2)

{-# DEPRECATED isConvex "use a real geometry library for this" #-}
