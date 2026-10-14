{-# LANGUAGE ScopedTypeVariables #-}
-- | The types of the library: points, shapes, and the 'HasBounds' class of
-- things with a bounding box.
module Shapes.Types
  ( -- * Points
    Point (..)
  , origin
  , translate
    -- * Shapes
  , Shape (..)
  , Bounds (..)
  , HasBounds (..)
    -- * Units
  , Length
  , Angle (..)
  , degrees
  ) where

-- | A point in the plane, with coordinates in user units.
--
-- The y axis points down, as it does in SVG.
data Point = Point
  { px :: !Double  -- ^ the horizontal coordinate
  , py :: !Double  -- ^ the vertical coordinate
  } deriving (Eq, Show)

-- | The point @(0, 0)@.
origin :: Point
origin = Point 0 0

-- | Move a point by an offset.
--
-- >>> translate 1 2 origin
-- Point {px = 1.0, py = 2.0}
translate :: Double -> Double -> Point -> Point
translate dx dy (Point x y) = Point (x + dx) (y + dy)

-- | A length, in the same units as the coordinates of a 'Point'.
type Length = Double

-- | An angle, in radians.  Use 'degrees' to make one from degrees.
newtype Angle = Angle { radians :: Double }
  deriving (Eq, Ord, Show)

-- | An angle from degrees.
--
-- >>> radians (degrees 180)
-- 3.141592653589793
degrees :: Double -> Angle
degrees d = Angle (d * pi / 180)

-- | A shape.  Each constructor says where the shape is, so shapes can be
-- drawn without a separate position.
data Shape
  = Circle Point Length
    -- ^ a circle: its centre and radius
  | Rectangle Point Length Length
    -- ^ a rectangle: its top left corner, width and height
  | Polygon [Point]
    -- ^ a closed polygon through the points, in order
  | Group [Shape]
    -- ^ shapes drawn together
  deriving (Eq, Show)

-- | An axis-aligned box: the smallest one around a shape.
data Bounds = Bounds
  { minPoint :: Point
  , maxPoint :: Point
  } deriving (Eq, Show)

-- | Things with a bounding box.
--
-- Laws:
--
-- * every point of @x@ is inside @'bounds' x@
-- * @'width' x >= 0@ and @'height' x >= 0@
class HasBounds a where
  -- | The bounding box.
  bounds :: a -> Bounds

  -- | The width of the bounding box.
  width :: a -> Length
  width x = let Bounds (Point x0 _) (Point x1 _) = bounds x in x1 - x0

  -- | The height of the bounding box.
  height :: a -> Length
  height x = let Bounds (Point _ y0) (Point _ y1) = bounds x in y1 - y0
  {-# MINIMAL bounds #-}

instance HasBounds Shape where
  bounds s =
    case s of
      Circle (Point x y) r -> Bounds (Point (x - r) (y - r)) (Point (x + r) (y + r))
      Rectangle p w h -> Bounds p (translate w h p)
      Polygon ps -> around ps
      Group ss -> around (concat [ [minPoint b, maxPoint b] | b <- map bounds ss ])
    where
      around [] = Bounds origin origin
      around ps = Bounds (Point (minimum (map px ps)) (minimum (map py ps)))
                         (Point (maximum (map px ps)) (maximum (map py ps)))

-- | Bounds combine into the bounds around both.
instance Semigroup Bounds where
  Bounds (Point a b) (Point c d) <> Bounds (Point e f) (Point g h) =
    Bounds (Point (min a e) (min b f)) (Point (max c g) (max d h))
