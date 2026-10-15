-- |
-- Module      : Shapes
-- Description : Two-dimensional shapes, their geometry, and SVG rendering.
-- Copyright   : (c) The shapes authors
-- License     : BSD-3-Clause
--
-- __shapes__ is a small library for working with two-dimensional shapes.
-- Build them with the constructors of "Shapes.Types", measure them with
-- "Shapes.Geometry", and draw them with "Shapes.Render".
--
-- This module re-exports all of them, so a single import is enough:
--
-- > import Shapes
-- >
-- > main :: IO ()
-- > main = putStrLn (renderSvg (Circle origin 10))
--
-- See <https://haskell-miso.org the miso website> for the framework these
-- docs are built with.
module Shapes
  ( -- * Shapes
    Shape (..)
  , Point (..)
  , origin
    -- * Measuring
  , area
  , perimeter
  , (<->)
    -- * Drawing
  , module Shapes.Render
  ) where

import Shapes.Geometry
import Shapes.Render
import Shapes.Types
