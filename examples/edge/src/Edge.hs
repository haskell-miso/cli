{-# LANGUAGE PatternSynonyms, TypeFamilies #-}
-- |
-- Module      : Edge
-- Copyright   : (c) The edge authors, 2026
-- License     : BSD-3-Clause
-- Maintainer  : edge@example.com
-- Stability   : experimental
-- Portability : portable
--
-- Unusual declarations, each the way Haddock shows it.
module Edge
  ( -- * Several names, one signature
    first, second
    -- * Operators and fixity
  , (|>), (<|)
    -- * Pattern synonyms, bundled
  , Point (.., Origin)
    -- * Classes
  , Container (..)
    -- * Families
  , Key
  , Store
    -- * From a hidden module
  , module Edge.Internal
  ) where

import Edge.Internal

-- | Two functions with one signature: both are documented.
first, second :: (a, a) -> a
first (a, _) = a
second (_, b) = b

-- | Forward application.
(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 1 |>

-- | Backward application.
(<|) :: (a -> b) -> a -> b
f <| x = f x
infixr 0 <|

-- | A point.
data Point = Point Int Int

-- | The point at the origin.
pattern Origin :: Point
pattern Origin = Point 0 0

-- | Things that hold elements.
class Container f where
  -- | The element type.
  type Elem f
  -- | No elements.
  empty :: f
  -- | Add an element.
  insert
    :: Elem f  -- ^ the element
    -- a comment in the middle of a signature
    -> f       -- ^ the container
    -> f
  -- | Every element.
  toList :: f -> [Elem f]
  toList _ = []
  {-# MINIMAL empty, insert #-}

-- | A list holds elements.
instance Container [a] where
  type Elem [a] = a
  empty = []
  insert = (:)

-- | The key of a store.
type family Key s

type instance Key [a] = Int

-- | A store, by its key.
data family Store k

-- | A store of strings.
data instance Store String = StringStore [String]

{-# WARNING second "the second of a pair is often the wrong one" #-}
