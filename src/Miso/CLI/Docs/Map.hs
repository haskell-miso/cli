-- | A finite map, the part of Data.Map miso docs uses: the tool is built
-- with mhs alone, without the containers package.  A weight-balanced tree,
-- as Data.Map is.
module Miso.CLI.Docs.Map
  ( Map
  , empty
  , insert
  , insertWith
  , fromList
  , fromListWith
  , lookup
  , findWithDefault
  , member
  , elems
  , (!)
  ) where

import Prelude hiding (lookup)
import Data.List (foldl')

data Map k v = Tip | Bin !Int k v (Map k v) (Map k v)

size :: Map k v -> Int
size Tip = 0
size (Bin n _ _ _ _) = n

empty :: Map k v
empty = Tip

lookup :: Ord k => k -> Map k v -> Maybe v
lookup _ Tip = Nothing
lookup k (Bin _ k' v l r) = case compare k k' of
  LT -> lookup k l
  GT -> lookup k r
  EQ -> Just v

findWithDefault :: Ord k => v -> k -> Map k v -> v
findWithDefault d k m = maybe d id (lookup k m)

member :: Ord k => k -> Map k v -> Bool
member k m = maybe False (const True) (lookup k m)

(!) :: Ord k => Map k v -> k -> v
m ! k = maybe (error "Miso.CLI.Docs.Map.!: no such key") id (lookup k m)

insert :: Ord k => k -> v -> Map k v -> Map k v
insert = insertWith const

-- | Insert with a function of the new value and the old one.
insertWith :: Ord k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v = go
  where
    go Tip = Bin 1 k v Tip Tip
    go (Bin n k' v' l r) = case compare k k' of
      LT -> balance k' v' (go l) r
      GT -> balance k' v' l (go r)
      EQ -> Bin n k (f v v') l r

fromList :: Ord k => [(k, v)] -> Map k v
fromList = foldl' (\m (k, v) -> insert k v m) empty

fromListWith :: Ord k => (v -> v -> v) -> [(k, v)] -> Map k v
fromListWith f = foldl' (\m (k, v) -> insertWith f k v m) empty

-- | The values, in the order of their keys.
elems :: Map k v -> [v]
elems m = go m []
  where
    go Tip acc = acc
    go (Bin _ _ v l r) acc = go l (v : go r acc)

-- Balancing, as in "Implementing Sets Efficiently in a Functional Language"
-- (Adams), with the parameters of Data.Map.

delta, ratio :: Int
delta = 3
ratio = 2

bin :: k -> v -> Map k v -> Map k v -> Map k v
bin k v l r = Bin (size l + size r + 1) k v l r

balance :: k -> v -> Map k v -> Map k v -> Map k v
balance k v l r
  | sl + sr <= 1 = bin k v l r
  | sr > delta * sl = rotateL k v l r
  | sl > delta * sr = rotateR k v l r
  | otherwise = bin k v l r
  where
    sl = size l
    sr = size r

rotateL :: k -> v -> Map k v -> Map k v -> Map k v
rotateL k v l r@(Bin _ _ _ rl rr)
  | size rl < ratio * size rr = singleL k v l r
  | otherwise = doubleL k v l r
rotateL k v l Tip = bin k v l Tip

rotateR :: k -> v -> Map k v -> Map k v -> Map k v
rotateR k v l@(Bin _ _ _ ll lr) r
  | size lr < ratio * size ll = singleR k v l r
  | otherwise = doubleR k v l r
rotateR k v Tip r = bin k v Tip r

singleL, singleR, doubleL, doubleR :: k -> v -> Map k v -> Map k v -> Map k v
singleL k v l (Bin _ k2 v2 rl rr) = bin k2 v2 (bin k v l rl) rr
singleL k v l Tip = bin k v l Tip
singleR k v (Bin _ k2 v2 ll lr) r = bin k2 v2 ll (bin k v lr r)
singleR k v Tip r = bin k v Tip r
doubleL k v l (Bin _ k2 v2 (Bin _ k3 v3 rll rlr) rr) = bin k3 v3 (bin k v l rll) (bin k2 v2 rlr rr)
doubleL k v l r = singleL k v l r
doubleR k v (Bin _ k2 v2 ll (Bin _ k3 v3 lrl lrr)) r = bin k3 v3 (bin k2 v2 ll lrl) (bin k v lrr r)
doubleR k v l r = singleR k v l r
