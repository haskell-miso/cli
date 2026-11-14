{-# OPTIONS_HADDOCK hide #-}
-- | Not listed: its names are documented where "Edge" re-exports them.
module Edge.Internal (helper, other) where

-- | A helper, re-exported by "Edge".
helper :: Int -> Int
helper = (+ 1)

{- | A block doc, with a space before its mark; &#1114112; is no character,
and [this](javascript:alert%281%29) is no link. -}
other :: Int
other = 0
