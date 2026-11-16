-- | A module named Index: its data is not the site's index, even where
-- the filesystem ignores case.
module Index (quote, noSig) where

-- | A quote, as a character: highlighted as one literal.
quote :: Char
quote = '\''

-- | No signature: Hoogle gets no line for it without a type.
noSig = 'x'
