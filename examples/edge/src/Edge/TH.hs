-- | Template Haskell and other syntax that is easy to misread.
module Edge.TH where

-- | A record.
data Config = Config { verbose :: Bool, level :: Int }

makeLenses ''Config

$(return [])

-- | An operator section, defined point-free.
plusOne :: Int -> Int
plusOne = (+ 1)

-- | A where clause with an operator definition.
combine :: Int -> Int -> Int
combine a b = a <+> b
  where
    x <+> y = x * 10 + y
