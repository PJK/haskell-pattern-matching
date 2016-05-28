module TypeClass where

f :: Num a => a -> Int
f x
    | x <= 0 = 1
    | x >= 1 = 2
    -- Exhaustive for integrals, but not for double, for example.
