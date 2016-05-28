module TypeClass where

unsound :: Num a => a -> Int
unsound x
    | x <= 0 = 1
    | x >= 1 = 2
    -- Exhaustive for integrals, but not for double, for example.
    -- Our solver says nothing while this should probably get some warning that it's not exhaustive.
