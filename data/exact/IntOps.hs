module IntOps where

-- Not exhaustive and the first clause is redundant because it's always false
f :: Int -> Int
f x | 2 `div` 1 == 1 = 1

-- Second is redundant
g :: Int -> Int
g x | 2 `rem` 3 == 2 = 1
    | otherwise = 2

-- Same but with infix
h :: Int -> Int
h x | 2 `rem` 3 == 2 = 1
    | otherwise = 2
