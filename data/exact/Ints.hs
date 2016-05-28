module Ints where

-- Missing > 0
f :: Int -> Int
f 0 = 1
f x | x < 0 = 2

-- Exhaustive
g :: Int -> Int
g 0 = 1
g x | x < 0 = 2
    | x > 0 = 3
