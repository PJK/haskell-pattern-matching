module BoolEs where

-- Not exhaustive
f :: Bool -> Bool -> Int
f x y
    | x && y = 1
    | x || y = 2

-- Exhaustive, but == is only supported for ints
-- g :: Bool -> Bool -> Int
-- g x y
--     | x == y = 3
--     | x /= y = 4
--     | otherwise = 5

h :: Bool -> Int
h x | otherwise = 1
