module BoolEs where

-- Not exhaustive
f :: Bool -> Bool -> Int
f x y
    | x && y = 1
    | x || y = 2
