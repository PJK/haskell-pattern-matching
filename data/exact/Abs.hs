module Abs where

abs :: Int -> Int
abs x
    | x < 0 = - x
    | x > 0 = x