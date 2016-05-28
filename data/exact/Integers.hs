module Integers where

odd :: Int -> Bool
odd 1
    | 1 + 1 > 4 = True -- redundant
    | otherwise = False
odd 2 = False
-- Infinite set of uncovered constructors
