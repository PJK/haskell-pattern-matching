module Integers where

odd :: Int -> Boolean
odd 1
    | 1 + 1 > 4 = True
    | otherwise = False
odd 2 = False
-- Infinite set of uncovered constructors
-- This parses to LiteralPattern