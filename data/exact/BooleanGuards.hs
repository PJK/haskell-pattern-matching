module BooleanGuards where

bguard :: Bool -> Int
bguard x
    | x         = 0
    | not x     = 1
    | otherwise = 2
    -- Exhaustive


