module BooleanGuards where

bguard :: Bool -> Int
bguard x
    | x     = 0
    | not x = 1
    | f x x = 3
    | x && x = 4
    -- Exhaustive


