module Lists where

-- Not exhaustive, 2 patterns missing
f :: [a] -> Int
f (x:y:xs) = 2 + (length xs)

-- Not exhaustive, lengths greater than 2 missing, one pattern.
g :: [a] -> Int
g [] = 0
g [x] = 1
g [x, y] = 2

