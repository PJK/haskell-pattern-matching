module ListsIncomplete where

length :: [a] -> Int
length [_] = 1
length (x:xs) = 1 + (length xs)

