module Lists where

length :: [Int] -> Int
length [_, _, _] = 3
length (x:xs) = 1 + (length xs)
length [] = 0
