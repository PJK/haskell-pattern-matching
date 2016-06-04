module Variables where

f :: Bool -> Bool -> Integer
f True y = 1
f x y = 2

incompleteF :: Bool -> Bool -> Integer
incompleteF True  y = 1
incompleteF x False = 2

data MyTrinary = A | B | C

extract :: Maybe MyTrinary -> Bool -> MyTrinary
extract (Just x) False = x
extract (Just C) True = C

