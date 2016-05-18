module Variables where

-- f :: MyBool -> MyBool -> Integer
-- f True y = 4
-- f x y = 3
--
-- incompleteF :: MyBool -> MyBool -> Integer
-- incompleteF True y = 4
-- incompleteF x False = 3

data MyTrinary = A | B | C

-- incompleteFT :: MyTrinary -> MyTrinary -> Integer
-- incompleteFT A y = 4
-- incompleteFT x B = 3

data MyMaybe = Sum MyTrinary | Nope

extract :: MyMaybe -> Bool -> MyTrinary
extract (Sum x) False = x
extract (Sum C) True = C
