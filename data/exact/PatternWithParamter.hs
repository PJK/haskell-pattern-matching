module PatternWithParamter where

data MyTrinary = A | B | C

-- incompleteFT :: MyTrinary -> MyTrinary -> Integer
-- incompleteFT A y = 4
-- incompleteFT x B = 3

data MyMaybe = Sum MyTrinary | Nope

extract :: MyMaybe -> MyBool -> MyTrinary
extract (Sum x) False = x
extract (Sum C) True = x

