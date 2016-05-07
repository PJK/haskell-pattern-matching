module PatternWithParamter where

data MyTrinary = A | B | C

data MyBool
    = True
    | False

data MyMaybe = Sum MyTrinary | Nope

extract :: MyMaybe -> MyBool -> MyTrinary
extract (Sum x) False = x
extract (Sum C) True = x

