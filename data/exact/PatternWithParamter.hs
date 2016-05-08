module PatternWithParamter where

data MyTrinary = A | B | C

data MyBool
    = True
    | False

data MyMaybe = Sum MyTrinary | Nope

extract :: MyMaybe -> MyTrinary
extract (Sum C) = A
extract (Sum A) = B

