module PatternWithParameter where

data MyTrinary = A | B | C

data MyMaybe = Sum MyTrinary | Nope

extract :: MyMaybe -> MyTrinary
extract (Sum C) = A
extract (Sum A) = B

