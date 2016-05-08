module PatternWithParameterVariableMatch where

data MyTrinary = A | B | C

data MyBool
    = True
    | False

data MyMaybe = Sum MyTrinary | Nope

extract :: MyMaybe -> MyMaybe -> MyTrinary
extract (Sum C) (Sum A) = A
extract (Sum x)  y      = B -- This should catch Sum a, Sum B, _
extract  Nope    x      = B