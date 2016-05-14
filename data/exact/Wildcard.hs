module Wildcard where

data MyTrinary = A | B | C

data MyBool
    = True
    | False

data MyMaybe = Sum MyTrinary | Nope

extract :: MyMaybe -> MyMaybe-> MyTrinary
extract (Sum A) _ = A
extract (Sum B) _ = B
extract _ (Sum B) = B
extract _ (Sum C) = B
extract Nope _ = B
extract _ Nope = B
-- S C, S C is not covered