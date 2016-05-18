module Wildcard where

data MyTrinary = A | B | C

data MyMaybe = Sum MyTrinary | Nope

extract :: MyMaybe -> MyMaybe-> MyTrinary
extract (Sum A) _ = A
extract (Sum B) _ = B
extract _ (Sum B) = B
extract _ (Sum C) = B
{-
    Uncovered:
    Sum C, Sum A
    Sum C, Nope
    Nope, Sum A
    Nope, Nope
-}
