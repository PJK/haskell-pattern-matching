module PartialPatternWithVariable where

data MyTrinary = A | B | C

data MyMaybe = Sum MyTrinary | Nope

partialExtract :: MyMaybe -> MyMaybe -> MyTrinary
partialExtract (Sum C) (Sum A) = A
partialExtract (Sum x)  y      = B
partialExtract  Nope   (Sum C) = B

