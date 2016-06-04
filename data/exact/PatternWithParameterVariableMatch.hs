module PatternWithParameterVariableMatch where

data MyTrinary = A | B | C

extract :: Maybe MyTritary -> Maybe MyTritary -> MyTrinary
extract (Just C) (Just A) = A
extract (Just x)  y       = B -- This should catch Just A, Just B, _
extract  Nothing  x       = B


partialExtract :: Maybe MyTritary -> Maybe MyTritary -> MyTrinary
partialExtract (Just C) (Just A) = A
partialExtract (Just x)  y       = B -- This should catch Just A, Just B, _
partialExtract  Nothing (Just C) = B


