module ClauseProcessing where

import Lib

data ValueAbstractionVector = VAV [Pattern] deriving Show
data ValueAbstractionSet = VAS [[Pattern]] deriving Show
data ClauseCoverage = ClauseCoverage { c :: ValueAbstractionSet, u :: ValueAbstractionSet, d :: ValueAbstractionSet } deriving Show

-- |Computes covered values for the pattern vector and a set of va
coveredValues :: [Pattern] -> ValueAbstractionSet -> ValueAbstractionVector
coveredValues [] [] = []