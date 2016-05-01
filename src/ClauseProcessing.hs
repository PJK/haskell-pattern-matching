module ClauseProcessing where

import DataDefs

type ValueAbstractionVector = [Pattern]
type ValueAbstractionSet = [ValueAbstractionVector]
data ClauseCoverage = ClauseCoverage { c :: ValueAbstractionSet, u :: ValueAbstractionSet, d :: ValueAbstractionSet } deriving Show

-- |Computes covered values for the pattern vector and a set of VAs
coveredValues :: [Pattern] -> ValueAbstractionVector -> ValueAbstractionSet
-- CNil
-- todo this probably wrong and should be maybe
coveredValues [] [] = [[]]
-- CVar
coveredValues (VariablePattern p:ps) (u:us) = map (ucon u) (coveredValues ps us)


-- |Refines the VA of viable inputs using the pattern vector
patVecProc :: [Pattern] -> ValueAbstractionSet -> ClauseCoverage
patVecProc ps s = ClauseCoverage c u d
    where
        c = concatMap (coveredValues ps) s
        u = []
        d = []

-- |Coverage vector concatenation
-- TODO: Add the term constraints merging
ucon :: Pattern -> ValueAbstractionVector -> ValueAbstractionVector
ucon x xs = x:xs

