module ClauseProcessing where

import DataDefs

type ValueAbstractionVector = [Pattern]
type ValueAbstractionSet = [ValueAbstractionVector]
data ClauseCoverage = ClauseCoverage { capC :: ValueAbstractionSet, capU :: ValueAbstractionSet, capD :: ValueAbstractionSet } deriving Show

-- |Computes covered values for the pattern vector and a set of VAs
coveredValues :: [Pattern] -> ValueAbstractionVector -> ValueAbstractionSet
-- CNil
-- todo this probably wrong and should be maybe
coveredValues [] [] = [[]]
-- CConCon
-- TODO implement the inner pattern expansion and recovery
coveredValues (ConstructorPattern pname args:ps) (ConstructorPattern vname _:us)
        | pname == vname = map (kcon (ConstructorPattern pname args)) (coveredValues ps us)
        | otherwise      = []
-- CConVar
coveredValues (k@(ConstructorPattern _ _):ps) (VariablePattern _:us) = coveredValues (k:ps) (k:us)
-- CVar
coveredValues (VariablePattern _:ps) (u:us) = map (ucon u) (coveredValues ps us)


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

-- Syd: I basicall want kcon :: ConstructorPattern -> ValueAbstractionVector -> ValueAbstractionVector
-- TODO pattern expansion
kcon :: Pattern -> ValueAbstractionVector -> ValueAbstractionVector
kcon pat@(ConstructorPattern _ _) ws = pat:ws