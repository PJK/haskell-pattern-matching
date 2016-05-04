module ClauseProcessing where

import DataDefs
import qualified Data.Map as Map

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


uncoveredValues :: [Pattern] -> TypeMap  -> ValueAbstractionVector -> ValueAbstractionSet
-- UNil
uncoveredValues [] _ []  = [] -- Important! This is different than coveredValues
-- UConCon
-- TODO expansion and recovery
uncoveredValues (k@(ConstructorPattern pname args):ps) tmap (kv@(ConstructorPattern vname _):us)
        | pname == vname = map (kcon k) (uncoveredValues ps tmap us)
        | otherwise      = [kv:us]
-- UConVar
uncoveredValues (p@(ConstructorPattern pname args):ps) tmap (VariablePattern _:us) =
        concatMap (\constructor ->  uncoveredValues (p:ps) tmap (constructor:us)) allConstructors
    where
        -- TODO type names from context
        allConstructors = case Map.lookup "MyBool" tmap of
                                Just p -> p
                                Nothing -> error $ "Lookup for type " ++ "TODO" ++ " failed"
-- UVar
uncoveredValues (VariablePattern _:ps) tmap (u:us) = map (ucon u) (uncoveredValues ps tmap us)


-- |Refines the VA of viable inputs using the pattern vector
patVecProc :: [Pattern] -> ValueAbstractionSet -> TypeMap -> ClauseCoverage
patVecProc ps s tmap = ClauseCoverage c u d
    where
        c = concatMap (coveredValues ps) s
        u = concatMap (uncoveredValues ps tmap) s
        d = []


prettyIteratedVecProc :: Integer -> [[Pattern]] -> ValueAbstractionSet -> TypeMap -> IO ()

prettyIteratedVecProc _ [] vas _ = do
    print "Final iteration. Uncovered set:"
    print vas

prettyIteratedVecProc i (ps:pss) s tmap = do
    putStrLn $ "Iteration: " ++ show i
    putStrLn $ "Value abstractions to consider as inputs: " ++ show s
    putStrLn $ "Pattern: " ++ show ps
    putStrLn $ "U: " ++ show (capU res)
    putStrLn $ "C: " ++ show (capC res)
    putStrLn $ "D: " ++ show (capD res)
    prettyIteratedVecProc (i + 1) pss (capU res) tmap
    where
        res = patVecProc ps s tmap

-- |Coverage vector concatenation
-- TODO: Add the term constraints merging
ucon :: Pattern -> ValueAbstractionVector -> ValueAbstractionVector
ucon x xs = x:xs

-- Syd: I basicall want kcon :: ConstructorPattern -> ValueAbstractionVector -> ValueAbstractionVector
-- TODO pattern expansion
kcon :: Pattern -> ValueAbstractionVector -> ValueAbstractionVector
kcon pat@(ConstructorPattern _ _) ws = pat:ws