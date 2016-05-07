module ClauseProcessing where

import qualified Data.Map   as Map
import           DataDefs

import           Data.Maybe (fromMaybe)

type ValueAbstractionVector = [Pattern]
type ValueAbstractionSet = [ValueAbstractionVector]
data ClauseCoverage = ClauseCoverage { capC :: ValueAbstractionSet, capU :: ValueAbstractionSet, capD :: ValueAbstractionSet } deriving Show


--
-- Implements the 'C' helper function
--
-- |Computes covered values for the pattern vector and a set of VAs
coveredValues :: PatternVector -> ValueAbstractionVector -> ValueAbstractionSet
-- CNil
-- todo this probably wrong and should be maybe
coveredValues [] [] = [[]]
-- CConCon
-- TODO implement the inner pattern expansion and recovery
coveredValues ((ConstructorPattern pname args, _):ps) (ConstructorPattern vname _:us)
        | pname == vname = map (kcon (ConstructorPattern pname args)) (coveredValues ps us)
        | otherwise      = []
-- CConVar
coveredValues (kk@(k@(ConstructorPattern _ _), _):ps) (VariablePattern _:us) = coveredValues (kk:ps) (k:us)
-- CVar
coveredValues ((VariablePattern _, _):ps) (u:us) = map (ucon u) (coveredValues ps us)
coveredValues _ _ = error "unsupported pattern"


--
-- Implements the 'U' helper function
--
uncoveredValues :: PatternVector -> TypeMap  -> ValueAbstractionVector -> ValueAbstractionSet
-- UNil
uncoveredValues [] _ []  = [] -- Important! This is different than coveredValues
-- UConCon
-- TODO expansion and recovery
uncoveredValues ((k@(ConstructorPattern pname pParams), _):ps) tmap (kv@(ConstructorPattern vname uParams):us)
        | pname == vname = map (kcon k) (uncoveredValues ([] ++ ps) tmap ([] ++ us))
        | otherwise      = [kv:us]
-- UConVar
uncoveredValues (p@(ConstructorPattern _ _, typeName):ps) tmap (VariablePattern _:us) =
        concatMap (\constructor ->  uncoveredValues (p:ps) tmap (constructor:us)) allConstructors
    where
        allConstructors = fromMaybe (error $ "Lookup for type " ++ typeName ++ " failed") (Map.lookup typeName tmap)
-- UVar
uncoveredValues ((VariablePattern _, _):ps) tmap (u:us) = map (ucon u) (uncoveredValues ps tmap us)
uncoveredValues _ _ _ = error "non exhaustive patterns in uncoveredValues"




-- |Refines the VA of viable inputs using the pattern vector
patVecProc :: PatternVector -> ValueAbstractionSet -> TypeMap -> ClauseCoverage
patVecProc ps s tmap = ClauseCoverage c u d
    where
        c = concatMap (coveredValues ps) s
        u = concatMap (uncoveredValues ps tmap) s
        d = []


prettyIteratedVecProc :: Integer -> [PatternVector] -> ValueAbstractionSet -> TypeMap -> IO ()

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
kcon _ _ = error "Only constructor patterns"
