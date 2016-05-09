module ClauseProcessing where

import qualified Data.Map   as Map
import           DataDefs
import           Util
import           Data.Maybe (fromMaybe)
import           Debug.Trace


data ClauseCoverage = ClauseCoverage { capC :: ValueAbstractionSet, capU :: ValueAbstractionSet, capD :: ValueAbstractionSet } deriving Show

-- Based on Figure 3 of 'GADTs meet their match'

--
-- Implements the 'C' helper function
--
coveredValues :: PatternVector -> SimpleTypeMap -> ValueAbstractionVector -> ValueAbstractionSet

-- CNil
coveredValues [] _ [] = [[]] -- Important: one empty set to start with

-- CConCon
coveredValues ((ConstructorPattern pname args, _):ps) tmap (ConstructorPattern vname up:us)
        | pname == vname =
            map (kcon (ConstructorPattern pname args))
                (coveredValues (annotatedArguments ++ ps) tmap (substitutePatterns up ++ us))
        | otherwise      = []
        where
            annotatedArguments = annotatePatterns tmap args

-- CConVarx
coveredValues (kk@(k@(ConstructorPattern _ _), _):ps) tmap (VariablePattern _:us) =
    -- Now this fresh variable substitution has no effect as there are are free
    -- variables in the pattern vector. Once we start using solver, we need separate
    -- names for them
    coveredValues (kk:ps) tmap (substituteFreshParameters k:us)

-- CVar
coveredValues ((VariablePattern _, _):ps) tmap (u:us) =
    map (ucon u) (coveredValues ps tmap us)

-- TODO CGuard

coveredValues _ _ _ = error "unsupported pattern"


--
-- Implements the 'U' helper function
--
uncoveredValues :: PatternVector -> SimpleTypeMap  -> ValueAbstractionVector -> ValueAbstractionSet

-- UNil
uncoveredValues [] _ []  = [] -- Important! This is different than coveredValues

-- UConCon
uncoveredValues ((k@(ConstructorPattern pname pParams), _):ps) tmap (kv@(ConstructorPattern vname uParams):us)
    | pname == vname =
        map (kcon k) (uncoveredValues (annotatedPParams ++ ps) tmap (uParams ++ us))
    | otherwise      =
        [substituteFreshParameters kv:us]
    where
        annotatedPParams = annotatePatterns tmap pParams

-- UConVar
uncoveredValues (p@(ConstructorPattern _ _, typeName):ps) tmap (VariablePattern _:us) =
    concatMap
        (\constructor ->  uncoveredValues (p:ps) tmap (constructor:us))
        allConstructorsWithFreshParameters
    where
        allConstructors = fromMaybe
                            (error $ "Lookup for type " ++ typeName ++ " failed")
                            (Map.lookup typeName tmap)
        allConstructorsWithFreshParameters = map substituteFreshParameters allConstructors

-- UVar
uncoveredValues ((VariablePattern _, _):ps) tmap (u:us) = map (ucon u) (uncoveredValues ps tmap us)

-- TODO UGuard

uncoveredValues a _ b = traceStack (show (a, b)) $ error "non-exhaustive pattern match"



--
-- Implements the 'D' helper function
--
divergentValues :: PatternVector -> SimpleTypeMap  -> ValueAbstractionVector -> ValueAbstractionSet

-- DNil
divergentValues [] _ []  = [] -- Important! This is different than coveredValues

-- DConCon
divergentValues ((k@(ConstructorPattern pname pParams), _):ps) tmap (kv@(ConstructorPattern vname uParams):us)
    | pname == vname =
        map (kcon k) (divergentValues (annotatedPParams ++ ps) tmap (uParams ++ us))
    | otherwise      = []
    where
        annotatedPParams = annotatePatterns tmap pParams

-- DConVar
divergentValues (p@(pc@(ConstructorPattern _ _), typeName):ps) tmap (u@(VariablePattern _):us) =
    (pc:us) : divergentValues (p:ps) tmap (substituteFreshParameters pc:us)

-- DVar
divergentValues ((VariablePattern _, _):ps) tmap (u:us) = map (ucon u) (divergentValues ps tmap us)

-- TODO DGuard

divergentValues a _ b = traceStack (show (a, b)) $ error "non-exhaustive pattern match"



-- | Refines the VA of viable inputs using the pattern vector
patVecProc :: PatternVector -> ValueAbstractionSet -> SimpleTypeMap -> ClauseCoverage
patVecProc ps s tmap =
    ClauseCoverage c u d
    where
        c = concatMap (coveredValues ps tmap) s
        u = concatMap (uncoveredValues ps tmap) s
        d = []


prettyIteratedVecProc :: Integer -> [PatternVector] -> ValueAbstractionSet -> SimpleTypeMap -> IO ()

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
kcon :: Pattern -> ValueAbstractionVector -> ValueAbstractionVector
kcon (ConstructorPattern name parameters) ws =
        ConstructorPattern name (take arity ws):drop arity ws
    where
        arity = length parameters
kcon _ _ = error "Only constructor patterns"

-- |Get fresh variables
-- TODO make this count new occurrences. (It is only relevant for the solver)
freshVars :: Int -> [Pattern]
freshVars 0 = []
freshVars k = VariablePattern ("__fresh" ++ show k):freshVars (k - 1)

-- | Replace PlaceHolderPatterns with appropriate fresh variables
substituteFreshParameters :: Pattern -> Pattern
substituteFreshParameters (ConstructorPattern name placeholders) =
        ConstructorPattern name (substitutePatterns placeholders)
-- TODO add lists and tuples
substituteFreshParameters _ = error "No substitution available"

-- TODO check these are PlaceHolderPatterns only
substitutePatterns :: [Pattern] -> [Pattern]
substitutePatterns xs = freshVars $ length xs

annotatePatterns :: SimpleTypeMap -> [Pattern] -> PatternVector
annotatePatterns tmap =
    map (\p -> (p, fetchType p))
    where
        constructorToType = invertMap tmap
        fetchType constructor =
            fromMaybe
                (error $ "Lookup for type " ++ show constructor ++ " failed")
                (Map.lookup constructor constructorToType)