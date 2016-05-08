module ClauseProcessing where

import qualified Data.Map   as Map
import           DataDefs
import           Util
import           Data.Maybe (fromMaybe)
import           Debug.Trace

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
coveredValues ((ConstructorPattern pname args, _):ps) (kv@(ConstructorPattern vname up):us)
        | pname == vname =
            map (kcon (ConstructorPattern pname args))
                (coveredValues (args ++ ps) (substitutePatterns up ++ us))
        | otherwise      = []
-- CConVarx
coveredValues (kk@(k@(ConstructorPattern _ _), _):ps) (VariablePattern _:us) = coveredValues (kk:ps) (k:us)
-- CVar
coveredValues ((VariablePattern _, _):ps) (u:us) = map (ucon u) (coveredValues ps us)
coveredValues _ _ = error "unsupported pattern"


--
-- Implements the 'U' helper function
--
uncoveredValues :: PatternVector -> SimpleTypeMap  -> ValueAbstractionVector -> ValueAbstractionSet
-- UNil
uncoveredValues [] _ []  = [] -- Important! This is different than coveredValues
-- UConCon
-- TODO expansion and recovery
uncoveredValues ((k@(ConstructorPattern pname pParams), _):ps) tmap (kv@(ConstructorPattern vname uParams):us)
        | pname == vname = traceStack (show (k, kv)) $ map (kcon k) (uncoveredValues (annotatedPParams ++ ps) tmap (uParams ++ us))
        | otherwise      = [kv:us]
    where
        constructorToType = invertMap tmap
        fetchType constructor = fromMaybe (error $ "Lookup for type " ++ show constructor ++ " failed") (Map.lookup constructor constructorToType)
        annotatePatterns = map (\p -> (p, fetchType p))
        annotatedPParams = annotatePatterns pParams

-- UConVar
uncoveredValues (p@(ConstructorPattern _ _, typeName):ps) tmap u@(VariablePattern _:us) | traceStack (show (p, u)) True =
        concatMap (\constructor ->  uncoveredValues (p: ps) tmap (constructor:us)) allConstructorsWithFreshParameters
    where
        allConstructors = fromMaybe (error $ "Lookup for type " ++ typeName ++ " failed") (Map.lookup typeName tmap)
        allConstructorsWithFreshParameters = map substituteFreshParameters allConstructors
-- UVar
uncoveredValues ((VariablePattern _, _):ps) tmap (u:us) = map (ucon u) (uncoveredValues ps tmap us)
uncoveredValues a _ b = traceStack (show (a, b)) $ error "non-exhaustive pattern match"




-- |Refines the VA of viable inputs using the pattern vector
patVecProc :: PatternVector -> ValueAbstractionSet -> SimpleTypeMap -> ClauseCoverage
patVecProc ps s tmap = ClauseCoverage c u d
    where
        c = concatMap (coveredValues ps) s
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
-- TODO make this count new occurrences. This is only relevant for the solver.
freshVars :: Int -> [Pattern]
freshVars 0 = []
freshVars k = VariablePattern "__fresh":freshVars (k - 1)

-- | Replace PlaceHolderPatterns with appropriate fresh variables
substituteFreshParameters :: Pattern -> Pattern
substituteFreshParameters (ConstructorPattern name placeholders) = -- HLint wont parse pattern@...
        ConstructorPattern name (substitutePatterns placeholders)
-- TODO add lists and tuples
substituteFreshParameters _ = error "No substitution available"

-- TODO check these are PlaceHolderPatterns only
substitutePatterns :: [Pattern] -> [Pattern]
substitutePatterns xs = freshVars $ freshVars (length xs)