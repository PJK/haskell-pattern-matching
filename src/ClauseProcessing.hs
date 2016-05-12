module ClauseProcessing where

import           Control.Monad.Reader

import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import           DataDefs
import           Debug.Trace
import           Util

type ExecutionTrace = [ClauseCoverage]
type Analyzer = Reader SimpleTypeMap

data ClauseCoverage = ClauseCoverage
    { capC :: ValueAbstractionSet
    , capU :: ValueAbstractionSet
    , capD :: ValueAbstractionSet
    } deriving (Show, Eq)

-- Based on Figure 3 of 'GADTs meet their match'

--
-- Implements the 'C' helper function
--
coveredValues :: PatternVector -> ValueAbstractionVector -> Analyzer ValueAbstractionSet

-- CNil
coveredValues [] []
    = return [[]] -- Important: one empty set to start with

-- CConCon
coveredValues ((ConstructorPattern pname args, _):ps) (ConstructorPattern vname up:us)
        | pname == vname = do
            annArgs <- annotatePatterns args
            cvs <- coveredValues (annArgs ++ ps) (substitutePatterns up ++ us)
            return $ map (kcon (ConstructorPattern pname args)) cvs
        | otherwise      = return []

-- CConVarx
coveredValues (kk@(k@(ConstructorPattern _ _), _):ps) (VariablePattern _:us) =
    -- Now this fresh variable substitution has no effect as there are are free
    -- variables in the pattern vector. Once we start using solver, we need separate
    -- names for them
    coveredValues (kk:ps) (substituteFreshParameters k:us)

-- CVar
coveredValues ((VariablePattern _, _):ps) (u:us) = do
    cvs <- coveredValues ps us
    return $ map (ucon u) cvs

-- TODO CGuard

coveredValues _ _ = error "unsupported pattern"


--
-- Implements the 'U' helper function
--
uncoveredValues :: PatternVector -> ValueAbstractionVector -> Analyzer ValueAbstractionSet

-- UNil
uncoveredValues [] []
    = return [] -- Important! This is different than coveredValues

-- UConCon
uncoveredValues ((k@(ConstructorPattern pname pParams), _):ps) (kv@(ConstructorPattern vname uParams):us)
    | pname == vname = do
        annArgs <- annotatePatterns pParams
        uvs <- uncoveredValues (annArgs ++ ps) (uParams ++ us)
        return $ map (kcon k) uvs
    | otherwise      = return [substituteFreshParameters kv:us]

-- UConVar
uncoveredValues (p@(ConstructorPattern _ _, typeName):ps) (VariablePattern _:us)
    = do
    tmap <- ask
    let allConstructors = fromMaybe
                            (error $ "Lookup for type " ++ typeName ++ " failed")
                            (Map.lookup typeName tmap)
        allConstructorsWithFreshParameters = map substituteFreshParameters allConstructors

    uvs <- forM allConstructorsWithFreshParameters $ \constructor ->
        uncoveredValues (p:ps) (constructor:us)
    return $ concat uvs
    -- concatMap
    --     (\constructor ->  uncoveredValues (p:ps) tmap (constructor:us))
    --     allConstructorsWithFreshParameters

-- UVar
uncoveredValues ((VariablePattern _, _):ps) (u:us)
    = do
    uvs <- uncoveredValues ps us
    return $ map (ucon u) uvs

-- TODO UGuard

uncoveredValues a b = traceStack (show (a, b)) $ error "non-exhaustive pattern match"



--
-- Implements the 'D' helper function
--
divergentValues :: PatternVector -> ValueAbstractionVector -> Analyzer ValueAbstractionSet

-- DNil
divergentValues [] []
    = return [] -- Important! This is different than coveredValues

-- DConCon
divergentValues ((k@(ConstructorPattern pname pParams), _):ps) (ConstructorPattern vname uParams:us)
    | pname == vname = do
        annPs <- annotatePatterns pParams
        dvs <- divergentValues (annPs ++ ps) (uParams ++ us)
        return $ map (kcon k) dvs
    | otherwise      = return []

-- DConVar
divergentValues (p@(pc@(ConstructorPattern _ _), _):ps) (VariablePattern _:us)
    = do
    dvs <- divergentValues (p:ps) (substituteFreshParameters pc:us)
    return $ (pc:us) : dvs

-- DVar
divergentValues ((VariablePattern _, _):ps) (u:us)
    = do
    dvs <- divergentValues ps us
    return $ map (ucon u) dvs

-- TODO DGuard

divergentValues a b = traceStack (show (a, b)) $ error "non-exhaustive pattern match"



-- | Refines the VA of viable inputs using the pattern vector
patVecProc :: PatternVector -> ValueAbstractionSet -> Analyzer ClauseCoverage
patVecProc ps s = do
    cvs <- concat <$> mapM (coveredValues ps) s
    uvs <- concat <$> mapM (uncoveredValues ps) s
    let dvs = []
    return $ ClauseCoverage cvs uvs dvs


iteratedVecProc :: [PatternVector] -> ValueAbstractionSet -> Analyzer ExecutionTrace
iteratedVecProc [] _ = return []
iteratedVecProc (ps:pss) s = do
    res <- patVecProc ps s
    rest <- iteratedVecProc pss (capU res)
    return $ res : rest



-- prettyIteratedVecProc :: Integer -> [PatternVector] -> ValueAbstractionSet -> SimpleTypeMap -> IO ()
--
-- prettyIteratedVecProc _ [] vas _ = do
--     print "Final iteration. Uncovered set:"
--     print vas
--
-- prettyIteratedVecProc i (ps:pss) s tmap = do
--         putStrLn $ "Iteration: " ++ show i
--         putStrLn $ "Value abstractions to consider as inputs: " ++ show s
--         putStrLn $ "Pattern: " ++ show ps
--         putStrLn $ "U: " ++ show (capU res)
--         putStrLn $ "C: " ++ show (capC res)
--         putStrLn $ "D: " ++ show (capD res)
--         prettyIteratedVecProc (i + 1) pss (capU res) tmap
--     where
--         res = patVecProc ps s tmap

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

annotatePatterns :: [Pattern] -> Analyzer PatternVector
annotatePatterns ps = do
    tmap <- ask
    let constructorToType = invertMap tmap
        fetchType constructor =
            fromMaybe
                (error $ "Lookup for type " ++ show constructor ++ " failed")
                (Map.lookup constructor constructorToType)
    return $ map (\p -> (p, fetchType p)) ps
