module ClauseProcessing where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.Map             as Map
import           DataDefs
import           Types
import           Util
import           Debug.Trace



-- | Extends map function to only work on the valueAbstraction component
patMap :: (ValueAbstractionVector -> ValueAbstractionVector) -> ConditionedValueAbstractionSet -> ConditionedValueAbstractionSet
patMap f
    = map extendedF
    where
        extendedF :: ConditionedValueAbstractionVector -> ConditionedValueAbstractionVector
        extendedF CVAV {valueAbstraction=va, delta=delta} = CVAV {valueAbstraction = f va, delta = delta}


-- Based on Figure 3 of 'GADTs meet their match'

--
-- Implements the 'C' helper function
--
coveredValues :: PatternVector -> ConditionedValueAbstractionVector -> Analyzer ConditionedValueAbstractionSet

-- TODO remove this once we have access to global types
coveredValues x y | trace (show (x, y)) False = error "fail"

-- CNil
coveredValues [] vav@CVAV {valueAbstraction=[], delta=delta}
    = return [vav] -- Important: one empty set to start with, keep constraints

-- TODO remove this once we have access to global types
coveredValues
    ((TruePattern, _):ps)
    CVAV {valueAbstraction=(VariablePattern _:us), delta=delta}
    = do
        -- ConVar + ConCon
        subCovered <- coveredValues ps CVAV {valueAbstraction = us, delta = delta}
        return $ patMap (kcon (ConstructorPattern "True" [])) subCovered

-- CConCon
coveredValues
    ((ConstructorPattern pname args, _):ps)
    CVAV {valueAbstraction=(ConstructorPattern vname up:us), delta=delta}
        | pname == vname = do
            annArgs <- annotatePatterns args
            subs <- substitutePatterns up
            cvs <- coveredValues (annArgs ++ ps) CVAV {valueAbstraction=subs ++ us, delta=delta}
            return $ patMap (kcon (ConstructorPattern pname args)) cvs
        | otherwise      = return []

-- CConVar
coveredValues
    (kk@(k@(ConstructorPattern _ _), _):ps)
    CVAV {valueAbstraction=(VariablePattern varName:us), delta=delta}
    = do
        substituted <- substituteFreshParameters k
        let delta' = (varName ++ " ~~ " ++ show substituted):delta
        coveredValues (kk:ps) CVAV {valueAbstraction=substituted:us, delta=delta'}

-- CVar
coveredValues
    ((VariablePattern varName, _):ps)
    CVAV {valueAbstraction=(u:us), delta=delta}
    = do
        let delta' = (varName ++ " ~~ " ++ show u):delta
        cvs <- coveredValues ps CVAV {valueAbstraction=us, delta=delta}
        return $ patMap (ucon u) cvs

-- CGuard
coveredValues
    ((GuardPattern p constraint, _):ps)
    CVAV {valueAbstraction=us, delta=delta}
    = do
        y <- freshVar
        let VariablePattern varName = y
        let delta' = (varName ++ " ~~ " ++ show constraint):delta
        pWithType <- annotatePattern p
        recursivelyCovered <- coveredValues (pWithType:ps) CVAV {valueAbstraction=y:us, delta=delta'}
        return $ patMap tail recursivelyCovered


coveredValues pat values
    = throwError
    $ UnpredictedError
    $ "coveredValues: unsupported pattern " ++ show pat ++ " with values " ++ show values

--
-- Implements the 'U' helper function
--
uncoveredValues :: PatternVector -> ConditionedValueAbstractionSet -> Analyzer ConditionedValueAbstractionSet

-- UNil
uncoveredValues [] []
    = return [] -- Important! This is different than coveredValues

-- TODO remove this once we have access to global types
uncoveredValues
    ((TruePattern, _):ps)
    CVAV {valueAbstraction=(VariablePattern _:us), delta=delta}
    = do
        subUncovered <- uncoveredValues ps us
        return $ map (kcon (ConstructorPattern "True" [])) subUncovered


-- UConCon
uncoveredValues
    (k@(ConstructorPattern pname args, _):ps)
    CVAV {valueAbstraction=(kv@(ConstructorPattern vname up):us), delta=delta}
        | pname == vname = do
            annArgs <- annotatePatterns up
            uvs <- uncoveredValues (annArgs ++ ps) CVAV {valueAbstraction=up ++ us, delta=delta}
            return $ map (kcon k) uvs
        | otherwise      = do
            substitute <- substituteFreshParameters kv
            return CVAV {valueAbstraction = [substitute:us], delta = delta }

-- UConVar
uncoveredValues (p@(ConstructorPattern _ _, typeName):ps) (VariablePattern _:us)
    = do
    allConstructors <- lookupConstructors typeName
    allConstructorsWithFreshParameters <- mapM substituteFreshParameters allConstructors
    uvs <- forM allConstructorsWithFreshParameters $ \constructor ->
        uncoveredValues (p:ps) (constructor:us)
    return $ concat uvs

-- UVar
uncoveredValues ((VariablePattern _, _):ps) (u:us)
    = do
    uvs <- uncoveredValues ps us
    return $ map (ucon u) uvs

-- UGuard
uncoveredValues ((GuardPattern p constraint, _):ps) us = do
    y <- freshVar
    pWithType <- annotatePattern p
    recursivelyUncovered <- uncoveredValues (pWithType:ps) (y:us)
    -- return $ map tail recursivelyUncovered
    return []

uncoveredValues pat values
    = throwError
    $ UnpredictedError
    $ "uncoveredValues: unsupported pattern " ++ show pat ++ " with values " ++ show values



--
-- Implements the 'D' helper function
--
divergentValues :: PatternVector -> ValueAbstractionVector -> Analyzer ValueAbstractionSet

-- DNil
divergentValues [] []
    = return [] -- Important! This is different than coveredValues

-- TODO remove this once we have access to global types
divergentValues ((TruePattern, _):ps) (VariablePattern _:us) = do
    subDivergent <- divergentValues ps us
    return $ map (kcon (ConstructorPattern "True" [])) subDivergent


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
    subs <- substituteFreshParameters pc
    dvs <- divergentValues (p:ps) (subs:us)
    return $ (pc:us) : dvs

-- DVar
divergentValues ((VariablePattern _, _):ps) (u:us)
    = do
    dvs <- divergentValues ps us
    return $ map (ucon u) dvs

-- DGuard
divergentValues ((GuardPattern p constraint, _):ps) us = do
    y <- freshVar
    pWithType <- annotatePattern p
    recursiveDivergent <- divergentValues (pWithType:ps) (y:us)
    -- return $ map tail recursiveDivergent
    return []

divergentValues pat values
    = throwError
    $ UnpredictedError
    $ "divergentValues: unsupported pattern " ++ show pat ++ " with values " ++ show values


-- | Refines the VA of viable inputs using the pattern vector
patVecProc :: PatternVector -> ValueAbstractionSet -> Analyzer ClauseCoverage
patVecProc ps s = do
    cvs <- concat <$> mapM (coveredValues ps) initialCVAS
    uvs <- concat <$> mapM (uncoveredValues ps) initialCVAS
    dvs <- concat <$> mapM (divergentValues ps) s
    return $ ClauseCoverage (extractValueAbstractions cvs) (extractValueAbstractions uvs) dvs
    where
        initialCVAS = withNoConstraints s

-- | Constructs ConditionedValueAbstractionSet without any conditions on each abstraction
withNoConstraints :: ValueAbstractionSet -> ConditionedValueAbstractionSet
withNoConstraints
    = map
        (\vector -> CVAV {valueAbstraction = vector, delta = []})

-- | SAT-check the constraints and return the abstractions
extractValueAbstractions :: ConditionedValueAbstractionSet -> ValueAbstractionSet
extractValueAbstractions (cvav:vs) | trace (show cvav) True
    = valueAbstraction cvav:extractValueAbstractions vs
extractValueAbstractions [] = []

iteratedVecProc :: [PatternVector] -> ValueAbstractionSet -> Analyzer ExecutionTrace
iteratedVecProc [] _ = return []
iteratedVecProc (ps:pss) s = do
    res <- patVecProc ps s
    rest <- iteratedVecProc pss (capU res)
    return $ res : rest

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

freshVar :: Analyzer Pattern
freshVar = do
    i <- gets nextFreshVarName
    modify (\s -> s { nextFreshVarName = i + 1} )
    return $ VariablePattern $ "fresh" ++ show i

-- | Replace PlaceHolderPatterns with appropriate fresh variables
substituteFreshParameters :: Pattern -> Analyzer Pattern
substituteFreshParameters (ConstructorPattern name placeholders) = do
    subs <- substitutePatterns placeholders
    return $ ConstructorPattern name subs
-- TODO add lists and tuples
substituteFreshParameters _ = error "No substitution available"

-- TODO check these are PlaceHolderPatterns only
substitutePatterns :: [Pattern] -> Analyzer [Pattern]
substitutePatterns xs = replicateM (length xs) freshVar

annotatePattern :: Pattern -> Analyzer TypedPattern
annotatePattern p = do
    t <- lookupType p
    return (p, t)

annotatePatterns :: [Pattern] -> Analyzer PatternVector
annotatePatterns = mapM annotatePattern

lookupType :: Pattern -> Analyzer String
-- TODO remove me after we have access to builtins
lookupType TruePattern = return "DummyBuiltinBool"
lookupType constructor = do
    tmap <- ask
    let itmap = invertMap tmap
    case Map.lookup constructor itmap of
        Nothing -> throwError $ TypeNotFound $ "Type lookup for constructor " ++ show constructor ++ " failed"
        Just r -> return r

lookupConstructors :: String -> Analyzer [Pattern]
lookupConstructors typeName = do
    tmap <- ask
    case Map.lookup typeName tmap of
        Nothing -> throwError $ ConstructorNotFound $ "Constructor lookup for type " ++ show typeName ++ " failed"
        Just cs -> return cs
