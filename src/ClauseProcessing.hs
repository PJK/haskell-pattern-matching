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
uncoveredValues :: PatternVector -> ConditionedValueAbstractionVector -> Analyzer ConditionedValueAbstractionSet

-- UNil
uncoveredValues [] CVAV {valueAbstraction=[], delta=_}
    = return [] -- Important! This is different than coveredValues


-- TODO remove this once we have access to global types
uncoveredValues
    -- TODO fix this for global types
    ((TruePattern, _):ps)
    CVAV {valueAbstraction=(VariablePattern _:us), delta=delta}
    = do
        subUncovered <- uncoveredValues ps CVAV {valueAbstraction = us, delta = delta}
        return $ patMap (kcon (ConstructorPattern "True" [])) subUncovered


-- UConCon
uncoveredValues
    ((k@(ConstructorPattern pname args), _):ps)
    CVAV {valueAbstraction=(kv@(ConstructorPattern vname up):us), delta=delta}
        | pname == vname = do
            annArgs <- annotatePatterns up
            uvs <- uncoveredValues (annArgs ++ ps) CVAV {valueAbstraction=up ++ us, delta=delta}
            return $ patMap (kcon k) uvs
        | otherwise      = do
            substitute <- substituteFreshParameters kv
            return [CVAV {valueAbstraction = substitute:us, delta = delta }]

-- UConVar
uncoveredValues
    (p@(ConstructorPattern _ _, typeName):ps)
    CVAV {valueAbstraction=(VariablePattern varName:us), delta=delta}
    = do
        allConstructors <- lookupConstructors typeName
        allConstructorsWithFreshParameters <- mapM substituteFreshParameters allConstructors
        uvs <- forM allConstructorsWithFreshParameters $ \constructor ->
            let delta' = (varName ++ " ~~ " ++ show constructor):delta in
                uncoveredValues (p:ps) CVAV {valueAbstraction=constructor:us, delta=delta}
        return $ concat uvs

-- UVar
uncoveredValues
    ((VariablePattern varName, _):ps)
    CVAV {valueAbstraction=(u:us), delta=delta}
    = do
        let delta' = (varName ++ " ~~ " ++ show u):delta
        cvs <- uncoveredValues ps CVAV {valueAbstraction=us, delta=delta}
        return $ patMap (ucon u) cvs

-- UGuard
uncoveredValues
    ((GuardPattern p constraint, _):ps)
    CVAV {valueAbstraction=us, delta=delta}
    = do
        y <- freshVar
        let VariablePattern varName = y
        let delta' = (varName ++ " ~~ " ++ show constraint):delta
        pWithType <- annotatePattern p
        recursivelyUncovered <- uncoveredValues (pWithType:ps) CVAV {valueAbstraction=y:us, delta=delta'}
        return $ patMap tail recursivelyUncovered


uncoveredValues pat values
    = throwError
    $ UnpredictedError
    $ "uncoveredValues: unsupported pattern " ++ show pat ++ " with values " ++ show values



--
-- Implements the 'D' helper function
--
divergentValues :: PatternVector -> ConditionedValueAbstractionVector -> Analyzer ConditionedValueAbstractionSet

-- DNil
divergentValues [] CVAV {valueAbstraction=[], delta=_}
    = return [] -- Important! This is different than coveredValues

-- TODO remove this once we have access to global types
divergentValues
    -- TODO fix this for global types
    ((TruePattern, _):ps)
    CVAV {valueAbstraction=(VariablePattern _:us), delta=delta}
    = do
        subUncovered <- uncoveredValues ps CVAV {valueAbstraction = us, delta = delta}
        return $ patMap (kcon (ConstructorPattern "True" [])) subUncovered



-- DConCon
divergentValues
    ((ConstructorPattern pname args, _):ps)
    CVAV {valueAbstraction=(ConstructorPattern vname up:us), delta=delta}
        | pname == vname = do
            annArgs <- annotatePatterns args
            subs <- substitutePatterns up
            cvs <- divergentValues (annArgs ++ ps) CVAV {valueAbstraction=subs ++ us, delta=delta}
            return $ patMap (kcon (ConstructorPattern pname args)) cvs
        | otherwise      = return []


-- DConVar
divergentValues
    (p@(pc@(ConstructorPattern _ _), _):ps)
    CVAV {valueAbstraction=(var@(VariablePattern varName):us), delta=delta}
        = do
            substituted <- substituteFreshParameters pc
            let delta' = (varName ++ " ~~ " ++ show substituted):delta
            let deltaBot = (varName ++ "~~" ++ "bottom"):delta
            dvs <- divergentValues (p:ps) CVAV {valueAbstraction = substituted:us, delta = delta'}
            return $ CVAV {valueAbstraction = var:us, delta = deltaBot}:dvs

-- DVar
divergentValues
    ((VariablePattern varName, _):ps)
    CVAV {valueAbstraction=(u:us), delta=delta}
    = do
        let delta' = (varName ++ " ~~ " ++ show u):delta
        cvs <- divergentValues ps CVAV {valueAbstraction=us, delta=delta}
        return $ patMap (ucon u) cvs

-- DGuard
divergentValues
    ((GuardPattern p constraint, _):ps)
    CVAV {valueAbstraction=us, delta=delta}
    = do
        y <- freshVar
        let VariablePattern varName = y
        let delta' = (varName ++ " ~~ " ++ show constraint):delta
        pWithType <- annotatePattern p
        recursivelyDivergent <- divergentValues (pWithType:ps) CVAV {valueAbstraction=y:us, delta=delta'}
        return $ patMap tail recursivelyDivergent

divergentValues pat values
    = throwError
    $ UnpredictedError
    $ "divergentValues: unsupported pattern " ++ show pat ++ " with values " ++ show values


-- | Refines the VA of viable inputs using the pattern vector
patVecProc :: PatternVector -> ValueAbstractionSet -> Analyzer ClauseCoverage
patVecProc ps s = do
    cvs <- concat <$> mapM (coveredValues ps) initialCVAS
    uvs <- concat <$> mapM (uncoveredValues ps) initialCVAS
    dvs <- concat <$> mapM (divergentValues ps) initialCVAS
    return $ ClauseCoverage (extractValueAbstractions cvs) (extractValueAbstractions uvs) (extractValueAbstractions dvs)
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
