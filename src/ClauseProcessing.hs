module ClauseProcessing where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.Map             as Map
import           DataDefs
import           Debug.Trace
import           Types
import           Util



-- | Extends map function to only work on the valueAbstraction component
patMap :: (ValueAbstractionVector -> ValueAbstractionVector) -> ConditionedValueAbstractionSet -> ConditionedValueAbstractionSet
patMap f
    = map extendedF
    where
        extendedF :: ConditionedValueAbstractionVector -> ConditionedValueAbstractionVector
        extendedF CVAV {valueAbstraction=va, delta=delta} = CVAV {valueAbstraction = f va, delta = delta}

-- | Creates CGuard, UGuard, DGuard implementations
-- Syd: Can we do something about all the params?
type AnalysisProcessor = PatternVector -> ConditionedValueAbstractionVector -> Analyzer ConditionedValueAbstractionSet

guardHandler :: AnalysisProcessor -> Pattern -> Constraint -> PatternVector -> ValueAbstractionVector -> [Constraint]-> Analyzer ConditionedValueAbstractionSet
guardHandler func p constraint ps us delta
    = do
        z <- freshVar
        let delta' = addGuardConstraint z constraint delta
        pWithType <- annotatePattern p
        recurse <- func (pWithType:ps) CVAV {valueAbstraction=z:us, delta=delta'}
        return $ patMap tail recurse

-- Based on Figure 3 of 'GADTs meet their match'

--
-- Implements the 'C' helper function
--
coveredValues :: PatternVector -> ConditionedValueAbstractionVector -> Analyzer ConditionedValueAbstractionSet

-- coveredValues x y | trace (show (x, y)) False = error "fail"

-- CNil
coveredValues [] vav@CVAV {valueAbstraction=[], delta=_}
    = return [vav] -- Important: one empty set to start with, keep constraints

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
        cvs <- coveredValues ps CVAV {valueAbstraction=us, delta=delta'}
        return $ patMap (ucon u) cvs

-- CGuard
coveredValues
    ((GuardPattern p constraint, _):ps)
    CVAV {valueAbstraction=us, delta=delta}
    = guardHandler coveredValues p constraint ps us delta

coveredValues pat values
    = throwError
    $ UnpredictedError
    $ "coveredValues: unsupported pattern " ++ show pat ++ " with values " ++ show values

--
-- Implements the 'U' helper function
--
uncoveredValues :: PatternVector -> ConditionedValueAbstractionVector -> Analyzer ConditionedValueAbstractionSet

-- uncoveredValues x y | trace (show (x, y)) False = error "fail"

-- UNil
uncoveredValues [] CVAV {valueAbstraction=[], delta=_}
    = return [] -- Important! This is different than coveredValues


-- UConCon
uncoveredValues
    ((k@(ConstructorPattern pname pargs), _):ps)
    CVAV {valueAbstraction=(kv@(ConstructorPattern vname up):us), delta=delta}
        | pname == vname = do
            annArgs <- annotatePatterns pargs
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
                uncoveredValues (p:ps) CVAV {valueAbstraction=constructor:us, delta=delta'}
        return $ concat uvs

-- UVar
uncoveredValues
    ((VariablePattern varName, _):ps)
    CVAV {valueAbstraction=(u:us), delta=delta}
    = do
        let delta' = (varName ++ " ~~ " ++ show u):delta
        cvs <- uncoveredValues ps CVAV {valueAbstraction=us, delta=delta'}
        return $ patMap (ucon u) cvs

-- UGuard
uncoveredValues
    ((GuardPattern p constraint, _):ps)
    CVAV {valueAbstraction=us, delta=delta}
    = guardHandler uncoveredValues p constraint ps us delta


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
        cvs <- divergentValues ps CVAV {valueAbstraction=us, delta=delta'}
        return $ patMap (ucon u) cvs

-- DGuard
divergentValues
    ((GuardPattern p constraint, _):ps)
    CVAV {valueAbstraction=us, delta=delta}
    = guardHandler divergentValues p constraint ps us delta

divergentValues pat values
    = throwError
    $ UnpredictedError
    $ "divergentValues: unsupported pattern " ++ show pat ++ " with values " ++ show values


addGuardConstraint :: Pattern -> Constraint -> [Constraint] -> [Constraint]
addGuardConstraint (VariablePattern varName) constraint delta = (varName ++ " ~~ " ++ show constraint):delta
addGuardConstraint _ _ _ = error "Can only require equality on variables"

-- | Refines the VA of viable inputs using the pattern vector
patVecProc :: PatternVector -> ConditionedValueAbstractionSet -> Analyzer ClauseCoverage
patVecProc ps s = do
    cvs <- concat <$> mapM (coveredValues ps) s
    uvs <- concat <$> mapM (uncoveredValues ps) s
    dvs <- concat <$> mapM (divergentValues ps) s
    return $ ClauseCoverage (extractValueAbstractions cvs) (extractValueAbstractions uvs) (extractValueAbstractions dvs)

-- | SAT-check the constraints and return the abstractions
extractValueAbstractions :: ConditionedValueAbstractionSet -> ConditionedValueAbstractionSet
extractValueAbstractions (cvav:vs)
    = {- trace ("Mock-SATing: " ++ show cvav) $ -} cvav:extractValueAbstractions vs
extractValueAbstractions [] = []


iteratedVecProc :: [PatternVector] -> ConditionedValueAbstractionSet -> Analyzer ExecutionTrace
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
    modify (\s -> s { nextFreshVarName = i + 1 } )
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
