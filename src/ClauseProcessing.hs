module ClauseProcessing where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.Map             as Map
import           Data.Maybe            (fromJust)
import qualified Data.Foldable as DFo
import           DataDefs
import           Debug.Trace
import           Types
import           Util
import           Gatherer



-- | Extends map function to only work on the valueAbstraction component
patMap :: (ValueAbstractionVector -> ValueAbstractionVector) -> ConditionedValueAbstractionSet -> ConditionedValueAbstractionSet
patMap f
    = map extendedF
    where
        extendedF :: ConditionedValueAbstractionVector -> ConditionedValueAbstractionVector
        extendedF CVAV {valueAbstraction=va
                       , delta=delta
                       , gamma=gamma} = CVAV {valueAbstraction = f va, delta = delta, gamma = gamma}

-- | Creates CGuard, UGuard, DGuard implementations
-- Syd: Can we do something about all the params?
type AnalysisProcessor = PatternVector -> ConditionedValueAbstractionVector -> Analyzer ConditionedValueAbstractionSet

guardHandler :: AnalysisProcessor -> Pattern -> Constraint -> PatternVector -> ValueAbstractionVector -> [Constraint]-> Binding -> Analyzer ConditionedValueAbstractionSet
guardHandler func p constraint ps us delta gamma
    = do
        z <- freshVar
        let delta' = addGuardConstraint z constraint delta
        let gamma' = Map.insert (varName z) booleanType gamma
        recurse <- func (p:ps) CVAV {valueAbstraction = z:us, delta = delta', gamma = gamma'}
        return $ patMap tail recurse

-- Based on Figure 3 of 'GADTs meet their match'

--
-- Implements the 'C' helper function
--
coveredValues :: PatternVector -> ConditionedValueAbstractionVector -> Analyzer ConditionedValueAbstractionSet

-- coveredValues x y | trace ("C: " ++ show (x, y)) False = error "fail"

-- CNil
coveredValues [] vav@CVAV {valueAbstraction=[]}
    = return [vav] -- Important: one empty set to start with, keep constraints

-- CConCon
coveredValues
    (ConstructorPattern pname args:ps)
    CVAV {valueAbstraction=(ConstructorPattern vname up:us), delta=delta, gamma=gamma}
        | pname == vname = do
            cvs <- coveredValues (args ++ ps) CVAV {valueAbstraction = up ++ us, delta = delta, gamma = gamma}
            return $ patMap (kcon (ConstructorPattern pname args)) cvs
        | otherwise      = return []

-- CConVar
coveredValues
    (k@(ConstructorPattern _ _):ps)
    CVAV {valueAbstraction=(VariablePattern varName:us), delta=delta, gamma=gamma}
    = do
        substituted <- substituteFreshParameters k
        let delta' = (Uncheckable $ varName ++ " ~~ " ++ show substituted):delta
        patternGamma <- substitutedConstructorContext substituted
        let gamma' = Map.union gamma patternGamma
        coveredValues (k:ps) CVAV {valueAbstraction = substituted:us, delta = delta', gamma = gamma'}


-- CVar
coveredValues
    (VariablePattern x:ps)
    CVAV {valueAbstraction=(u@(VariablePattern uName):us), delta=delta, gamma=gamma}
    = do
        -- Substitute x (which depends on the type definition and may occur many times)
        -- with a fresh variable (must have the same meaning)
        x' <- freshVar
        let delta' = (Uncheckable $ x ++ " ~~ " ++ show u):delta
        uType <- lookupVariableType uName gamma
        let gamma' = Map.insert (varName x') uType gamma
        cvs <- coveredValues ps CVAV {valueAbstraction = us, delta = delta', gamma = gamma'}
        return $ patMap (ucon u) cvs

-- CGuard
coveredValues
    (GuardPattern p constraint:ps)
    CVAV {valueAbstraction=us, delta=delta, gamma=gamma}
    = guardHandler coveredValues p constraint ps us delta gamma

coveredValues pat values
    = throwError
    $ UnpredictedError
    $ "coveredValues: unsupported pattern " ++ show pat ++ " with values " ++ show values

--
-- Implements the 'U' helper function
--
uncoveredValues :: PatternVector -> ConditionedValueAbstractionVector -> Analyzer ConditionedValueAbstractionSet

uncoveredValues x y | trace ("U: " ++ show (x, y)) False = error "fail"

-- UNil
uncoveredValues [] CVAV {valueAbstraction=[], delta=_}
    = return [] -- Important! This is different than coveredValues


-- UConCon
uncoveredValues
    (k@(ConstructorPattern pname pargs):ps)
    CVAV {valueAbstraction=(kv@(ConstructorPattern vname up):us), delta=delta, gamma=gamma}
        | pname == vname = do
            uvs <- uncoveredValues (pargs ++ ps) CVAV {valueAbstraction = up ++ us, delta = delta, gamma = gamma}
            return $ patMap (kcon k) uvs
        | otherwise      = do
            substitute <- substituteFreshParameters kv
            return [CVAV {valueAbstraction = substitute:us, delta = delta, gamma = gamma}]

-- UConVar
uncoveredValues
    (pat@(ConstructorPattern _ _):ps)
    CVAV {valueAbstraction=(VariablePattern varName:us), delta=delta, gamma=gamma}
    = do
        constructorType <- lookupType pat
        allConstructors <- lookupConstructors constructorType
        allConstructorsWithFreshParameters <- mapM substituteFreshParameters allConstructors

        patternGammas <- mapM substitutedConstructorContext allConstructorsWithFreshParameters
        let gamma' = foldr Map.union gamma patternGammas

        uvs <- forM allConstructorsWithFreshParameters $ \constructor ->
            let delta' = (Uncheckable $ varName ++ " ~~ " ++ show constructor):delta in
                uncoveredValues (pat:ps) CVAV {valueAbstraction=constructor:us, delta=delta', gamma = gamma'}
        return $ concat uvs

-- UVar
uncoveredValues
    (VariablePattern x:ps)
    CVAV {valueAbstraction=(u@(VariablePattern uName):us), delta=delta, gamma=gamma}
    = do
        -- Substitute x (which depends on the type definition and may occur many times)
        -- with a fresh variable (must have the same meaning)
        x' <- freshVar
        let delta' = (Uncheckable $ x ++ " ~~ " ++ show u):delta
        uType <- lookupVariableType uName gamma
        let gamma' = Map.insert (varName x') uType gamma
        cvs <- uncoveredValues ps CVAV {valueAbstraction = us, delta = delta', gamma = gamma'}
        return $ patMap (ucon u) cvs

-- UGuard
uncoveredValues
    (GuardPattern p constraint:ps)
    CVAV {valueAbstraction=us, delta=delta, gamma=gamma}
    = guardHandler uncoveredValues p constraint ps us delta gamma


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
    (ConstructorPattern pname args:ps)
    CVAV {valueAbstraction=(ConstructorPattern vname up:us), delta=delta, gamma=gamma}
        | pname == vname = do
            cvs <- divergentValues
                        (args ++ ps)
                        CVAV {valueAbstraction = up ++ us, delta = delta, gamma = gamma}
            return $ patMap (kcon (ConstructorPattern pname args)) cvs
        | otherwise      = return []


-- DConVar
divergentValues
    (p@(ConstructorPattern _ _):ps)
    CVAV {valueAbstraction=(var@(VariablePattern varName):us), delta=delta, gamma=gamma}
        = do
            substituted <- substituteFreshParameters p
            let delta' = (Uncheckable $ varName ++ " ~~ " ++ show substituted):delta
            let deltaBot = (Uncheckable $ varName ++ "~~" ++ "bottom"):delta
            patternGamma <- substitutedConstructorContext substituted
            let gamma' = Map.union gamma patternGamma
            dvs <- divergentValues (p:ps) CVAV {valueAbstraction = substituted:us, delta = delta', gamma = gamma'}
            return $ CVAV {valueAbstraction = var:us, delta = deltaBot, gamma = gamma}:dvs

-- DVar
divergentValues
    (VariablePattern x:ps)
    CVAV {valueAbstraction=(u@(VariablePattern uName):us), delta=delta, gamma=gamma}
    = do
        -- Substitute x (which depends on the type definition and may occur many times)
        -- with a fresh variable (must have the same meaning)
        x' <- freshVar
        let delta' = (Uncheckable $ x ++ " ~~ " ++ show u):delta
        uType <- lookupVariableType uName gamma
        let gamma' = Map.insert (varName x') uType gamma
        cvs <- divergentValues ps CVAV {valueAbstraction = us, delta = delta', gamma = gamma'}
        return $ patMap (ucon u) cvs

-- DGuard
divergentValues
    (GuardPattern p constraint:ps)
    CVAV {valueAbstraction=us, delta=delta, gamma=gamma}
    = guardHandler divergentValues p constraint ps us delta gamma

divergentValues pat values
    = throwError
    $ UnpredictedError
    $ "divergentValues: unsupported pattern " ++ show pat ++ " with values " ++ show values


addGuardConstraint :: Pattern -> Constraint -> [Constraint] -> [Constraint]
addGuardConstraint (VariablePattern varName) constraint delta = (Uncheckable $ varName ++ " ~~ " ++ show constraint):delta
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
extractValueAbstractions = id


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

-- | Maps Constructors to their Types
lookupType :: Pattern -> Analyzer DataType
lookupType constructor@(ConstructorPattern constructorName _) = do
    universe <- ask
    case DFo.find containsConstructor universe of
        Nothing -> throwError $ TypeNotFound $ "Type lookup for constructor " ++ show constructor ++ " failed (searched in " ++ show universe ++ ")"
        Just r -> return r
    where
        containsConstructor :: DataType -> Bool
        containsConstructor (DataType _ _ constructors)
            = any (\(Constructor name _) -> name == constructorName) constructors

-- | Extracts all constructors for the type and turns them into patterns
-- TODO rename this - this no longer does look ups
lookupConstructors :: DataType -> Analyzer [Pattern]
lookupConstructors (DataType _ _ constructors)
    = mapM constructorToPattern constructors -- Wooo I did a thing with a monad

lookupConstructorDefinition :: Pattern -> Analyzer Constructor
lookupConstructorDefinition constructor@(ConstructorPattern constructorName _) = do
    DataType _ _ constructors  <- lookupType constructor
    return $ fromJust (DFo.find sameName constructors)
    where
        sameName (Constructor name _) = name == constructorName


constructorToPattern :: Constructor -> Analyzer Pattern
constructorToPattern (Constructor name types) = do
        -- don't expand the parameters - this can be done at the next step if forced by the pattern
        params <- replicateM (length types) freshVar
        return $ ConstructorPattern name params

-- | Takes a pattern with freshly substituted parameters and produces a binding map for them
substitutedConstructorContext :: Pattern -> Analyzer Binding
substitutedConstructorContext construtorPattern@(ConstructorPattern name vars) = do
    Constructor _ types <- lookupConstructorDefinition construtorPattern
    let varNames = map varName vars
    return $ Map.fromList (zip varNames types)

lookupVariableType :: String -> Binding -> Analyzer Type
lookupVariableType name gamma
    = case Map.lookup name gamma of
        Nothing -> throwError $ VariableNotBound $ "Type lookup for variable " ++ name ++ " failed (searched in " ++ show gamma ++ ")"
        Just x  -> return x