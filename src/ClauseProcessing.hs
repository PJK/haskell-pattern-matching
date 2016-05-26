module ClauseProcessing where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Foldable         as DFo
import qualified Data.Map              as Map
import           Data.Maybe            (fromJust)
import           DataDefs
import           Debug.Trace
import           Gatherer
import           Language.Haskell.Exts hiding (DataOrNew (..), Name (..),
                                        Pretty, Type (..), prettyPrint)
import qualified Text.Show.Pretty      as Pr
import           Types
import           Util



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
-- Pavel: Answer: pass in the CVAV instead of the last three arguments?
type AnalysisProcessor = PatternVector -> ConditionedValueAbstractionVector -> Analyzer ConditionedValueAbstractionSet

guardHandler :: AnalysisProcessor -> Pattern -> Expression -> PatternVector -> ValueAbstractionVector -> ConstraintSet -> Binding -> Analyzer ConditionedValueAbstractionSet
guardHandler func p constraint ps us delta gamma
    = do
        z <- freshVar
        let delta' = addGuardConstraint z constraint delta
        let gamma' = Map.insert (varName z) booleanType gamma
        recurse <- func (p:ps) CVAV {valueAbstraction = z:us, delta = delta', gamma = gamma'}
        return $ patMap tail recurse

-- | Creates CVar, UVar, DVar implementations
varHandler :: AnalysisProcessor -> String -> PatternVector -> Pattern -> PatternVector -> ConstraintSet-> Binding -> Analyzer ConditionedValueAbstractionSet
varHandler func x ps u us delta gamma
    = do
        -- Substitute x (which depends on the type definition and may occur many times)
        -- with a fresh variable (must have the same meaning)
        x' <- freshVar
        uType <- lookupType u gamma

        let delta' = addEqualityConstraint x' u delta
        let gamma' = Map.insert (varName x') uType gamma

        cvs <- func ps CVAV {valueAbstraction = us, delta = delta', gamma = gamma'}
        return $ patMap (ucon u) cvs

-- | We are not able to add all types of equalities. This function takes any variable and
-- | a term and either adds the appropriate constraint, or an Uncheckable constraint
addEqualityConstraint :: Pattern -> Pattern -> ConstraintSet -> ConstraintSet
addEqualityConstraint (VariablePattern aName) (VariablePattern bName) delta
    = addConstraint (VarsEqual aName bName) delta
-- TODO handle other cases and start using this everywhere
addEqualityConstraint a b delta
    = addConstraint (Uncheckable (show a ++ " ~~ " ++ show b)) delta

-- Based on Figure 3 of 'GADTs meet their match'

--
-- Implements the 'C' helper function
--
coveredValues :: PatternVector -> ConditionedValueAbstractionVector -> Analyzer ConditionedValueAbstractionSet

-- coveredValues x y | trace ("C: " ++ Pr.ppShow (x, y)) False = error "fail"

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
        substituted@(ConstructorPattern consName conspats) <- substituteFreshParameters k

        patternGamma <- substitutedConstructorContext substituted
        let gamma' = Map.union gamma patternGamma

        varType <- lookupVariableType varName gamma
        constructorType <- dataTypeToType <$> lookupDataType k

        let delta' = addConstraint (VarEqualsCons varName consName conspats) delta
        let delta'' = addTypeConstraint (varType, constructorType) delta'

        coveredValues (k:ps) CVAV {valueAbstraction = substituted:us, delta = delta'', gamma = gamma'}


-- CVar
coveredValues
    (VariablePattern x:ps)
    CVAV {valueAbstraction=(u:us), delta=delta, gamma=gamma}
    = varHandler coveredValues x ps u us delta gamma


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

--
-- uncoveredValues x y | trace ("U: " ++ Pr.ppShow (x, y)) False = error "fail"

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

            subGamma <- substitutedConstructorContext substitute
            let gamma' = Map.union gamma subGamma

            return [CVAV {valueAbstraction = substitute:us, delta = delta, gamma = gamma'}]

-- UConVar
uncoveredValues
    (pat@(ConstructorPattern consname conspats):ps)
    CVAV {valueAbstraction=(VariablePattern varName:us), delta=delta, gamma=gamma}
    = do
        constructorType <- lookupDataType pat
        allConstructors <- lookupConstructors constructorType
        allConstructorsWithFreshParameters <- mapM substituteFreshParameters allConstructors

        patternGammas <- mapM substitutedConstructorContext allConstructorsWithFreshParameters
        let gamma' = foldr Map.union gamma patternGammas

        varType <- lookupVariableType varName gamma
        constructorType <- dataTypeToType <$> lookupDataType pat

        let delta' = addTypeConstraint (varType, constructorType) delta

        uvs <- forM allConstructorsWithFreshParameters $ \constructor ->
            let delta'' = addConstraint (VarEqualsCons varName consname conspats) delta' in
                uncoveredValues (pat:ps) CVAV {valueAbstraction=constructor:us, delta=delta', gamma = gamma'}
        return $ concat uvs

-- UVar
uncoveredValues
    (VariablePattern x:ps)
    CVAV {valueAbstraction=(u:us), delta=delta, gamma=gamma}
    = varHandler uncoveredValues x ps u us delta gamma


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
    (p@(ConstructorPattern consname conspats):ps)
    CVAV {valueAbstraction=(var@(VariablePattern varName):us), delta=delta, gamma=gamma}
    = do
            substituted <- substituteFreshParameters p

            varType <- lookupVariableType varName gamma
            constructorType <- dataTypeToType <$> lookupDataType p

            let delta' = addConstraint (VarEqualsCons varName consname conspats) delta
            let delta'' = addTypeConstraint (varType, constructorType) delta'

            let deltaBot = addConstraint (IsBottom varName) delta

            patternGamma <- substitutedConstructorContext substituted

            let gamma' = Map.union gamma patternGamma

            dvs <- divergentValues (p:ps) CVAV {valueAbstraction = substituted:us, delta = delta'', gamma = gamma'}
            return $ CVAV {valueAbstraction = var:us, delta = deltaBot, gamma = gamma}:dvs

-- DVar
divergentValues
    (VariablePattern x:ps)
    CVAV {valueAbstraction=(u:us), delta=delta, gamma=gamma}
    = varHandler divergentValues x ps u us delta gamma

-- DGuard
divergentValues
    (GuardPattern p constraint:ps)
    CVAV {valueAbstraction=us, delta=delta, gamma=gamma}
    = guardHandler divergentValues p constraint ps us delta gamma

divergentValues pat values
    = throwError
    $ UnpredictedError
    $ "divergentValues: unsupported pattern " ++ show pat ++ " with values " ++ show values


addGuardConstraint :: Pattern -> Expression -> ConstraintSet -> ConstraintSet
addGuardConstraint (VariablePattern varName) expression ConstraintSet {termConstraints=termC, typeConstraints=typeC}
    = let cons
            = case expression of
                UnknownExp exp -> Uncheckable $ varName ++ " ~~ " ++ show expression
                BExp be -> VarEqualsBool varName be
      in
        ConstraintSet { termConstraints = cons:termC
                    , typeConstraints = typeC
                    }
addGuardConstraint _ _ _ = error "Can only require equality on variables"

addConstraint :: Constraint -> ConstraintSet -> ConstraintSet
addConstraint constraint ConstraintSet {termConstraints=termC, typeConstraints=typeC}
    = ConstraintSet { termConstraints = constraint:termC
                    , typeConstraints = typeC
                    }

addTypeConstraint :: TypeConstraint -> ConstraintSet -> ConstraintSet
addTypeConstraint constraint ConstraintSet {termConstraints=termC, typeConstraints=typeC}
    = ConstraintSet { termConstraints = termC
                    , typeConstraints = constraint:typeC
                    }

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
    rest <- trace ("C: " ++ Pr.ppShow res) iteratedVecProc pss (capU res)
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
    modify (\s -> s { nextFreshVarName = i + 1 })
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

-- | Maps Patterns to their Types
lookupType :: Pattern -> Binding -> Analyzer Type
-- Variables are described by the binding
lookupType (VariablePattern name) binding
-- For Constructors, just see the type it belongs to
    = lookupVariableType name binding
lookupType pat@(ConstructorPattern _ _) _ = do
    dataType <- lookupDataType pat
    return $ dataTypeToType dataType

lookupDataType :: Pattern -> Analyzer DataType
lookupDataType constructor@(ConstructorPattern constructorName _) = do
    universe <- ask
    case DFo.find containsConstructor universe of
        Nothing -> throwError $ TypeNotFound $ "Type lookup for constructor " ++ show constructor ++ " failed (searched in " ++ show universe ++ ")"
        Just r -> return r
    where
        containsConstructor :: DataType -> Bool
        containsConstructor (DataType _ _ constructors)
            = any (\(Constructor name _) -> name == constructorName) constructors

-- TODO I'm not sure if this makes sense?
dataTypeToType :: DataType -> Type
dataTypeToType (DataType name _ _) = TypeConstructor name

-- | Extracts all constructors for the type and turns them into patterns
lookupConstructors :: DataType -> Analyzer [Pattern]
lookupConstructors (DataType _ _ constructors)
    = mapM constructorToPattern constructors -- Wooo I did a thing with a monad

lookupConstructorDefinition :: Pattern -> Analyzer Constructor
lookupConstructorDefinition constructor@(ConstructorPattern constructorName _) = do
    DataType _ _ constructors  <- lookupDataType constructor
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
