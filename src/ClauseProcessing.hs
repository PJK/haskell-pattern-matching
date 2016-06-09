module ClauseProcessing where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Foldable        as DFo
import qualified Data.Map             as Map
import           Data.Maybe           (fromJust)
import           DataDefs
import           Gatherer
import           Types



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
varHandler :: AnalysisProcessor -> Pattern -> PatternVector -> Pattern -> PatternVector -> ConstraintSet-> Binding -> Analyzer ConditionedValueAbstractionSet
varHandler func x ps u us delta gamma
    = do
        -- Substitute x (which depends on the type definition and may occur many times)
        -- with a fresh variable (must have the same meaning)
        -- x' <- freshVar
        -- uType <- lookupType u gamma

        let delta' = addEqualityConstraint x u delta
--         let gamma' = Map.insert (varName x') uType gamma

        cvs <- func ps CVAV {valueAbstraction = us, delta = delta', gamma = gamma}
        return $ patMap (ucon u) cvs

-- | We are not able to add all types of equalities. This function takes any variable and
-- | a term and either adds the appropriate constraint, or an Uncheckable constraint
addEqualityConstraint :: Pattern -> Pattern -> ConstraintSet -> ConstraintSet
addEqualityConstraint (VariablePattern aName) (VariablePattern bName) delta
    = addConstraint (VarsEqual aName bName) delta
-- TODO handle other cases and start using this everywhere
addEqualityConstraint a b delta
    = addConstraint (Uncheckable (show a ++ " ~~ " ++ show b)) delta

-- Integers have 'Infinitely many constructors' => perform guard magic variable substitution
refreshGuard :: AnalysisProcessor -> PatternVector -> ConditionedValueAbstractionVector -> Analyzer ConditionedValueAbstractionSet
refreshGuard
    ap
    (IntVariablePattern:GuardPattern (ConstructorPattern "True" []) (BExp (IntBoolOp IntEQ (IntVar "__placeholder__") (IntLit val))):ps)
    CVAV {valueAbstraction=us, delta=delta, gamma=gamma}
    = do
        x <- freshVar
        let gamma' = Map.insert (varName x) (TypeConstructor "Int") gamma
        ap (x:GuardPattern (ConstructorPattern "True" []) (BExp (IntBoolOp IntEQ (IntVar (varName x)) (IntLit val))):ps) CVAV {valueAbstraction=us, delta=delta, gamma=gamma'}
refreshGuard _ _ _ = error "Only IntVariablePatterns are refreshable"

-- Based on Figure 3 of 'GADTs meet their match'

--
-- Implements the 'C' helper function
--

{-# ANN module "HLint: ignore Reduce duplication" #-}
coveredValues :: PatternVector -> ConditionedValueAbstractionVector -> Analyzer ConditionedValueAbstractionSet

-- CNil
coveredValues [] vav@CVAV {valueAbstraction=[]}
    = return [vav] -- Important: one empty set to start with, keep constraints


-- Integers magic dust
coveredValues (IntVariablePattern:gp:ps) cvav = refreshGuard coveredValues (IntVariablePattern:gp:ps) cvav

-- CConCon
coveredValues
    (ConstructorPattern pname args:ps)
    CVAV {valueAbstraction=(ConstructorPattern vname up:us), delta=delta, gamma=gamma}
        | pname == vname = do
            cvs <- coveredValues (args ++ ps) CVAV {valueAbstraction = up ++ us, delta = delta, gamma = gamma}
            return $ patMap (kcon (ConstructorPattern pname args)) cvs
        | otherwise      = return []


coveredValues
    (TuplePattern pp:ps)
    CVAV {valueAbstraction=(TuplePattern up:us), delta=delta, gamma=gamma}
    = do
        cvs <- coveredValues (pp ++ ps) CVAV {valueAbstraction = up ++ us, delta = delta, gamma = gamma}
        return $ patMap (kcon (TuplePattern pp)) cvs

coveredValues
    (EmptyListPattern:ps)
    CVAV {valueAbstraction=(EmptyListPattern:us), delta=delta, gamma=gamma}
    = do
        cvs <- coveredValues ps CVAV {valueAbstraction = us, delta = delta, gamma = gamma}
        return $ patMap (kcon EmptyListPattern) cvs

coveredValues
     (InfixConstructorPattern p1 ":" p2:ps)
     CVAV {valueAbstraction=(InfixConstructorPattern u1 ":" u2:us), delta=delta, gamma=gamma}
     = do
        cvs <- coveredValues (p1:p2:ps) CVAV {valueAbstraction = u1:u2:us, delta = delta, gamma = gamma}
        return $ patMap (kcon (InfixConstructorPattern p1 ":" p2)) cvs

coveredValues
     (InfixConstructorPattern {}:_)
     CVAV {valueAbstraction=(EmptyListPattern:_)}
     = return []

coveredValues
     (EmptyListPattern:_)
     CVAV {valueAbstraction=(InfixConstructorPattern {}:_)}
     = return []

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


coveredValues
    (k@(TuplePattern _):ps)
    CVAV {valueAbstraction=(VariablePattern varName:us), delta=delta, gamma=gamma}
    = do
        substituted@(TuplePattern _) <- substituteFreshParameters k

        varType <- lookupVariableType varName gamma
        let patternGamma = substitutedPatternContext substituted varType

        let gamma' = Map.union gamma patternGamma

        let delta' = addConstraint (VarEqualsPat varName k) delta
        -- TC would be trivial, skip it

        coveredValues (k:ps) CVAV {valueAbstraction = substituted:us, delta = delta', gamma = gamma'}

coveredValues
    (k@(InfixConstructorPattern _ ":" _):ps)
    CVAV {valueAbstraction=(VariablePattern varName:us), delta=delta, gamma=gamma}
    = do
        substituted@(InfixConstructorPattern _ ":" _) <- substituteFreshParameters k

        varType <- lookupVariableType varName gamma
        let patternGamma = substitutedPatternContext substituted varType

        let gamma' = Map.union gamma patternGamma
        consType <- lookupType substituted gamma'

        let delta' = addConstraint (VarEqualsPat varName k) delta
        let delta'' = addTypeConstraint (varType, consType) delta'

        coveredValues (k:ps) CVAV {valueAbstraction = substituted:us, delta = delta'', gamma = gamma'}

coveredValues
    (EmptyListPattern:ps)
    CVAV {valueAbstraction=(VariablePattern varName:us), delta=delta, gamma=gamma}
    = do
        -- varType <- lookupVariableType varName gamma

        let delta' = addConstraint (VarEqualsPat varName EmptyListPattern) delta

        coveredValues (EmptyListPattern:ps) CVAV {valueAbstraction = EmptyListPattern:us, delta = delta', gamma = gamma}


-- CVar
coveredValues
    (x@(VariablePattern _):ps)
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

-- UNil
uncoveredValues [] CVAV {valueAbstraction=[], delta=_}
    = return [] -- Important! This is different than coveredValues

-- uncoveredValues x y | trace ("U: " ++ Pr.ppShow (x, y)) False = undefined

-- Integers magic dust
uncoveredValues (IntVariablePattern:gp:ps) cvav = refreshGuard uncoveredValues (IntVariablePattern:gp:ps) cvav

-- UConCon
uncoveredValues
    (k@(ConstructorPattern pname pargs):ps)
    CVAV {valueAbstraction=(kv@(ConstructorPattern vname up):us), delta=delta, gamma=gamma}
        | pname == vname = do
            uvs <- uncoveredValues (pargs ++ ps) CVAV {valueAbstraction = up ++ us, delta = delta, gamma = gamma}
            return $ patMap (kcon k) uvs
        | otherwise      =
            return [CVAV {valueAbstraction = kv:us, delta = delta, gamma = gamma}]

uncoveredValues
    (k@(TuplePattern pp):ps)
    CVAV {valueAbstraction=(TuplePattern up:us), delta=delta, gamma=gamma}
    = do
        uvs <- uncoveredValues (pp ++ ps) CVAV {valueAbstraction = up ++ us, delta = delta, gamma = gamma}
        return $ patMap (kcon k) uvs

uncoveredValues
    (EmptyListPattern:ps)
    CVAV {valueAbstraction=(EmptyListPattern:us), delta=delta, gamma=gamma}
    = do
        uvs <- uncoveredValues ps CVAV {valueAbstraction = us, delta = delta, gamma = gamma}
        return $ patMap (kcon EmptyListPattern) uvs

uncoveredValues
    (k@(InfixConstructorPattern p1 ":" p2):ps)
    CVAV {valueAbstraction=(InfixConstructorPattern u1 ":" u2:us), delta=delta, gamma=gamma}
    = do
        uvs <- uncoveredValues (p1:p2:ps) CVAV {valueAbstraction = u1:u2:us, delta = delta, gamma = gamma}
        return $ patMap (kcon k) uvs

uncoveredValues
    (InfixConstructorPattern {}:_)
    cvav@CVAV {valueAbstraction=(EmptyListPattern:_)}
        = return [cvav]

uncoveredValues
    (EmptyListPattern:_)
    cvav@CVAV {valueAbstraction=(InfixConstructorPattern {}:_)}
        = return [cvav]

-- UConVar
uncoveredValues
    (pat@(ConstructorPattern _ _):ps)
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

        uvs <- forM allConstructorsWithFreshParameters $ \constructor@(ConstructorPattern consname conspats)->
            let delta'' = addConstraint (VarEqualsCons varName consname conspats) delta' in
                uncoveredValues (pat:ps) CVAV {valueAbstraction=constructor:us, delta=delta'', gamma = gamma'}
        return $ concat uvs

uncoveredValues
    (k@(TuplePattern _):ps)
    CVAV {valueAbstraction=(VariablePattern varName:us), delta=delta, gamma=gamma}
    = do
        -- There is only one constructor for tuples, saving our asses
        substituted@(TuplePattern _) <- substituteFreshParameters k

        varType <- lookupVariableType varName gamma
        let patternGamma = substitutedPatternContext substituted varType
        let gamma' = Map.union gamma patternGamma
        consType <- lookupType substituted gamma'

        let delta' = addConstraint (VarEqualsPat varName k) delta
        let delta'' = addTypeConstraint (varType, consType) delta'

        -- There is only one constructor for tuples
        uncoveredValues (k:ps) CVAV {valueAbstraction = substituted:us, delta = delta'', gamma = gamma'}

uncoveredValues
    (k@(InfixConstructorPattern _ ":" _):ps)
    CVAV {valueAbstraction=(VariablePattern varName:us), delta=delta, gamma=gamma}
    = do
        substituted@(InfixConstructorPattern _ ":" _) <- substituteFreshParameters k

        varType <- lookupVariableType varName gamma
        let patternGamma = substitutedPatternContext substituted varType

        let gamma' = Map.union gamma patternGamma

        let deltaCons = addConstraint (VarEqualsPat varName k) delta
        let deltaEmpty = addConstraint (VarEqualsPat varName EmptyListPattern) delta

        -- Consider :
        withConcat <- uncoveredValues (k:ps) CVAV {valueAbstraction=substituted:us, delta=deltaCons, gamma = gamma'}
        -- Consider []
        withEmpty <- uncoveredValues (k:ps) CVAV {valueAbstraction=EmptyListPattern:us, delta=deltaEmpty, gamma = gamma'}

        return $ withConcat ++ withEmpty

uncoveredValues
    (EmptyListPattern:ps)
    CVAV {valueAbstraction=(VariablePattern varName:us), delta=delta, gamma=gamma}
    = do
        s1 <- freshVar
        s2 <- freshVar
        let substituted = InfixConstructorPattern s1 ":" s2
        varType <- lookupVariableType varName gamma
        let patternGamma = substitutedPatternContext substituted varType

        let gamma' = Map.union gamma patternGamma

        let deltaCons = addConstraint (VarEqualsPat varName substituted) delta
        let deltaEmpty = addConstraint (VarEqualsPat varName EmptyListPattern) delta

        -- Consider :
        withConcat <- uncoveredValues (EmptyListPattern:ps) CVAV {valueAbstraction=substituted:us, delta=deltaCons, gamma = gamma'}
        -- Consider []
        withEmpty <- uncoveredValues (EmptyListPattern:ps) CVAV {valueAbstraction=EmptyListPattern:us, delta=deltaEmpty, gamma = gamma'}

        return $ withConcat ++ withEmpty


-- UVar
uncoveredValues
    (x@(VariablePattern _):ps)
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
divergentValues [] CVAV {valueAbstraction=[]}
    = return [] -- Important! This is different than coveredValues

-- Integers magic dust
divergentValues (IntVariablePattern:gp:ps) cvav = refreshGuard divergentValues (IntVariablePattern:gp:ps) cvav

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

divergentValues
    (k@(TuplePattern pp):ps)
    CVAV {valueAbstraction=(TuplePattern up:us), delta=delta, gamma=gamma}
        = do
            dvs <- divergentValues (pp ++ ps) CVAV {valueAbstraction = up ++ us, delta = delta, gamma = gamma}
            return $ patMap (kcon k) dvs

divergentValues
    (k@(InfixConstructorPattern p1 ":" p2):ps)
    CVAV {valueAbstraction=(InfixConstructorPattern u1 ":" u2:us), delta=delta, gamma=gamma}
        = do
            dvs <- divergentValues (p1:p2:ps) CVAV {valueAbstraction = u1:u2:us, delta = delta, gamma = gamma}
            return $ patMap (kcon k) dvs

divergentValues
    (EmptyListPattern:ps)
    CVAV {valueAbstraction=(EmptyListPattern:us), delta=delta, gamma=gamma}
    = do
        cvs <- coveredValues ps CVAV {valueAbstraction = us, delta = delta, gamma = gamma}
        return $ patMap (kcon EmptyListPattern) cvs

divergentValues
     (InfixConstructorPattern {}:_)
     CVAV {valueAbstraction=(EmptyListPattern:_)}
     = return []

divergentValues
     (EmptyListPattern:_)
     CVAV {valueAbstraction=(InfixConstructorPattern {}:_)}
     = return []

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


divergentValues
    (p@(TuplePattern _):ps)
    CVAV {valueAbstraction=(var@(VariablePattern varName):us), delta=delta, gamma=gamma}
    = do
       substituted <- substituteFreshParameters p
       varType <- lookupVariableType varName gamma

       let patternGamma = substitutedPatternContext substituted varType
       let gamma' = Map.union gamma patternGamma

       constructorType <- lookupType substituted gamma'

       let delta' = addConstraint (VarEqualsPat varName substituted) delta
       let delta'' = addTypeConstraint (varType, constructorType) delta'

       let deltaBot = addConstraint (IsBottom varName) delta

       dvs <- divergentValues (p:ps) CVAV {valueAbstraction = substituted:us, delta = delta'', gamma = gamma'}
       return $ CVAV {valueAbstraction = var:us, delta = deltaBot, gamma = gamma}:dvs

divergentValues
    (p@(InfixConstructorPattern _ ":" _):ps)
    CVAV {valueAbstraction=(var@(VariablePattern varName):us), delta=delta, gamma=gamma}
    = do
        substituted <- substituteFreshParameters p

        varType <- lookupVariableType varName gamma
        let patternGamma = substitutedPatternContext substituted varType
        let gamma' = Map.union gamma patternGamma

        constructorType <- lookupType substituted gamma'

        let delta' = addConstraint (VarEqualsPat varName substituted) delta
        let delta'' = addTypeConstraint (varType, constructorType) delta'

        let deltaBot = addConstraint (IsBottom varName) delta

        let patternGamma = substitutedPatternContext substituted varType

        let gamma' = Map.union gamma patternGamma

        dvs <- divergentValues (p:ps) CVAV {valueAbstraction = substituted:us, delta = delta'', gamma = gamma'}
        return $ CVAV {valueAbstraction = var:us, delta = deltaBot, gamma = gamma}:dvs

divergentValues
    (p@EmptyListPattern:ps)
    CVAV {valueAbstraction=(var@(VariablePattern varName):us), delta=delta, gamma=gamma}
    = do
        substituted <- substituteFreshParameters p

        varType <- lookupVariableType varName gamma
        let patternGamma = substitutedPatternContext substituted varType
        let gamma' = Map.union gamma patternGamma

        constructorType <- lookupType substituted gamma'

        let delta' = addConstraint (VarEqualsPat varName substituted) delta
        let delta'' = addTypeConstraint (varType, constructorType) delta'

        let deltaBot = addConstraint (IsBottom varName) delta

        let patternGamma = substitutedPatternContext substituted varType

        let gamma' = Map.union gamma patternGamma

        dvs <- divergentValues (p:ps) CVAV {valueAbstraction = substituted:us, delta = delta'', gamma = gamma'}
        return $ CVAV {valueAbstraction = var:us, delta = deltaBot, gamma = gamma}:dvs


-- DVar
divergentValues
    (x@(VariablePattern _):ps)
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
                UnknownExp _ -> Uncheckable $ varName ++ " ~~ " ++ show expression
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
    rest <- iteratedVecProc pss $ capU res
    return $ res : rest

-- | Coverage vector concatenation
ucon :: Pattern -> ValueAbstractionVector -> ValueAbstractionVector
ucon x xs = x:xs

-- Syd: I basicall want kcon :: ConstructorPattern -> ValueAbstractionVector -> ValueAbstractionVector
kcon :: Pattern -> ValueAbstractionVector -> ValueAbstractionVector
kcon (ConstructorPattern name parameters) ws =
        ConstructorPattern name (take arity ws):drop arity ws
    where
        arity = length parameters
kcon (TuplePattern parameters) ws =
        TuplePattern (take arity ws):drop arity ws
    where
        arity = length parameters
kcon (InfixConstructorPattern _ name _) (a:b:ws) =
        InfixConstructorPattern a name b:ws
kcon EmptyListPattern ws = EmptyListPattern:ws
kcon _ _ = error "Only constructor patterns"

freshVar :: Analyzer Pattern
freshVar = do
    i <- gets nextFreshVarName
    modify (\s -> s { nextFreshVarName = i + 1 })
    return $ VariablePattern $ "~" ++ varnames !! i
  where
    varnames :: [String]
    varnames = go 0
      where
        go 0 = map (:[]) alphabet ++ go 1
        go n = [ as ++ [a] | a <- alphabet, as <- go (n - 1)]
    alphabet = ['a'..'z']

-- | Replace PlaceHolderPatterns with appropriate fresh variables
substituteFreshParameters :: Pattern -> Analyzer Pattern
substituteFreshParameters (ConstructorPattern name placeholders) = do
    subs <- substitutePatterns placeholders
    return $ ConstructorPattern name subs
substituteFreshParameters (TuplePattern placeholders) = do
    subs <- substitutePatterns placeholders
    return $ TuplePattern subs
substituteFreshParameters (InfixConstructorPattern _ name _) = do
    s1 <- freshVar
    s2 <- freshVar
    return $ InfixConstructorPattern s1 name s2
substituteFreshParameters EmptyListPattern = return EmptyListPattern
substituteFreshParameters _ = error "No substitution available"

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
-- This is list specific.
lookupType (InfixConstructorPattern p1 ":" _) binding = do
    elemType <- lookupType p1 binding
    return $ ListType elemType
lookupType EmptyListPattern _ = return $ ListType (VariableType "a")
lookupType (TuplePattern pats) binding = do
    types <- forM pats (`lookupType` binding)
    return $ TupleType types
lookupType (LiteralPattern _ _) _ = undefined
lookupType _ _ = undefined
-- lookupType x _ = trace (show x) (error "Cannot lookup non-contructors or variables")

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
lookupDataType _ = undefined

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
lookupConstructorDefinition _ = undefined


constructorToPattern :: Constructor -> Analyzer Pattern
constructorToPattern (Constructor name types) = do
        -- don't expand the parameters - this can be done at the next step if forced by the pattern
        params <- replicateM (length types) freshVar
        return $ ConstructorPattern name params

-- | Takes a pattern with freshly substituted parameters and produces a binding map for them
substitutedConstructorContext :: Pattern -> Analyzer Binding
substitutedConstructorContext construtorPattern@(ConstructorPattern _ vars) = do
    Constructor _ types <- lookupConstructorDefinition construtorPattern
    let varNames = map varName vars
    return $ Map.fromList (zip varNames types)
substitutedConstructorContext _ = undefined

substitutedPatternContext :: Pattern -> Type -> Binding
substitutedPatternContext (TuplePattern varsToAnnotate) (TupleType types)
    = Map.fromList (zip varNames types)
    where
        varNames = map varName varsToAnnotate
substitutedPatternContext (InfixConstructorPattern p1 _ p2) lt@(ListType t)
    = Map.fromList [(varName p1, t), (varName p2, lt)]
substitutedPatternContext EmptyListPattern _
    = Map.empty
substitutedPatternContext _ _ = undefined


lookupVariableType :: String -> Binding -> Analyzer Type
lookupVariableType name gamma
    = case Map.lookup name gamma of
        Nothing -> throwError $ VariableNotBound $ "Type lookup for variable " ++ name ++ " failed (searched in " ++ show gamma ++ ")"
        Just x  -> return x
