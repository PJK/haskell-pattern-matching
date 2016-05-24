module Oracle where

import           Data.List         (find)
import           Data.Maybe        (catMaybes)
import           DataDefs
import           Oracle.SBVQueries
import qualified Text.Show.Pretty  as Pr
import           Types

data Oracle
    = Oracle
    { queryOracle :: ConditionedValueAbstractionVector -> IO Bool }


-- TODO make the oracle a separate datatype instead of a builtin function so we can try out using other ones
-- and partially order them? muhaha

-- Just IO?
runOracle :: FunctionResult -> IO SolvedFunctionResult
runOracle (FunctionResult tr) = SolvedFunctionResult <$> mapM runCoverageOracle tr

runCoverageOracle :: ClauseCoverage -> IO SolvedClauseCoverage
runCoverageOracle cc
    = SolvedClauseCoverage
    <$> runCoverageSetOracle (capC cc)
    <*> runCoverageSetOracle (capU cc)
    <*> runCoverageSetOracle (capD cc)

runCoverageSetOracle :: ConditionedValueAbstractionSet -> IO ValueAbstractionSet
runCoverageSetOracle cvas = catMaybes <$> mapM runSingleVectorOracle cvas

runSingleVectorOracle :: ConditionedValueAbstractionVector -> IO (Maybe ValueAbstractionVector)
runSingleVectorOracle cvav = do
    let oracle = myOracle
    satisfiable <- queryOracle oracle cvav
    if satisfiable
    then return $ Just $ valueAbstraction cvav
    else return Nothing


trivialOracle :: Oracle
trivialOracle = Oracle (\_ -> return True)


-- FIXME This whole thing is unsound. For each constraint we need to set it to Unsat, Sat, or Dunno, and then only leave a constraint if it is definitely unsat.
myOracle :: Oracle
myOracle = Oracle { queryOracle = oracleOracleIsThisConditionedValueAbstractionVectorSatisfiable }

-- Mirror, mirror, on the wall, ...
oracleOracleIsThisConditionedValueAbstractionVectorSatisfiable :: ConditionedValueAbstractionVector -> IO Bool
oracleOracleIsThisConditionedValueAbstractionVectorSatisfiable (CVAV _ {-gamma-}_ delta) = do
    putStrLn "Before:"
    putStrLn $ Pr.ppShow delta
    let firstRound = resolveBottoms $ resolveVariableEqualities $ termConstraints delta
    secondRound <- resolveTrueBools firstRound
    thirdRound <- resolveSimpleBools secondRound
    let delta' = delta {
            termConstraints = thirdRound
          , typeConstraints = resolveTrivialTypeEqualities $ typeConstraints delta
        }
    putStrLn "After:"
    putStrLn $ Pr.ppShow delta'

    return $ isUnconstrainedSet delta'


isUnconstrainedSet :: ConstraintSet -> Bool
isUnconstrainedSet d = null (typeConstraints d) && null (termConstraints d)

resolveTrivialTypeEqualities :: [TypeConstraint] -> [TypeConstraint]
resolveTrivialTypeEqualities = filter (uncurry (/=))

-- | Resolve variable equalities
-- That is, for every @VarsEqual v1 v2@, replace all occurrences of v2 with v1 in all the other constraints
resolveVariableEqualities :: [Constraint] -> [Constraint]
resolveVariableEqualities [] = []
resolveVariableEqualities vs
    = case filter isVarEquality vs of
        [] -> vs
        VarsEqual v1 v2 : vs' -> resolveVariableEqualities $ replaceVars v1 v2 vs'
  where
    vareqs = filter isVarEquality vs

    isVarEquality (VarsEqual _ _) = True
    isVarEquality _ = False

    replaceVars v1 v2 = map $ replaceVar v1 v2
    replaceVar :: Name -> Name -> (Constraint -> Constraint)
    replaceVar v1 v2 = mapVarConstraint (\v -> if v == v2 then v1 else v)

-- | Figure out whether the bottoms are satisfiable
-- That is, for every @IsBottom var@ constraint, it is satisfiable if @var@ does not occur in other constraints.
--
-- This only works on lists that already have variables resolved
resolveBottoms :: [Constraint] -> [Constraint]
resolveBottoms [] = []
resolveBottoms (c@(VarEqualsCons _ _ _):cs) = c : resolveBottoms cs
resolveBottoms (c@(VarEqualsBool _ _):cs)   = c : resolveBottoms cs
resolveBottoms (c@(BoolExp _):cs)           = c : resolveBottoms cs
resolveBottoms (c@(VarsEqual _ _):cs)       = c : resolveBottoms cs
resolveBottoms (c@(Uncheckable _):cs)       = c : resolveBottoms cs
resolveBottoms (bc@(IsBottom v):cs)
    = if otherOccurrenceOf v cs
        then bc : resolveBottoms cs
        else resolveBottoms cs

otherOccurrenceOf :: Name -> [Constraint] -> Bool
otherOccurrenceOf var = any occurrence
  where
    occurrence (Uncheckable _) = False
    occurrence (VarsEqual _ _) = error "Variables have to be resolved first"
    occurrence (IsBottom ovar)
            | ovar == var = True
            | otherwise   = False
    occurrence (BoolExp be) = notElem var $ varsInBE be
    occurrence (VarEqualsBool ovar _)
            | ovar == var = True
            | otherwise   = False
    occurrence (VarEqualsCons ovar _ _)
            | ovar == var = True
            | otherwise   = False

-- | Solve boolean expressions that have to be true
--
-- We solve a pair of constraints of the following form with a SAT solver and remove both if the boolean expression is satisfiable.
-- This works best if variable equalities are resolved.
-- Unresolved bottoms don't matter.
--
-- We also assume that all constraints are unique, this is fine because A ∧ A <=> A holds anyway
--
-- TODO maybe cache queries, we'll need them three times per query anyway...
resolveTrueBools :: [Constraint] -> IO [Constraint]
resolveTrueBools cs = case filter isVarsEqualBoolConstraint cs of
    [] -> return cs
    vareq@(VarEqualsBool var be):_ -> do
        let isMatchingConsTrueConstraint (VarEqualsCons ovar "True" []) = var == ovar
            isMatchingConsTrueConstraint _ = False
        case find isMatchingConsTrueConstraint cs of
            Nothing -> return cs
            Just conseq -> do
                sat <- boolESat $ BoolOp BoolEQ (LitBool True) be
                let leftout = filter (\c -> c /= vareq && c /= conseq) cs
                if sat
                then resolveSimpleBools leftout
                else do
                    rest <- resolveSimpleBools leftout
                    return $ vareq:conseq:rest

-- | Solve independent boolean constraints
--
-- If a constraint @VarEqualsBool var be@ occurs such that @var@ doesn't occur in any other constraint, then we can try to SAT-solve @be to satisfy the constraint
-- This has to happen after resolving variable equalities
resolveSimpleBools :: [Constraint] -> IO [Constraint]
resolveSimpleBools [] = return []
resolveSimpleBools cs =
    case find isVarsEqualBoolConstraint cs of
        Nothing -> return cs
        Just c@(VarEqualsBool var be) -> do
            let filtered = filter (/= c) cs
            if otherOccurrenceOf var filtered
            then (c :) <$> resolveSimpleBools filtered
            else do
                sat <- boolESat be
                if sat
                then resolveSimpleBools filtered
                else (c :) <$> resolveSimpleBools filtered






isVarsEqualBoolConstraint (VarEqualsBool var boolE) = True
isVarsEqualBoolConstraint _ = False

varsInBE :: BoolE -> [Name]
varsInBE (LitBool _)            = []
varsInBE (BoolNot be)           = varsInBE be
varsInBE (BoolOp _ be1 be2)     = varsInBE be1 ++ varsInBE be2
varsInBE (IntBoolOp _ ie1 ie2)  = varsInIE ie1 ++ varsInIE ie2
varsInBE (FracBoolOp _ fe1 fe2) = varsInFE fe1 ++ varsInFE fe2
varsInBE (BoolVar var)          = [var]

varsInIE :: IntE -> [Name]
varsInIE (IntLit _)             = []
varsInIE (IntUnOp _ ie)         = varsInIE ie
varsInIE (IntOp _ ie1 ie2)      = varsInIE ie1 ++ varsInIE ie2
varsInIE (IntVar var)           = [var]

varsInFE :: FracE -> [Name]
varsInFE (FracLit _)            = []
varsInFE (FracUnOp _ fe)        = varsInFE fe
varsInFE (FracOp _ fe1 fe2)     = varsInFE fe1 ++ varsInFE fe2
varsInFE (FracVar var)          = [var]

-- TODO move these to Oracle.Utils

mapVarConstraint :: (Name -> Name) -> Constraint -> Constraint
mapVarConstraint f (BoolExp be) = BoolExp $ mapVarBE f be
mapVarConstraint f (IsBottom var) = IsBottom $ f var
mapVarConstraint f (VarsEqual v1 v2) = VarsEqual (f v1) (f v2)
mapVarConstraint _ uc@(Uncheckable _) = uc

mapVarBE :: (Name -> Name) -> BoolE -> BoolE
mapVarBE f (BoolVar var) = BoolVar $ f var
mapVarBE f (BoolNot bn) = BoolNot $ mapVarBE f bn
mapVarBE f (BoolOp bo be1 be2) = BoolOp bo (mapVarBE f be1) (mapVarBE f be2)
mapVarBE f (IntBoolOp ibo ie1 ie2) = IntBoolOp ibo (mapVarIE f ie1) (mapVarIE f ie2)
mapVarBE f (FracBoolOp fbo fe1 fe2) = FracBoolOp fbo (mapVarFE f fe1) (mapVarFE f fe2)
mapVarBE _ b = b

mapVarIE :: (Name -> Name) -> IntE -> IntE
mapVarIE = undefined

mapVarFE :: (Name -> Name) -> FracE -> FracE
mapVarFE = undefined







