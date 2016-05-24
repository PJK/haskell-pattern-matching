module Oracle where

import           Data.List         (find)
import           Data.Maybe        (catMaybes)
import           DataDefs
import           Debug.Trace
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
    -- putStrLn "Before:"
    -- putStrLn $ Pr.ppShow delta
    let firstRound = resolveBottoms $ resolveVariableEqualities $ termConstraints delta
    if any isBottom firstRound
    then -- do
        -- putStrLn "Unsat because of bottom that occurs in other constraints too"
        return False
    else do
        secondRound <- resolveSatBools firstRound
        let delta' = delta {
                termConstraints = secondRound
              , typeConstraints = resolveTrivialTypeEqualities $ typeConstraints delta
            }
        -- putStrLn "After:"
        -- putStrLn $ Pr.ppShow delta'

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
    = case find isVarEquality vs of
        Nothing -> vs
        Just c@(VarsEqual v1 v2) ->
            let filtered = filter (/= c) vs
            in resolveVariableEqualities $ replaceVars v1 v2 filtered
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
resolveBottoms cs =
    case find isBottom cs of
        Nothing -> cs
        Just c@(IsBottom var) -> do
            let filtered = filter (/= c) cs
            if otherOccurrenceOf var filtered
            then c : resolveBottoms filtered
            else resolveBottoms filtered

isBottom (IsBottom _) = True
isBottom _ = False

otherOccurrenceOf :: Name -> [Constraint] -> Bool
otherOccurrenceOf var = any occurrence
  where
    occurrence (Uncheckable _) = False
    occurrence (VarsEqual ovar1 ovar2)
            | ovar1 == var || ovar2 == var = True
            | otherwise                    = False
    occurrence (IsBottom ovar)
            | ovar == var = True
            | otherwise   = False
    occurrence (VarEqualsBool ovar _)
            | ovar == var = True
            | otherwise   = False
    occurrence (VarEqualsCons ovar _ _)
            | ovar == var = True
            | otherwise   = False


resolveSatBools :: [Constraint] -> IO [Constraint]
resolveSatBools cs
    | not (sattable cs) = return cs
    | otherwise = do
        let clauses = map convertToBoolE cs
        let be = foldl (BoolOp BoolAnd) (LitBool True) clauses
        sat <- boolESat $ BoolOp BoolEQ (LitBool True) be
        return $ if sat
                    then []
                    else cs
  where
    -- The most literal translation possible, for now.
    convertToBoolE (IsBottom be) = error "cannot occur as per 'not (sattable cs)'"
    convertToBoolE (VarsEqual v1 v2) = BoolOp BoolEQ (BoolVar v1) (BoolVar v2)
    convertToBoolE (VarEqualsBool v be) = BoolOp BoolEQ (BoolVar v) be
    convertToBoolE (VarEqualsCons v "True" []) = BoolOp BoolEQ (BoolVar v) (LitBool True)
    convertToBoolE (VarEqualsCons v "False" []) = BoolOp BoolEQ (BoolVar v) (LitBool False)
    convertToBoolE (VarEqualsCons _ _ _) = error "cannot occur either"
    convertToBoolE (Uncheckable _) = error "cannot occur either"


sattable :: [Constraint] -> Bool
sattable = all convertibleToSat
  where
    convertibleToSat (IsBottom _) = False
    convertibleToSat (VarsEqual _ _) = True
    convertibleToSat (VarEqualsBool _ _) = True
    convertibleToSat (VarEqualsCons _ "True" []) = True
    convertibleToSat (VarEqualsCons _ "False" []) = True
    convertibleToSat (VarEqualsCons _ _ _) = False
    convertibleToSat (Uncheckable _) = False

isVarsEqualBoolConstraint (VarEqualsBool var boolE) = True
isVarsEqualBoolConstraint _ = False

-- TODO move these to Oracle.Utils

mapVarConstraint :: (Name -> Name) -> Constraint -> Constraint
-- mapVarConstraint f (BoolExp be) = BoolExp $ mapVarBE f be
mapVarConstraint f (IsBottom var) = IsBottom $ f var
mapVarConstraint f (VarsEqual v1 v2) = VarsEqual (f v1) (f v2)
mapVarConstraint f (VarEqualsBool n be) = VarEqualsBool (f n) (mapVarBE f be)
mapVarConstraint f (VarEqualsCons n1 n2 ps) = VarEqualsCons (f n1) n2 ps
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




