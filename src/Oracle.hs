module Oracle where

import           Data.List         (find)
import           Data.Maybe        (catMaybes)
import           Data.SBV
import           DataDefs
import           Debug.Trace
import           Oracle.SBVQueries
import qualified Text.Show.Pretty  as Pr
import           Types

data Oracle
    = Oracle
    { queryOracle :: ConditionedValueAbstractionVector -> IO OracleResult }


-- Just IO?
runOracle :: FunctionResult -> IO SolvedFunctionResult
runOracle (FunctionResult tr) = SolvedFunctionResult <$> mapM runCoverageOracle tr

runCoverageOracle :: ClauseCoverage -> IO SolvedClauseCoverage
runCoverageOracle cc
    = SolvedClauseCoverage
    <$> runCoverageSetOracle (capC cc)
    <*> runCoverageSetOracle (capU cc)
    <*> runCoverageSetOracle (capD cc)

runCoverageSetOracle :: ConditionedValueAbstractionSet -> IO SolvedValueAbstractionSet
runCoverageSetOracle cvas = catMaybes <$> mapM runSingleVectorOracle cvas

runSingleVectorOracle :: ConditionedValueAbstractionVector -> IO (Maybe SolvedValueAbstractionVector)
runSingleVectorOracle cvav = do
    let oracle = myOracle
    satisfiable <- queryOracle oracle cvav
    case satisfiable of
        DefinitelyUnsatisfiable -> return Nothing
        DontReallyKnow -> return $ Just $ SolvedValueAbstractionVector (valueAbstraction cvav) Nothing
        DefinitelySatisfiable be model -> return $ Just $ SolvedValueAbstractionVector (valueAbstraction cvav) (Just (be, model))


trivialOracle :: Oracle
trivialOracle = Oracle (\_ -> return DontReallyKnow)


-- FIXME This whole thing is unsound. For each constraint we need to set it to Unsat, Sat, or Dunno, and then only leave a constraint if it is definitely unsat.
myOracle :: Oracle
myOracle = Oracle { queryOracle = oracleOracleIsThisConditionedValueAbstractionVectorSatisfiable }

-- Mirror, mirror, on the wall, ...
oracleOracleIsThisConditionedValueAbstractionVectorSatisfiable :: ConditionedValueAbstractionVector -> IO OracleResult
oracleOracleIsThisConditionedValueAbstractionVectorSatisfiable (CVAV _ {-gamma-}_ delta) = do
    let firstRound = resolveBottoms $ resolveVariableEqualities $ termConstraints delta
    if any isBottom firstRound
    then return DefinitelyUnsatisfiable
    else do
        let cs = firstRound
        -- We solve term constraints first
        if not (sattable cs)
        then return DontReallyKnow
        else do
            let bes = map convertToBoolE cs
            let be = foldl (BoolOp BoolAnd) (LitBool True) bes
            SatResult satResult <- boolESatResult $ BoolOp BoolEQ (LitBool True) be
            case satResult of
                Unsatisfiable _ -> return DefinitelyUnsatisfiable
                Unknown _ _ -> return DontReallyKnow
                ProofError _ _ -> return DontReallyKnow
                TimeOut _ -> return DontReallyKnow
                Satisfiable _ model ->
                    -- Only if term constraints are definitely satisfiable, then we solve type constraints
                    -- Currently we only do trivial type constraint checking
                    return $ if null $ resolveTrivialTypeEqualities $ typeConstraints delta
                                then DefinitelySatisfiable bes model
                                else DontReallyKnow

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
    convertibleToSat (VarEqualsPat _ _) = False
    convertibleToSat (Uncheckable _) = False

isVarsEqualBoolConstraint (VarEqualsBool var boolE) = True
isVarsEqualBoolConstraint _ = False

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


-- TODO move these to Oracle.Utils

mapVarConstraint :: (Name -> Name) -> Constraint -> Constraint
-- mapVarConstraint f (BoolExp be) = BoolExp $ mapVarBE f be
mapVarConstraint f (IsBottom var) = IsBottom $ f var
mapVarConstraint f (VarsEqual v1 v2) = VarsEqual (f v1) (f v2)
mapVarConstraint f (VarEqualsBool n be) = VarEqualsBool (f n) (mapVarBE f be)
mapVarConstraint f (VarEqualsCons n1 n2 ps) = VarEqualsCons (f n1) n2 ps
mapVarConstraint f uc@(VarEqualsPat n p) = uc -- TODO @Syd can we do better here?
mapVarConstraint _ uc@(Uncheckable _) = uc

mapVarBE :: (Name -> Name) -> BoolE -> BoolE
mapVarBE _ b@(LitBool _) = b
mapVarBE _ Otherwise = Otherwise
mapVarBE f (BoolVar var) = BoolVar $ f var
mapVarBE f (BoolNot bn) = BoolNot $ mapVarBE f bn
mapVarBE f (BoolOp bo be1 be2) = BoolOp bo (mapVarBE f be1) (mapVarBE f be2)
mapVarBE f (IntBoolOp ibo ie1 ie2) = IntBoolOp ibo (mapVarIE f ie1) (mapVarIE f ie2)

mapVarIE :: (Name -> Name) -> IntE -> IntE
mapVarIE _ i@(IntLit _) = i
mapVarIE f (IntVar v) = IntVar $ f v
mapVarIE f (IntUnOp IntNeg ie) = IntUnOp IntNeg $ mapVarIE f ie
mapVarIE f (IntOp io ie1 ie2) = IntOp io (mapVarIE f ie1) (mapVarIE f ie2)

