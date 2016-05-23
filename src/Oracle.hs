module Oracle where

import           Data.Maybe (catMaybes)
import           DataDefs
import           Types

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
    satisfiable <- oracleOracleIsThisConditionedValueAbstractionVectorSatisfiable cvav
    if satisfiable
    then return $ Just $ valueAbstraction cvav
    else return Nothing

-- Mirror, mirror, on the wall, ...
oracleOracleIsThisConditionedValueAbstractionVectorSatisfiable :: ConditionedValueAbstractionVector -> IO Bool
oracleOracleIsThisConditionedValueAbstractionVectorSatisfiable _ = return True
