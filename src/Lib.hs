module Lib where


import           ClauseProcessing
import           Control.Monad              (forM_, replicateM)
import           Control.Monad.Except       (runExceptT)
import           Control.Monad.Reader       (runReader)
import           Control.Monad.State        (evalStateT)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Map                   as Map
import           Data.Maybe                 (mapMaybe)
import           DataDefs
import           Debug.Trace
import           Gatherer
import           Language.Haskell.Exts      hiding (DataOrNew (..), Name (..),
                                             Pretty, Type (..), prettyPrint)
import           Language.Haskell.Exts      (fromParseResult, parseFile)
import           OptParse
import           OptParse.Types
import           Oracle
import qualified Text.Show.Pretty           as Pr
import           Types


patterns :: IO ()
patterns = do
    -- svbTest
    sets <- getSettings
    -- print sets
    res <- processTarget (setsTargetFile sets)
    case setsCommand sets of
        Analyze -> prettyOutput res
        DumpResults -> LB8.putStrLn $ encodePretty res

processTarget :: FilePath -> IO AnalysisResult
processTarget inputFile = do
    ast <- fromParseResult <$> parseFile inputFile
    -- print ast
    let ass = AnalysisAssigment inputFile ast
    processAssignment ass

processAssignment :: AnalysisAssigment -> IO AnalysisResult
processAssignment (AnalysisAssigment _ ast)
    = case (,) <$> getFunctions ast <*> getTypeUniverse ast of
        Left err -> return $ AnalysisError $ GatherError err
        Right (fs, ptcm) -> do
            let targets = map FunctionTarget fs
                initState = AnalyzerState 0
            -- LB.putStr $ encodePretty targets
            case flip runReader ptcm $ flip evalStateT initState $ runExceptT $ mapM analyzeFunction targets of
                Left err -> return $ AnalysisError $ ProcessError err
                Right res -> do
                    -- Solve the constraints
                    sres <- mapM runOracle res
                    let tups = zip targets sres
                    return $ AnalysisSuccess $ concatMap (uncurry produceRecommendations) tups

-- TODO actually solve the constraints, now I'm just ignoring them, which is like having an oracle that returns true.

-- TODO replace FunctionResult with (solved Conditioned)ValueAbstractionSet.
produceRecommendations :: FunctionTarget -> SolvedFunctionResult -> [Recommendation]
produceRecommendations t@(FunctionTarget (Function name _ clss)) (SolvedFunctionResult tr)
    =  map (Recommendation name)
    $  findNonExhaustives tr
    ++ findRedundants tr
    ++ findInaccessibleRhss tr
  where
    findNonExhaustives :: SolvedExecutionTrace -> [RecommendationReason]
    findNonExhaustives [] = []
    findNonExhaustives tr
        = case scapU $ last tr of
            [] -> []
            cvavs ->  [NonExhaustive $ map Clause cvavs]

    findRedundants :: SolvedExecutionTrace -> [RecommendationReason]
    findRedundants tr = mapMaybe checkRedundant $ zip tr clss
        where
            checkRedundant :: (SolvedClauseCoverage, Clause) -> Maybe RecommendationReason
            checkRedundant (cc, c)
                | null (scapC cc) && null (scapD cc) = Just $ Redundant c
                | otherwise = Nothing

    findInaccessibleRhss :: SolvedExecutionTrace -> [RecommendationReason]
    findInaccessibleRhss tr = mapMaybe checkInaccessibleRhs $ zip tr clss
        where
            checkInaccessibleRhs :: (SolvedClauseCoverage, Clause) -> Maybe RecommendationReason
            checkInaccessibleRhs (cc, c)
                | null (scapC cc) && (not . null) (scapD cc) = Just $ InaccessibleRhs c
                | otherwise = Nothing

prettyOutput :: AnalysisResult -> IO ()
prettyOutput (AnalysisError err) = print err
prettyOutput (AnalysisSuccess recs) = forM_ recs $ \(Recommendation n r) -> do
    putStrLn $ "In function " ++ n ++ ":"
    case r of
        Redundant c -> do
            putStrLn "The following clause is redundant:"
            printClause n c
        InaccessibleRhs c -> do
            putStrLn "The following clause has an inaccesible right hand side:"
            printClause n c
        NonExhaustive cs -> do
            putStrLn "The patterns may not be exhaustive, the following clauses are missing"
            mapM_ (printClause n) cs
    putStrLn ""
  where
    printClause :: Name -> Clause -> IO ()
    printClause n c = do
        putStr n
        putStr " "
        putStrLn $ pretty c

-- | Constructs ConditionedValueAbstractionSet without any conditions on each abstraction
withNoConstraints :: ValueAbstractionSet -> Binding -> ConditionedValueAbstractionSet
withNoConstraints vas gamma
    = map
        (\vector -> CVAV {valueAbstraction = vector
                         , delta = ConstraintSet {termConstraints = [], typeConstraints = []}
                         , gamma = gamma
                         })
        vas


analyzeFunction :: FunctionTarget -> Analyzer FunctionResult
analyzeFunction (FunctionTarget fun) = do
    freshVars <- replicateM (arity (head clauses)) freshVar
    let Right gamma = initialGamma fun freshVars
    let initialAbstraction = withNoConstraints [freshVars] gamma
    desugaredPatterns <- mapM desugarPatternVector patterns
    executionTrace <- iteratedVecProc desugaredPatterns initialAbstraction
    return $ trace (Pr.ppShow executionTrace) FunctionResult executionTrace
    where
        Function _ _ clauses = fun
        Right patterns = getPatternVectors fun


