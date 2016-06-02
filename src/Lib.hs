module Lib where


import           ClauseProcessing
import           Control.Monad              (forM, forM_, replicateM)
import           Control.Monad.Except       (runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Reader       (ReaderT (..), runReader)
import           Control.Monad.State        (evalStateT)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LB8
import           Data.Either                (lefts, rights)
import qualified Data.Map                   as Map
import           Data.Maybe                 (mapMaybe)
import           Data.SBV
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
import           Util


patterns :: IO ()
patterns = do
    -- svbTest
    sets <- getSettings
    -- print sets
    res <- flip runReaderT sets $ processTarget (setsTargetFile sets)
    case setsCommand sets of
        Analyze -> prettyOutput res
        DumpResults -> LB8.putStrLn $ encodePretty res

type Configured = ReaderT Settings IO

processTarget :: FilePath -> Configured AnalysisResult
processTarget inputFile = do
    ast <- liftIO $ fromParseResult <$> parseFile inputFile
    -- print ast
    let ass = AnalysisAssigment inputFile ast
    processAssignment ass

processAssignment :: AnalysisAssigment -> Configured AnalysisResult
processAssignment (AnalysisAssigment _ ast)
    = case (,) <$> getFunctions ast <*> getTypeUniverse ast of
        Left err -> return $ AnalysisError [GatherError err]
        Right (fs, ptcm) -> do
            let targets = map FunctionTarget fs
                initState = AnalyzerState 0
            -- LB.putStr $ encodePretty targets
            ress <- forM targets $ \target -> do
                debug "Analysing target:"
                debugShow target
                case flip runReader ptcm
                        $ flip evalStateT initState
                        $ runExceptT
                        $ analyzeFunction target
                    of
                    Left err -> do
                        debug "Something went wrong while analyzing this target:"
                        debugShow err
                        return $ Left err
                    Right res -> do
                        debug "Trace gathered during analysis:"
                        debugShow res
                        -- Solve the constraints
                        sres <- liftIO $ runOracle res
                        debug "Result oracle consultation:"
                        debugShow sres
                        return $ Right $ produceRecommendations target sres
            case lefts ress of
                [] -> return $ AnalysisSuccess $ concat $ rights ress
                rs -> return $ AnalysisError $ map ProcessError rs

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
            cvavs ->  [NonExhaustive $ map (\scvav -> (makePrettyClause . Clause . svav $ scvav, showMModel $ mmodel scvav)) cvavs]

      where
        showMModel Nothing = ""
        showMModel (Just m) = show $ SatResult $ Satisfiable z3 m

    findRedundants :: SolvedExecutionTrace -> [RecommendationReason]
    findRedundants tr = mapMaybe checkRedundant $ zip tr clss
        where
            checkRedundant :: (SolvedClauseCoverage, Clause) -> Maybe RecommendationReason
            checkRedundant (cc, c)
                | null (scapC cc) && null (scapD cc) = Just $ Redundant $ makePrettyClause c
                | otherwise = Nothing

    findInaccessibleRhss :: SolvedExecutionTrace -> [RecommendationReason]
    findInaccessibleRhss tr = mapMaybe checkInaccessibleRhs $ zip tr clss
        where
            checkInaccessibleRhs :: (SolvedClauseCoverage, Clause) -> Maybe RecommendationReason
            checkInaccessibleRhs (cc, c)
                | null (scapC cc) && (not . null) (scapD cc) = Just $ InaccessibleRhs $ makePrettyClause c
                | otherwise = Nothing

    makePrettyClause :: Clause -> Clause
    makePrettyClause (Clause ps) = Clause $ map makeVarsWildcards ps

    makeVarsWildcards :: Pattern -> Pattern
    makeVarsWildcards (VariablePattern n) = WildcardPattern
    makeVarsWildcards l@(LiteralPattern _ _) = l
    makeVarsWildcards (ConstructorPattern n ps) = ConstructorPattern n $ map makeVarsWildcards ps
    makeVarsWildcards (TuplePattern ps) = TuplePattern $ map makeVarsWildcards ps
    makeVarsWildcards EmptyListPattern = EmptyListPattern
    makeVarsWildcards WildcardPattern = WildcardPattern
    makeVarsWildcards (GuardPattern p e) = GuardPattern (makeVarsWildcards p) e
    makeVarsWildcards (InfixConstructorPattern p1 name p2) = InfixConstructorPattern (makeVarsWildcards p1) name (makeVarsWildcards p2)


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
            forM_ cs $ \(c, s) -> do
                printClause n c
                putStrLn s
    putStrLn ""
  where
    printClause :: Name -> Clause -> IO ()
    printClause n c = do
        putStr n
        putStr " "
        prettyPrint c

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
    freshVars <- {- trace (Pr.ppShow fun) $ -} replicateM (arity (head clauses)) freshVar
    let Right gamma = initialGamma fun freshVars
    let initialAbstraction = withNoConstraints [freshVars] gamma
    executionTrace <- {- trace (Pr.ppShow desugaredPatterns) $ -} iteratedVecProc desugaredPatterns initialAbstraction
    return $ FunctionResult executionTrace
    where
        Function _ _ clauses = fun
        Right patterns = getPatternVectors fun
        desugaredPatterns = map desugarPatternVector patterns

