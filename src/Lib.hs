module Lib where


import           ClauseProcessing
import           Control.Monad            (replicateM)
import           Control.Monad.Except     (runExceptT)
import           Control.Monad.Reader     (runReader)
import           Control.Monad.State      (evalStateT)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as LB
import           Data.Maybe               (mapMaybe)
import qualified Text.Show.Pretty as Pr
import           DataDefs
import           Debug.Trace
import           Language.Haskell.Exts    hiding (DataOrNew (..), Name (..),
                                           Pretty, Type (..), prettyPrint)
import           Language.Haskell.Exts    (fromParseResult, parseFile)
import qualified Data.Map                 as Map
import           Gatherer
import           OptParse
import           OptParse.Types
import           Types


patterns :: IO ()
patterns = do
    -- svbTest
    sets <- getSettings
    -- print sets
    res <- processTarget (setsTargetFile sets)
    print res
    LB.putStr $ encodePretty res

processTarget :: FilePath -> IO AnalysisResult
processTarget inputFile = do
    ast <- fromParseResult <$> parseFile inputFile
    -- print ast
    let ass = AnalysisAssigment inputFile ast
    processAssignment ass

    -- TODO name conflicts in variable patterns
processAssignment :: AnalysisAssigment -> IO AnalysisResult
processAssignment (AnalysisAssigment _ ast)
    = case (,) <$> getFunctions ast <*> getTypeUniverse ast of
        Left err -> return $ AnalysisError $ GatherError err
        Right (fs, ptcm) -> do
            let targets = map FunctionTarget fs
                initState = AnalyzerState 0
            LB.putStr $ encodePretty targets
            case flip runReader ptcm $ flip evalStateT initState $ runExceptT $ mapM analyzeFunction targets of
                Left err -> return $ AnalysisError $ ProcessError err
                Right res -> do
                    print targets
                    print res
                    return $ AnalysisSuccess $ concatMap (uncurry produceRecommendations) $ zip targets res

-- TODO actually solve the constraints, now I'm just ignoring them, which is like having an oracle that returns true.

-- TODO replace FunctionResult with (solved Conditioned)ValueAbstractionSet.
produceRecommendations :: FunctionTarget -> FunctionResult -> [Recommendation]
produceRecommendations t@(FunctionTarget (Function name _ clss)) (FunctionResult tr)
    =  map (Recommendation name)
    $  findNonExhaustives tr
    ++ findRedundants tr
    ++ findInaccessibleRhss tr
  where
    findNonExhaustives [] = []
    findNonExhaustives tr
        = case capU $ last tr of
            [] -> []
            cvavs ->  [NonExhaustive $ map (Clause . valueAbstraction) cvavs]
    findRedundants tr = mapMaybe checkRedundant $ zip tr clss
        where
            checkRedundant :: (ClauseCoverage, Clause) -> Maybe RecommendationReason
            checkRedundant (cc, c)
                | null (capC cc) && null (capD cc) = Just $ Redundant c
                | otherwise = Nothing
    findInaccessibleRhss tr = mapMaybe checkInaccessibleRhs $ zip tr clss
        where
            checkInaccessibleRhs :: (ClauseCoverage, Clause) -> Maybe RecommendationReason
            checkInaccessibleRhs (cc, c)
                | null (capC cc) && (not . null) (capD cc) = Just $ InaccessibleRhs c
                | otherwise = Nothing


-- | Constructs ConditionedValueAbstractionSet without any conditions on each abstraction
withNoConstraints :: ValueAbstractionSet -> ConditionedValueAbstractionSet
withNoConstraints
    = map
        (\vector -> CVAV {valueAbstraction = vector, delta = [], gamma = Map.fromList []})


analyzeFunction :: FunctionTarget -> Analyzer FunctionResult
analyzeFunction (FunctionTarget fun) = do
    freshVars <- replicateM (arity (head clauses)) freshVar
    let initialAbstraction = withNoConstraints [freshVars]
    executionTrace <- iteratedVecProc desugaredPatterns gammas initialAbstraction
    return $ trace (Pr.ppShow executionTrace) (FunctionResult executionTrace)
    where
        Function _ _ clauses = fun
        Right patterns = getPatternVectors fun
        desugaredPatterns = map desugarPatternVector patterns
        Right gammas = initialGammas fun
