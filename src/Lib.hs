module Lib where


import           ClauseProcessing
import           Control.Monad            (replicateM)
import           Control.Monad.Except     (runExceptT)
import           Control.Monad.Reader     (runReader)
import           Control.Monad.State      (evalStateT)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as LB
import           Data.Maybe               (mapMaybe)
import           DataDefs
import           Debug.Trace
import           Language.Haskell.Exts    (fromParseResult, parseFile)

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
    res <- processAssignment ass
    return res

processAssignment :: AnalysisAssigment -> IO AnalysisResult
processAssignment (AnalysisAssigment _ ast)
    = case (,) <$> getFunctions ast <*> getPlainTypeConstructorsMap ast of
        Left err -> return $ AnalysisError $ GatherError err
        Right (fs, ptcm) -> let
                targets = map FunctionTarget fs
                initState = AnalyzerState 0
            in do
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
    = map (\vector -> CVAV {valueAbstraction = vector, delta = []})


analyzeFunction :: FunctionTarget -> Analyzer FunctionResult
analyzeFunction (FunctionTarget fun) = do
    freshVars <- replicateM (length (head patterns)) freshVar
    FunctionResult <$> iteratedVecProc desugaredPatterns (withNoConstraints [freshVars])
  where
    Right patterns = getTypedPatternVectors fun
    desugaredPatterns = map desugarPatternVector patterns
