module Lib where


import           ClauseProcessing
import           Control.Monad            (replicateM)
import           Control.Monad.Except     (runExceptT)
import           Control.Monad.Reader     (runReader)
import           Control.Monad.State      (evalStateT)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as LB
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
    print sets
    res <- processTarget (setsTargetFile sets)
    print res
    LB.putStr $ encodePretty res

processTarget :: FilePath -> IO AnalysisResult
processTarget inputFile = do
    ast <- fromParseResult <$> parseFile inputFile
    let ass = AnalysisAssigment inputFile ast
        res = processAssignment ass
    return res

    -- TODO name conflicts in variable patterns

processAssignment :: AnalysisAssigment -> AnalysisResult
processAssignment (AnalysisAssigment _ ast)
    = case (,) <$> getFunctions ast <*> getPlainTypeConstructorsMap ast of
        Left err -> AnalysisError $ GatherError err
        Right (fs, ptcm) -> let
                targets = map FunctionTarget fs
                initState = AnalyzerState 0
            in case flip runReader ptcm $ flip evalStateT initState $ runExceptT $ mapM analyzeFunction targets of
                Left err -> AnalysisError $ ProcessError err
                Right res -> AnalysisSuccess res

-- | Constructs ConditionedValueAbstractionSet without any conditions on each abstraction
withNoConstraints :: ValueAbstractionSet -> ConditionedValueAbstractionSet
withNoConstraints
    = map
        (\vector -> CVAV {valueAbstraction = vector, delta = [], gamma = Map.fromList []})


analyzeFunction :: FunctionTarget -> Analyzer FunctionResult
analyzeFunction (FunctionTarget fun) = do
    freshVars <- replicateM (length (head patterns)) freshVar
    let initialAbstraction = withNoConstraints [freshVars]
    FunctionResult <$> iteratedVecProc desugaredPatterns gammas initialAbstraction
  where
    Right patterns = getTypedPatternVectors fun
    desugaredPatterns = map desugarPatternVector patterns
    Right gammas = initialGammas fun
