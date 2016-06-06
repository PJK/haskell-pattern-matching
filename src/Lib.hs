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
import           Oracle.SBVQueries
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
            cvavs ->  [NonExhaustive $ map formatResult cvavs]

      where
        formatResult (SolvedValueAbstractionVector vec Nothing) = (makePrettyClause [] (Clause vec), "")
        formatResult (SolvedValueAbstractionVector vec (Just (bes, m)))
            = if null $ varsInVAV vec
              then (makePrettyClause []   $ Clause vec, "")
              else (makePrettyClause vars $ Clause vec, str)
          where
            varsInVAV :: ValueAbstractionVector -> [Name]
            varsInVAV = concatMap varsInPattern
              where
                varsInPattern :: Pattern -> [Name]
                varsInPattern (VariablePattern n) = [n]
                varsInPattern (LiteralPattern _ _) = []
                varsInPattern (ConstructorPattern _ ps) = concatMap varsInPattern ps
                varsInPattern (TuplePattern ps) = concatMap varsInPattern ps
                varsInPattern EmptyListPattern = []
                varsInPattern (InfixConstructorPattern p1 _ p2) = concatMap varsInPattern [p1, p2]
                varsInPattern WildcardPattern = []
                varsInPattern IntVariablePattern = []
                varsInPattern (GuardPattern p _) = varsInPattern p
            vars = concatMap varsInBE bes
            str = unlines ["Constraints:\n" ++ unlines (map (("  " ++) . pretty) bes), show $ SatResult $ Satisfiable z3 m]

    findRedundants :: SolvedExecutionTrace -> [RecommendationReason]
    findRedundants tr = mapMaybe checkRedundant $ zip tr clss
        where
            checkRedundant :: (SolvedClauseCoverage, Clause) -> Maybe RecommendationReason
            checkRedundant (cc, c)
                | null (scapC cc) && null (scapD cc) = Just $ Redundant $ makePrettyClause [] c
                | otherwise = Nothing

    findInaccessibleRhss :: SolvedExecutionTrace -> [RecommendationReason]
    findInaccessibleRhss tr = mapMaybe checkInaccessibleRhs $ zip tr clss
        where
            checkInaccessibleRhs :: (SolvedClauseCoverage, Clause) -> Maybe RecommendationReason
            checkInaccessibleRhs (cc, c)
                | null (scapC cc) && (not . null) (scapD cc) = Just $ InaccessibleRhs $ makePrettyClause [] c
                | otherwise = Nothing

    makePrettyClause :: [Name] -> Clause -> Clause
    makePrettyClause ns (Clause ps) = Clause $ map (makeVarsWildcards ns) ps

    makeVarsWildcards :: [Name] -> Pattern -> Pattern
    makeVarsWildcards ns (VariablePattern n) = if n `elem` ns then VariablePattern n else WildcardPattern
    makeVarsWildcards ns l@(LiteralPattern _ _) = l
    makeVarsWildcards ns (ConstructorPattern n ps) = ConstructorPattern n $ map (makeVarsWildcards ns) ps
    makeVarsWildcards ns (TuplePattern ps) = TuplePattern $ map (makeVarsWildcards ns) ps
    makeVarsWildcards ns EmptyListPattern = EmptyListPattern
    makeVarsWildcards ns WildcardPattern = WildcardPattern
    makeVarsWildcards ns (GuardPattern p e) = GuardPattern (makeVarsWildcards ns p) e
    makeVarsWildcards ns (InfixConstructorPattern p1 name p2) = InfixConstructorPattern (makeVarsWildcards ns p1) name (makeVarsWildcards ns p2)


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
            putStrLn "The patterns may not be exhaustive, the following clauses are missing:"
            forM_ cs $ \(c, s) -> do
                printClause n c
                putStr s
    putStrLn ""
  where
    printClause :: Name -> Clause -> IO ()
    printClause n c = do
        putStr n
        putStr " "
        prettyPrint c


gammaVAV :: Binding -> ConditionedValueAbstractionVectorq
gammaVAV gamma = CVAV { valueAbstraction = []
                        , delta = ConstraintSet {
                                 termConstraints = [],
                                 typeConstraints = []
                        }
                        , gamma = gamma
                        }


patAppend :: Pattern -> ConditionedValueAbstractionVector -> ConditionedValueAbstractionVector
patAppend p cvav = CVAV (p:valueAbstraction cvav) (gamma cvav) (delta cvav)


addIntegerBound :: Pattern -> Name -> Binding -> ConditionedValueAbstractionVector -> Int -> Analyzer ConditionedValueAbstractionVector
addIntegerBound vp name gamma subVAV bound = do
    falseVar <- freshVar
    let gamma' = Map.insert (varName falseVar) (TypeConstructor "Bool") gamma
    let boundsContraint
            = VarEqualsBool
                (varName falseVar)
                (BoolOp
                    BoolAnd
                    (IntBoolOp IntGE (IntVar name) (IntLit 0))
                    (IntBoolOp IntLT (IntVar name) (IntLit 256)))
    let d = delta subVAV
    let d' = addConstraint boundsContraint d
    let d'' = addConstraint (VarEqualsCons (varName falseVar) "True" []) d'
    return $ CVAV (vp:valueAbstraction subVAV) gamma' d''

extractTypingConstraints :: Binding -> ValueAbstractionVector -> Analyzer ConditionedValueAbstractionVector
extractTypingConstraints gamma []
    = return $ gammaVAV gamma

extractTypingConstraints gamma (vp@(VariablePattern name):vs) = do
    subVAV <- extractTypingConstraints gamma vs
    case Map.lookup name gamma of
        Just (TypeConstructor "Word8") -> addIntegerBound vp name gamma subVAV (2^8)
        Just (TypeConstructor "Word16") -> addIntegerBound vp name gamma subVAV (2^16)
        Just (TypeConstructor "Word32") -> addIntegerBound vp name gamma subVAV (2^32)
        _                              -> return $ patAppend vp subVAV
extractTypingConstraints gamma (v:vs) = do
    subVAV <- extractTypingConstraints gamma vs
    return $ patAppend v subVAV

-- | Constructs ConditionedValueAbstractionSet without any conditions on each abstraction, apart from
-- | those imposed by gamma
withNoConstraints :: ValueAbstractionSet -> Binding -> Analyzer ConditionedValueAbstractionSet
withNoConstraints vas gamma
    = mapM (extractTypingConstraints gamma) vas


analyzeFunction :: FunctionTarget -> Analyzer FunctionResult
analyzeFunction (FunctionTarget fun) = do
    freshVars <- {- trace (Pr.ppShow fun) $ -} replicateM (arity (head clauses)) freshVar
    let Right gamma = initialGamma fun freshVars
    initialAbstraction <- withNoConstraints [freshVars] gamma
    executionTrace <- {- trace (Pr.ppShow desugaredPatterns) $ -} iteratedVecProc desugaredPatterns initialAbstraction
    return $ FunctionResult executionTrace
    where
        Function _ _ clauses = fun
        Right patterns = getPatternVectors fun
        desugaredPatterns = map desugarPatternVector patterns

