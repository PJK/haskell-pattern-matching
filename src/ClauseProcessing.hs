module ClauseProcessing where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import           DataDefs
import           Types
import           Util

-- Based on Figure 3 of 'GADTs meet their match'

--
-- Implements the 'C' helper function
--
coveredValues :: PatternVector -> ValueAbstractionVector -> Analyzer ValueAbstractionSet

-- CNil
coveredValues [] []
    = return [[]] -- Important: one empty set to start with

-- CConCon
coveredValues ((ConstructorPattern pname args, _):ps) (ConstructorPattern vname up:us)
        | pname == vname = do
            annArgs <- annotatePatterns args
            subs <- substitutePatterns up
            cvs <- coveredValues (annArgs ++ ps) (subs ++ us)
            return $ map (kcon (ConstructorPattern pname args)) cvs
        | otherwise      = return []

-- CConVarx
coveredValues (kk@(k@(ConstructorPattern _ _), _):ps) (VariablePattern _:us) = do
    -- Now this fresh variable substitution has no effect as there are are free
    -- variables in the pattern vector. Once we start using solver, we need separate
    -- names for them
    substituted <- substituteFreshParameters k
    coveredValues (kk:ps) (substituted:us)

-- CVar
coveredValues ((VariablePattern _, _):ps) (u:us) = do
    cvs <- coveredValues ps us
    return $ map (ucon u) cvs

-- TODO CGuard
coveredValues pat values
    = throwError
    $ UnpredictedError
    $ "coveredValues: unsupported pattern " ++ show pat ++ " with values " ++ show values

--
-- Implements the 'U' helper function
--
uncoveredValues :: PatternVector -> ValueAbstractionVector -> Analyzer ValueAbstractionSet

-- UNil
uncoveredValues [] []
    = return [] -- Important! This is different than coveredValues

-- UConCon
uncoveredValues ((k@(ConstructorPattern pname pParams), _):ps) (kv@(ConstructorPattern vname uParams):us)
    | pname == vname = do
        annArgs <- annotatePatterns pParams
        uvs <- uncoveredValues (annArgs ++ ps) (uParams ++ us)
        return $ map (kcon k) uvs
    | otherwise      = do
        substitute <- substituteFreshParameters kv
        return [substitute:us]

-- UConVar
uncoveredValues (p@(ConstructorPattern _ _, typeName):ps) (VariablePattern _:us)
    = do
    tmap <- ask
    -- TODO do this in the monad
    let allConstructors = fromMaybe
                            (error $ "Lookup for type " ++ typeName ++ " failed")
                            (Map.lookup typeName tmap)
    allConstructorsWithFreshParameters <- mapM substituteFreshParameters allConstructors

    uvs <- forM allConstructorsWithFreshParameters $ \constructor ->
        uncoveredValues (p:ps) (constructor:us)
    return $ concat uvs

-- UVar
uncoveredValues ((VariablePattern _, _):ps) (u:us)
    = do
    uvs <- uncoveredValues ps us
    return $ map (ucon u) uvs

-- TODO UGuard
uncoveredValues pat values
    = throwError
    $ UnpredictedError
    $ "uncoveredValues: unsupported pattern " ++ show pat ++ " with values " ++ show values



--
-- Implements the 'D' helper function
--
divergentValues :: PatternVector -> ValueAbstractionVector -> Analyzer ValueAbstractionSet

-- DNil
divergentValues [] []
    = return [] -- Important! This is different than coveredValues

-- DConCon
divergentValues ((k@(ConstructorPattern pname pParams), _):ps) (ConstructorPattern vname uParams:us)
    | pname == vname = do
        annPs <- annotatePatterns pParams
        dvs <- divergentValues (annPs ++ ps) (uParams ++ us)
        return $ map (kcon k) dvs
    | otherwise      = return []

-- DConVar
divergentValues (p@(pc@(ConstructorPattern _ _), _):ps) (VariablePattern _:us)
    = do
    subs <- substituteFreshParameters pc
    dvs <- divergentValues (p:ps) (subs:us)
    return $ (pc:us) : dvs

-- DVar
divergentValues ((VariablePattern _, _):ps) (u:us)
    = do
    dvs <- divergentValues ps us
    return $ map (ucon u) dvs

-- TODO DGuard
divergentValues pat values
    = throwError
    $ UnpredictedError
    $ "divergentValues: unsupported pattern " ++ show pat ++ " with values " ++ show values



-- | Refines the VA of viable inputs using the pattern vector
patVecProc :: PatternVector -> ValueAbstractionSet -> Analyzer ClauseCoverage
patVecProc ps s = do
    cvs <- concat <$> mapM (coveredValues ps) s
    uvs <- concat <$> mapM (uncoveredValues ps) s
    dvs <- concat <$> mapM (divergentValues ps) s
    return $ ClauseCoverage cvs uvs dvs


iteratedVecProc :: [PatternVector] -> ValueAbstractionSet -> Analyzer ExecutionTrace
iteratedVecProc [] _ = return []
iteratedVecProc (ps:pss) s = do
    res <- patVecProc ps s
    rest <- iteratedVecProc pss (capU res)
    return $ res : rest

-- |Coverage vector concatenation
-- TODO: Add the term constraints merging
ucon :: Pattern -> ValueAbstractionVector -> ValueAbstractionVector
ucon x xs = x:xs

-- Syd: I basicall want kcon :: ConstructorPattern -> ValueAbstractionVector -> ValueAbstractionVector
kcon :: Pattern -> ValueAbstractionVector -> ValueAbstractionVector
kcon (ConstructorPattern name parameters) ws =
        ConstructorPattern name (take arity ws):drop arity ws
    where
        arity = length parameters
kcon _ _ = error "Only constructor patterns"

-- TODO make this count new occurrences. (It is only relevant for the solver)
-- freshVars :: Int -> [Pattern]
-- freshVars 0 = []
-- freshVars k = VariablePattern ("__fresh" ++ show k):freshVars (k - 1)

freshVar :: Analyzer Pattern
freshVar = do
    i <- gets nextFreshVarName
    modify (\s -> s { nextFreshVarName = i + 1} )
    return $ VariablePattern $ show i

-- | Replace PlaceHolderPatterns with appropriate fresh variables
substituteFreshParameters :: Pattern -> Analyzer Pattern
substituteFreshParameters (ConstructorPattern name placeholders) = do
    subs <- substitutePatterns placeholders
    return $ ConstructorPattern name subs
-- TODO add lists and tuples
substituteFreshParameters _ = error "No substitution available"

-- TODO check these are PlaceHolderPatterns only
substitutePatterns :: [Pattern] -> Analyzer [Pattern]
substitutePatterns xs = replicateM (length xs) freshVar

annotatePatterns :: [Pattern] -> Analyzer PatternVector
annotatePatterns ps = do
    tmap <- ask
    let constructorToType = invertMap tmap
        fetchType constructor =
            fromMaybe
                (error $ "Lookup for type " ++ show constructor ++ " failed")
                (Map.lookup constructor constructorToType)
    return $ map (\p -> (p, fetchType p)) ps
