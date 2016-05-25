{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Aeson            (FromJSON, ToJSON)
import           GHC.Generics          (Generic)

import           DataDefs
import qualified Language.Haskell.Exts as H

data AnalysisAssigment
    = AnalysisAssigment
        FilePath
        H.Module
    deriving (Show, Eq)

data AnalysisResult
    = AnalysisError AnalysisError
    | AnalysisSuccess [Recommendation] -- TODO add the evaluatedness of functions
    deriving (Show, Eq, Generic)

instance ToJSON   AnalysisResult
instance FromJSON AnalysisResult

data Recommendation
    = Recommendation
        Name -- ^ Function name
        RecommendationReason
    deriving (Show, Eq, Generic)

instance ToJSON   Recommendation
instance FromJSON Recommendation

data RecommendationReason
    = NonExhaustive [Clause] -- Missing clause, namely these
    | Redundant Clause -- Redundant Clause
    | InaccessibleRhs Clause -- Clause with inaccessible right-hand side
    deriving (Show, Eq, Generic)

instance ToJSON   RecommendationReason
instance FromJSON RecommendationReason

data AnalysisError
    = GatherError GatherError -- ^ Something went wrong while scraping functions
    | ProcessError AnalyzerError -- ^ Something went wrong during clause processing
    deriving (Show, Eq, Generic)

instance ToJSON   AnalysisError
instance FromJSON AnalysisError

type GatherError = String

data FunctionTarget
    = FunctionTarget Function
    -- TODO add SrcLoc for printing later
    deriving (Show, Eq, Generic)

instance ToJSON   FunctionTarget
instance FromJSON FunctionTarget

-- TODO rename to intermediate function result
data FunctionResult
    = FunctionResult ExecutionTrace
    deriving (Show, Eq, Generic)

instance ToJSON   FunctionResult
instance FromJSON FunctionResult

type ExecutionTrace = [ClauseCoverage]

-- TODO rename to Function result once the todo on line 65 is resolved
data SolvedFunctionResult
    = SolvedFunctionResult SolvedExecutionTrace
    deriving (Show, Eq, Generic)

instance ToJSON   SolvedFunctionResult
instance FromJSON SolvedFunctionResult

type SolvedExecutionTrace = [SolvedClauseCoverage]

data ClauseCoverage = ClauseCoverage
    { capC :: ConditionedValueAbstractionSet
    , capU :: ConditionedValueAbstractionSet
    , capD :: ConditionedValueAbstractionSet
    } deriving (Show, Eq, Generic)


instance ToJSON   ClauseCoverage
instance FromJSON ClauseCoverage

data SolvedClauseCoverage = SolvedClauseCoverage
    { scapC :: ValueAbstractionSet
    , scapU :: ValueAbstractionSet
    , scapD :: ValueAbstractionSet
    } deriving (Show, Eq, Generic)

instance ToJSON   SolvedClauseCoverage
instance FromJSON SolvedClauseCoverage


type Analyzer = ExceptT AnalyzerError (StateT AnalyzerState (Reader AnalyzerContext))

data AnalyzerError
    = TypeNotFound String
    | VariableNotBound String
    | ConstructorNotFound String -- TODO make these two better than strings
    | UnpredictedError String
    deriving (Show, Eq, Generic)

instance ToJSON   AnalyzerError
instance FromJSON AnalyzerError

data AnalyzerState
    = AnalyzerState
    { nextFreshVarName :: Int -- ^ To generate fresh variable names
    }

type AnalyzerContext = TypeUniverse


data OracleResult
    = DefinitelySatisfiable
    | DefinitelyUnsatisfiable
    | DontReallyKnow
