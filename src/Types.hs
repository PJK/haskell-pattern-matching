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
    | AnalysisSuccess [FunctionResult] -- TODO replace with recommendations instead
    deriving (Show, Eq, Generic)

instance ToJSON AnalysisResult
instance FromJSON AnalysisResult

data AnalysisError
    = GatherError GatherError -- ^ Something went wrong while scraping functions
    | ProcessError AnalyzerError -- ^ Something went wrong during clause processing
    deriving (Show, Eq, Generic)

instance ToJSON AnalysisError
instance FromJSON AnalysisError

type GatherError = String

data FunctionTarget
    = FunctionTarget Function
    -- TODO add SrcLoc for printing later
    deriving (Show, Eq)

data FunctionResult
    = FunctionResult ExecutionTrace
    deriving (Show, Eq, Generic)

instance ToJSON   FunctionResult
instance FromJSON FunctionResult

type ExecutionTrace = [ClauseCoverage]


{-
We also have to pass the constraints set: Consider
f 1 = ... <- We cannot discard the substituted variable equality
f 2 = ...
-}
data ClauseCoverage = ClauseCoverage
    { capC :: ConditionedValueAbstractionSet
    , capU :: ConditionedValueAbstractionSet
    , capD :: ConditionedValueAbstractionSet
    } deriving (Show, Eq, Generic)


instance ToJSON   ClauseCoverage
instance FromJSON ClauseCoverage


type Analyzer = ExceptT AnalyzerError (StateT AnalyzerState (Reader AnalyzerContext))

data AnalyzerError
    = TypeNotFound String
    | ConstructorNotFound String -- TODO make these two better than strings
    | UnpredictedError String
    deriving (Show, Eq, Generic)

instance ToJSON   AnalyzerError
instance FromJSON AnalyzerError

data AnalyzerState
    = AnalyzerState
    { nextFreshVarName :: Int -- ^ To generate fresh variable names
    }

type AnalyzerContext = SimpleTypeMap
