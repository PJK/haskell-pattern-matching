module Types where

import           ClauseProcessing
import           DataDefs
import qualified Language.Haskell.Exts as H

data AnalysisAssigment
    = AnalysisAssigment
        FilePath
        H.Module
    deriving (Show, Eq)

data AnalysisResult
    = AnalysisError AnalysisError
    | AnalysisSuccess [ExecutionTrace] -- TODO replace with recommendations instead
    deriving (Show, Eq)

data AnalysisError
    = GatherError GatherError -- ^ Something went wrong while scraping functions
    deriving (Show, Eq)

type GatherError = String

data FunctionTarget
    = FunctionTarget Function
    -- TODO add SrcLoc for printing later
    deriving (Show, Eq)
